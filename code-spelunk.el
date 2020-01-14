;;; code-spelunk --- Visualise your code exploration sessions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'map)
(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'project)

(defun spelunk-list-of-spelunk-tree-p (x)
  "Produce t if X is a list of `spelunk-tree's."
  (cl-every #'spelunk-tree-p x))

(cl-deftype spelunk-list-of-spelunk-tree ()
  "A list of `spelunk-tree's."
  '(satisfies spelunk-list-of-spelunk-tree-p))

(defclass spelunk-tree ()
  ((node-tag  :initarg :node-tag
              :type symbol)
   (sub-nodes :initarg :sub-nodes
              :type spelunk-list-of-spelunk-tree))
  "A node the tree of code navigation actions taken in a spelunking session.")

(defun spelunk-history-record-p (x)
  "Produce t if X is a cons of two `spelunk-tree's.

The first is the root of the tree and the second is the current
node."
  (and (consp x)
       (spelunk-tree-p (car x))
       (spelunk-tree-p (cdr x))))

(cl-deftype spelunk-history-record ()
  "A tuple of the root of the history tree and a pointer to the current node."
  '(satisfies spelunk-history-record-p))

(defvar spelunk--trees-per-project (make-hash-table :test #'equal)
  "A record of the navigations made per project.")

;; TODO: needs to handle the case that xref didn't find a tag
;; (maybe?).
(defun spelunk--record-navigation-event (&optional identifier)
  "Append the navigation event which just occured to the current node.

The notion of a \"current-node\" exists because we go back up the
tree when you navigate back to where you came from.  This is
similar to how `undo-tree' works and is meant to help you keep
track of where you came from when you go down a deep rabbit hole.

Navigation trees are tracked per-project, so the first step is to
fetch the navigation record for the current project.

If we were navigating to the definition (i.e. using
`xref-find-definitions') then IDENTIFIER is the name of the
symbol which we're heading to.  If we're going back (i.e. using
`xref-pop-marker-stack'), then it'll be null."
  (cl-declare (type (or 'string 'null) identifier))
  (pcase (spelunk--retrieve-navigation-tree)
    (`(,root . ,current-node)
     (spelunk--update-navigation-tree
      (if identifier
          (let ((key       (intern identifier))
                (sub-nodes (oref current-node :sub-nodes)))
            (cl-labels ((find-sub-node (sub-node) (eq (oref sub-node :node-tag) key)))
              (if (some #'find-sub-node sub-nodes)
                  (cons root (find-if #'find-sub-node sub-nodes))
                (let* ((sub-node (make-instance 'spelunk-tree
                                                :node-tag key
                                                :sub-nodes '())))
                  (push sub-node (oref current-node :sub-nodes))
                  (cons root sub-node)))))
        (cons root
              (spelunk--find-by-sub-node-identifier root
                                                    (oref current-node :node-tag))))))))

(defun spelunk--close-window-by-buffer-name (buffer-name)
  "Close the window which is currently showing BUFFER-NAME."
  (let ((original-window (selected-window)))
    (cl-loop
     for window being the windows
     when (equal buffer-name (buffer-name (window-buffer window)))
     do (select-window window) (call-interactively #'quit-window))
    (select-window original-window)))

(defun spelunk-show-history (&rest _)
  "Show the history of code navigations in other window.

Window will exist for `spelunk--show-history-duration' before
dissapearing."
  (let ((history-buffer-name (spelunk--history-buffer-name))
        (original-window     (selected-window)))
    (with-current-buffer (switch-to-buffer-other-window (get-buffer-create history-buffer-name))
      (when spelunk-history-view-mode
        (spelunk-history-view-mode -1))
      (spelunk-history-view-mode 1))
    ;; Maybe trigger the close on the next command instead
    (run-at-time 1 nil (lambda () (spelunk--close-window-by-buffer-name history-buffer-name)))
    (select-window original-window)))

(cl-defmethod spelunk--node-name ((tree spelunk-tree))
  "Produce a name for TREE which is suitable for printing in a tree."
  (symbol-name (oref tree :node-tag)))

(define-minor-mode spelunk-history-view-mode
  "Sets up a buffer for viewing the history of code navigations."
  :init-value nil
  :lighter " Spelunk"
  :group 'spelunk
  (pcase (spelunk--retrieve-navigation-tree)
    (`(,root . ,current-node)
     (progn
       (read-only-mode -1)
       (delete-region (point-min) (point-max))
       (spelunk--print-tree root current-node)
       (read-only-mode 1)
       (goto-char (point-min))
       (search-forward (spelunk--node-name current-node))))))

(defun spelunk--history-buffer-name ()
  "Create a name for the buffer to show the code navigation.

Buffer name is unique per project."
  (let* ((candidate-projects (project-roots (project-current)))
         (existing-tree-key  (thread-last candidate-projects
                               ;; TODO: what if multiple trees match?
                               ;; Do I need to store the mode as well?
                               (car))))
    (format "*spelunk:%s*" existing-tree-key)))

(defun spelunk--start-recording ()
  "Start recording code navigation events on a per-project basis.

See: `spelunk--record-navigation-event'."
  (advice-add #'xref-find-definitions :before #'spelunk--record-navigation-event)
  (advice-add #'xref-find-definitions :after  #'spelunk-show-history)
  (advice-add #'xref-pop-marker-stack :before #'spelunk--record-navigation-event)
  (advice-add #'xref-pop-marker-stack :after  #'spelunk-show-history))

(defun spelunk--stop-recording ()
  "Remove advice which records code navigation events on a per-project basis."
  (advice-remove #'xref-find-definitions #'spelunk--record-navigation-event)
  (advice-remove #'xref-find-definitions #'spelunk-show-history)
  (advice-remove #'xref-pop-marker-stack #'spelunk--record-navigation-event)
  (advice-remove #'xref-pop-marker-stack #'spelunk-show-history))

(cl-defmethod spelunk--find-by-sub-node-identifier ((tree spelunk-tree) identifier)
  "Find the node in TREE which is identified by IDENTIFIER."
  (cl-declare (type 'symbol identifier))
  (or (cl-loop
       for sub-node in (oref tree :sub-nodes)
       when (eq (oref sub-node :node-tag) identifier) return tree)
      (cl-loop
       for sub-node in (oref tree :sub-nodes)
       for found = (spelunk--find-by-sub-node-identifier sub-node identifier)
       when found return found)))

(defun spelunk--one-if-zero (x)
  "If X is 0 then 1 else X."
  (if (eq x 0) 1 x))

(cl-defmethod spelunk--print-tree ((tree spelunk-tree) current-node)
  "Print TREE vertically so that the start of your search is at the top.

CURRENT-NODE is printed in *bold* to indicate that it's the
current node."
  (cl-labels
      ((iter (trees)
             (progn
               (dolist (tree trees)
                 (let* ((is-name (stringp tree))
                        (node-name (or (and is-name tree)
                                       (spelunk--node-name tree)))
                        (name-length (length node-name))
                        (width-of-children (if is-name
                                               name-length
                                             (apply #'+ (mapcar #'spelunk--max-width
                                                                (oref tree :sub-nodes)))))
                        (max-width (+ 2 (max name-length width-of-children)))
                        (whitespace-padding (/ (- max-width name-length) 2))
                        (left-padding (ceiling whitespace-padding))
                        (right-padding (floor whitespace-padding)))
                   (cl-loop for i from 0 below left-padding
                            do (insert " "))
                   (insert node-name)
                   (when (eq current-node tree)
                     (overlay-put (make-overlay (- (point) name-length) (point))
                                  'face
                                  'bold))
                   (cl-loop for i from 0 below right-padding
                            do (insert " "))))
               (insert "\n")
               (let ((next-round
                      (thread-last (mapcar (lambda (sub-node)
                                             (if (stringp sub-node)
                                                 (list sub-node)
                                               (let ((sub-nodes (oref sub-node :sub-nodes)))
                                                 (if sub-nodes
                                                     sub-nodes
                                                   (or (and (listp sub-node) sub-node)
                                                       (list (spelunk--node-name sub-node)))))))
                                           trees)
                        (apply #'append))))
                 (when (and next-round
                            (not (cl-every #'stringp next-round)))
                   (iter next-round))))))
    (insert "\n")
    (iter (list tree))))

(cl-defmethod spelunk--max-width ((tree spelunk-tree))
  "Produce a count of all leaves in TREE.
This is used when printing a tree to determine how much space to
leave for printing children."
  (spelunk--one-if-zero
   (cl-loop
    for sub-node in (oref tree :sub-nodes)
    summing (or (and (null (oref sub-node :sub-nodes))
                     (length (spelunk--node-name sub-node)))
                (spelunk--max-width sub-node)))))

(defun spelunk--update-navigation-tree (new-tree)
  "Set the current tree for this project to NEW-TREE."
  (cl-declare (type 'spelunk-history-record new-tree))
  (let ((existing-tree-key  (thread-last (project-roots (project-current))
                              (cl-remove-if-not (apply-partially #'map-contains-key
                                                                 spelunk--trees-per-project))
                              ;; TODO: what if multiple trees match?
                              ;; Do I need to store the mode as well?
                              (car))))
    (setf (gethash existing-tree-key spelunk--trees-per-project) new-tree)))

(defun spelunk--retrieve-navigation-tree ()
  "Find the navigation tree applicable for the current `default-directory'."
  (let* ((candidate-projects (project-roots (project-current)))
         (existing-tree-key  (thread-last candidate-projects
                               (cl-remove-if-not (apply-partially #'map-contains-key
                                                                  spelunk--trees-per-project))
                               ;; TODO: what if multiple trees match?
                               ;; Do I need to store the mode as well?
                               (car))))
    (if existing-tree-key
        (gethash existing-tree-key spelunk--trees-per-project)
      ;; TODO: assuming that it's the first project here.  See
      ;; previous note.
      (setf (map-elt spelunk--trees-per-project (car candidate-projects))
            (let ((tree (make-instance 'spelunk-tree
                                       :node-tag 'root
                                       :sub-nodes '())))
              (cons tree tree))))))



;; Example tree
'(let* ((right-tree (make-instance
                     'spelunk-tree
                     :node-tag 'teehee
                     :sub-nodes (list (make-instance
                                       'spelunk-tree
                                       :node-tag 'test
                                       :sub-nodes (list (make-instance 'spelunk-tree
                                                                       :node-tag 'blerg
                                                                       :sub-nodes '()))))))
        (tree (make-instance
               'spelunk-tree
               :node-tag 'blah
               :sub-nodes (list (make-instance
                                 'spelunk-tree
                                 :node-tag 'blah
                                 :sub-nodes (list (make-instance
                                                   'spelunk-tree
                                                   :node-tag 'haha
                                                   :sub-nodes '())
                                                  (make-instance
                                                   'spelunk-tree
                                                   :node-tag 'hehe
                                                   :sub-nodes '())))
                                right-tree))))
   (spelunk--print-tree tree right-tree))

(provide 'code-spelunk)
;;; code-spelunk ends here
