;;; code-spelunk --- Visualise your code exploration sessions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'map)
(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'project)

(defgroup spelunk nil
  "The customisation options for spelunk."
  :version 26.3
  :group 'tools)

(defcustom spelunk-show-history-behaviour 'show-until-next-command
  "What to do with the history window when it pops up."
  :group 'spelunk
  :type 'symbol
  :options '(show-until-next-command
             show-indefinitely
             show-until-seconds))

(defcustom spelunk-show-history-seconds 1
  "How long to show history.

Effective when `spelunk-show-history-behaviour' is set to
'show-until-seconds."
  :group 'spelunk
  :type 'number)

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
                (sub-nodes (slot-value current-node 'sub-nodes)))
            (cl-labels ((find-sub-node (sub-node) (eq (slot-value sub-node 'node-tag) key)))
              (if (some #'find-sub-node sub-nodes)
                  (cons root (find-if #'find-sub-node sub-nodes))
                (let* ((sub-node (make-instance 'spelunk-tree
                                                'node-tag key
                                                'sub-nodes '())))
                  (push sub-node (slot-value current-node 'sub-nodes))
                  (cons root sub-node)))))
        (cons root
              (spelunk--find-by-sub-node-identifier root
                                                    (slot-value current-node 'node-tag))))))))

(defun spelunk--close-window-by-buffer-name (buffer-name)
  "Close the window which is currently showing BUFFER-NAME."
  (cl-declare (type 'string buffer-name))
  (let ((original-window (selected-window)))
    (thread-last (cl-loop
                  for window being the windows
                  when (equal buffer-name (buffer-name (window-buffer window)))
                  collect window)
      (mapc (lambda (window) (progn
                               (select-window window)
                               (call-interactively #'quit-window)))))
    (select-window original-window)))

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
    (cl-case spelunk-show-history-behaviour
      (show-until-next-command
       (cl-labels ((close-spelunk-history (&rest _) (progn
                                                      (spelunk--close-window-by-buffer-name history-buffer-name)
                                                      (remove-hook 'post-command-hook #'close-spelunk-history))))
         (run-at-time 0.1 nil (lambda () (add-hook 'post-command-hook #'close-spelunk-history)))))
      (show-until-seconds
       (run-at-time spelunk-show-history-seconds
                    nil
                    (lambda ()
                      (spelunk--close-window-by-buffer-name history-buffer-name))))
      (show-indefinitely nil))
    (select-window original-window)))

(cl-defmethod spelunk--node-name ((tree spelunk-tree))
  "Produce a name for TREE which is suitable for printing in a tree."
  (symbol-name (slot-value tree 'node-tag)))

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
       for sub-node in (slot-value tree 'sub-nodes)
       when (eq (slot-value sub-node 'node-tag) identifier) return tree)
      (cl-loop
       for sub-node in (slot-value tree 'sub-nodes)
       for found = (spelunk--find-by-sub-node-identifier sub-node identifier)
       when found return found)))

(defun spelunk--other-if-zero (other x)
  "Produce OTHER if X is 0 else X."
  (if (eq x 0) other x))

(cl-deftype spelunk-tree-or-label ()
  "Either a spelunk tree or the name of a node in such a tree."
  '(or spelunk-tree string))

(defun spelunk--print-node (tree current-node)
  "Print TREE's label.

Print CURRENT-NODE in bold to indicate that it's current.

Node is aligned according to the width of all it's children."
  (cl-declare (type 'spelunk-tree-or-label tree)
              (type 'spelunk-tree current-node))
  (let* ((is-name (stringp tree))
         (node-name (or (and is-name tree)
                        (spelunk--node-name tree)))
         (name-length (length node-name))
         ;; Problem is that this padding is added per sub node...
         (max-width (+ 2 (if is-name
                             name-length
                           (spelunk--max-width tree))))
         (whitespace-padding (/ (- max-width name-length) 2))
         (left-padding (ceiling whitespace-padding))
         (right-padding (floor whitespace-padding)))
    (cl-loop for i from 0 below left-padding
             do (insert " "))
    (insert node-name)
    (when (eq tree current-node)
      (overlay-put (make-overlay (- (point) name-length) (point))
                   'face
                   'bold))
    (cl-loop for i from 0 below right-padding
             do (insert " "))))

(defun spelunk--generate-next-nodes (nodes)
  "Expand NODES to a flat list of their child nodes."
  ;; Problem with this declare for some reason.  It's not ensuring the type...
  (cl-declare (type 'spelunk-list-of-spelunk-tree nodes))
  (thread-last (mapcar (lambda (sub-node)
                         (if (stringp sub-node)
                             (list sub-node)
                           (let ((sub-nodes (slot-value sub-node 'sub-nodes)))
                             (if sub-nodes
                                 sub-nodes
                               (or (and (listp sub-node) sub-node)
                                   (list (replace-regexp-in-string "."
                                                                   " "
                                                                   (spelunk--node-name sub-node))))))))
                       nodes)
    (apply #'append)))

(cl-defmethod spelunk--print-tree ((tree spelunk-tree) current-node)
  "Print TREE vertically so that the start of your search is at the top.

CURRENT-NODE is printed in *bold* to indicate that it's the
current node."
  (cl-declare (type 'spelunk-tree current-node))
  (cl-labels
      ((iter (trees)
             (progn
               (dolist (tree trees)
                 (spelunk--print-node tree current-node))
               (insert "\n")
               (let ((next-round
                      (spelunk--generate-next-nodes trees)))
                 (when (and next-round
                            (not (cl-every #'stringp next-round)))
                   (iter next-round))))))
    (insert "\n")
    (iter (list tree))))

(cl-defmethod spelunk--max-width ((tree spelunk-tree))
  "Produce a count of all leaves in TREE.
This is used when printing a tree to determine how much space to
leave for printing children."
  (spelunk--other-if-zero (length (spelunk--node-name tree))
                          (cl-loop
                           for sub-node in (slot-value tree 'sub-nodes)
                           for current-width = (length (spelunk--node-name sub-node))
                           summing (max current-width (or (and (null (slot-value sub-node 'sub-nodes))
                                                               current-width)
                                                          (spelunk--max-width sub-node))))))

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
                     :sub-nodes (list
                                 (make-instance
                                  'spelunk-tree
                                  :node-tag 'test
                                  :sub-nodes (list
                                              (make-instance 'spelunk-tree
                                                             :node-tag 'blerg
                                                             :sub-nodes '()))))))
        (tree (make-instance
               'spelunk-tree
               :node-tag 'blah
               :sub-nodes (list
                           (make-instance
                            'spelunk-tree
                            :node-tag 'blah-blah
                            :sub-nodes (list
                                        (make-instance
                                         'spelunk-tree
                                         :node-tag 'another-one
                                         :sub-nodes (list
                                                     (make-instance
                                                      'spelunk-tree
                                                      :node-tag 'deeper-on-left
                                                      :sub-nodes (list
                                                                  (make-instance
                                                                   'spelunk-tree
                                                                   :node-tag 'even-deeper
                                                                   :sub-nodes (list
                                                                               (make-instance
                                                                                'spelunk-tree
                                                                                :node-tag 'deepest-yet
                                                                                :sub-nodes '())))))))
                                        (make-instance
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
