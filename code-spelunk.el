;;; code-spelunk --- Visualise your code exploration sessions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'map)
(require 'eieio)
(require 'cl-lib)
(require 'cl-macs)
(require 'subr-x)
(require 'project)
(require 'widget)

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

(defcustom spelunk-padding-width 1
  "Spaces to insert either side of a node label when printing."
  :group 'spelunk
  :type 'number)

(defcustom spelunk-transient-tree-duration 30
  "The amount of seconds to keep trees before starting a new tree."
  :group 'spelunk
  :type 'number)

(defun spelunk-list-of-spelunk-tree-p (x)
  "Produce t if X is a list of `spelunk-tree's."
  (cl-every #'spelunk-tree-p x))

(cl-deftype spelunk-list-of-spelunk-tree ()
  "A list of `spelunk-tree's."
  '(satisfies spelunk-list-of-spelunk-tree-p))

(cl-deftype spelunk-optional-location ()
  "Either a `spelunk-location' or NIL."
  '(or spelunk-location-p null))

(defclass spelunk-tree ()
  ((parent    :initarg :parent
              :type spelunk-tree)
   (node-tag  :initarg :node-tag
              :type symbol)
   (sub-nodes :initarg :sub-nodes
              :type spelunk-list-of-spelunk-tree)
   (location  :initarg :location
              :type spelunk-optional-location))
  "A node the tree of code navigation actions taken in a spelunking session.")

(defclass spelunk-location ()
  ((file-path :initarg :file-path
              :type string)
   (position  :initarg :position
              :type number))
  "A representation of a location in an arbitrary file.")

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

(defvar spelunk--tree-creation-times-per-project (make-hash-table :test #'equal)
  "A record of the time at which each navigation tree was created.

Tree's are keyed by the project for which they were created.")

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
  (let ((symbol (or (and identifier (thing-at-point 'symbol)) nil)))
    (pcase (spelunk--retrieve-navigation-tree)
      (`(,root . ,current-node)
       (spelunk--update-navigation-tree
        (if symbol
            (let ((key       (intern symbol))
                  (sub-nodes (slot-value current-node 'sub-nodes)))
              (cl-labels ((find-sub-node (sub-node) (eq (slot-value sub-node 'node-tag) key)))
                (if (cl-some #'find-sub-node sub-nodes)
                    (cons root (cl-find-if #'find-sub-node sub-nodes))
                  (let* ((sub-node (make-instance 'spelunk-tree
                                                  :parent current-node
                                                  :node-tag key
                                                  :sub-nodes '()
                                                  :location (make-instance 'spelunk-location
                                                                           :file-path (buffer-file-name)
                                                                           :position  (point)))))
                    (push sub-node (slot-value current-node 'sub-nodes))
                    (cons root sub-node)))))
          (cons root (slot-value current-node 'parent))))))))

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
       (remove-overlays)
       (read-only-mode -1)
       (delete-region (point-min) (point-max))
       (spelunk--print-tree root current-node)
       (read-only-mode 1)
       (goto-char (point-min))))))

(defvar spelunk--history-transient-map
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "M-n") #'spelunk-history-move-deeper)
    (define-key keymap (kbd "M-p") #'spelunk-history-move-shallower)
    (define-key keymap (kbd "M-f") #'spelunk-history-move-right)
    (define-key keymap (kbd "M-b") #'spelunk-history-move-left)
    keymap)
  "A transient setup for keybindings while the history is visible.")

(defun spelunk-history-move-deeper ()
  "Move one deeper in the history tree.

i.e. in the opposite direction of root.

Does not place the point into the history tree."
  (interactive)
  (progn
    (pcase (spelunk--retrieve-navigation-tree)
      (`(,root . ,current-node)
       (spelunk--update-navigation-tree
        (let ((new-current (or (car (slot-value current-node 'sub-nodes)) current-node)))
          (spelunk-goto-node-no-window-switch new-current)
          (cons root new-current)))))
    (set-transient-map spelunk--history-transient-map)
    (spelunk-show-history)))

(defun spelunk-history-move-shallower ()
  "Move one shallower in the history tree.

When there are many sub-nodes, then select the left most.

i.e. in the direction or root."
  (interactive)
  (progn
    (pcase (spelunk--retrieve-navigation-tree)
      (`(,root . ,current-node)
       (spelunk--update-navigation-tree
        (let ((new-current (slot-value current-node 'parent)))
          (spelunk-goto-node-no-window-switch new-current)
          (cons root new-current)))))
    (set-transient-map spelunk--history-transient-map)
    (spelunk-show-history)))

(defun spelunk-history-move-right ()
  "Move one sibling node to the right.

When there are no siblings then no movement occurs.

i.e. move to this nodes parent, and then back down, but one child
to the right of the current node."
  (interactive)
  (progn
    (pcase (spelunk--retrieve-navigation-tree)
      (`(,root . ,current-node)
       (spelunk--update-navigation-tree
        (let ((new-current (spelunk--right-of current-node)))
          (spelunk-goto-node-no-window-switch new-current)
          (cons root new-current)))))
    (set-transient-map spelunk--history-transient-map)
    (spelunk-show-history)))

(defun spelunk--right-of (tree)
  "Produce the sibling node of TREE to the right.

When there's no sibling nodes to the right of this node then
produce this node."
  (cl-declare (type tree 'spelunk-tree))
  (let* ((parent   (slot-value tree   'parent))
         (children (slot-value parent 'sub-nodes)))
    (or (cl-loop with previous-node
                 for node in children
                 when (eq previous-node tree) return node
                 do (setq previous-node node))
        tree)))

(defun spelunk-history-move-left ()
  "Move one sibling node to the left.

When there are no siblings then no movement occurs.

i.e. move to this nodes parent, and then back down, but one child
to the left of the current node."
  (interactive)
  (progn
    (pcase (spelunk--retrieve-navigation-tree)
      (`(,root . ,current-node)
       (spelunk--update-navigation-tree
        (let ((new-current (spelunk--left-of current-node)))
          (spelunk-goto-node-no-window-switch new-current)
          (cons root new-current)))))
    (set-transient-map spelunk--history-transient-map)
    (spelunk-show-history)))

(defun spelunk--left-of (tree)
  "Produce the sibling node of TREE to the left.

When there's no sibling nodes to the left of this node then
produce this node."
  (cl-declare (type tree 'spelunk-tree))
  (let* ((parent   (slot-value tree   'parent))
         (children (slot-value parent 'sub-nodes)))
    (or (cl-loop with previous-node
                 for node in children
                 when (and previous-node (eq node tree)) return previous-node
                 do (setq previous-node node))
        tree)))

(defun spelunk-show-history (&rest _)
  "Show the history of code navigations in other window.

Window will exist for `spelunk--show-history-duration' before
dissapearing."
  (let ((history-buffer-name (spelunk--history-buffer-name))
        (original-window     (selected-window)))
    (set-transient-map spelunk--history-transient-map)
    (with-current-buffer (switch-to-buffer-other-window (get-buffer-create history-buffer-name))
      (when spelunk-history-view-mode
        (spelunk-history-view-mode -1))
      (spelunk-history-view-mode 1))
    (cl-case spelunk-show-history-behaviour
      (show-until-next-command
       (cl-labels ((close-spelunk-history (&rest _) (when (not (or (string-equal (buffer-name (current-buffer))
                                                                                 history-buffer-name)
                                                                   (memq last-command '(xref-find-definitions
                                                                                        xref-pop-marker-stack
                                                                                        widget-button-click
                                                                                        widget-button-press
                                                                                        mouse-drag-region
                                                                                        spelunk-history-move-deeper
                                                                                        spelunk-history-move-shallower
                                                                                        spelunk-history-move-right
                                                                                        spelunk-history-move-left))))
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
  (format "*spelunk:%s*" (car (spelunk--current-project-tree-key))))

(define-minor-mode spelunk-transient-tracking-mode
  "Have `code-spelunk' automatically start and stop recording transiently.

`code-spelunk' will record the time of your last interaction with
either `xref-find-definition' and `xref-pop-marker-stack' or your
last interaction with tree navigation
commands (`spelunk-history-move-left' etc.).  If your last
invocation of one of those commands was longer than
`spelunk-transient-tree-duration' then it will start a new
tree.

The timeout is managed on a per project basis."
  :init-value nil
  :lighter nil
  :group 'spelunk
  (if spelunk-transient-tracking-mode
      (spelunk--start-recording)
    (spelunk--stop-recording)))

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
         (max-width (if is-name
                        name-length
                      (spelunk--max-width tree spelunk-padding-width)))
         (whitespace-padding (/ (- max-width name-length) 2))
         (left-padding (ceiling whitespace-padding))
         (right-padding (floor whitespace-padding)))
    (cl-loop for i from 0 below left-padding
             do (insert " "))
    (widget-create 'link
                   :button-face 'default
                   :button-prefix ""
                   :button-suffix ""
                   :action (spelunk-goto-node tree)
                   node-name)
    (when (eq tree current-node)
      (overlay-put (make-overlay (- (point) name-length) (point))
                   'face
                   'bold))
    (cl-loop for i from 0 below right-padding
             do (insert " "))))

(defun spelunk-goto-node (node)
  "Produce a function which will go to the location which created NODE."
  (cl-declare (type 'spelunk-tree node))
  (lambda (&rest _) (if (slot-boundp node 'location)
                        (let ((location (slot-value node 'location)))
                          (find-file-other-window (slot-value location 'file-path))
                          (goto-char (slot-value location 'position))
                          (other-window 1))
                      (message "No location for: %s" (spelunk--node-name node)))))

(defun spelunk-goto-node-no-window-switch (node)
  "Go to the location which created NODE."
  (cl-declare (type 'spelunk-tree node))
  (if (slot-boundp node 'location)
      (let ((location (slot-value node 'location)))
        (find-file (slot-value location 'file-path))
        (goto-char (slot-value location 'position)))
    (message "No location for: %s" (spelunk--node-name node))))

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
                                   (list (cl-coerce (make-vector
                                                     (+ (* spelunk-padding-width 2)
                                                        (length (spelunk--node-name sub-node)))
                                                     ?\ )
                                                    'string)))))))
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

(cl-defmethod spelunk--max-width ((tree spelunk-tree) &optional (padding 0))
  "Produce a count of all leaves in TREE.
This is used when printing a tree to determine how much space to
leave for printing children.

Add PADDING for each label involved in the maximum length of
subtrees."
  (max (+ (* 2 padding) (length (spelunk--node-name tree)))
       (cl-loop
        for sub-node in (slot-value tree 'sub-nodes)
        for current-width = (length (spelunk--node-name sub-node))
        summing (+ (* 2 padding) (or (and (null (slot-value sub-node 'sub-nodes))
                                          current-width)
                                     (spelunk--max-width sub-node))))))

(defun spelunk--update-navigation-tree (new-tree)
  "Set the current tree for this project to NEW-TREE."
  (cl-declare (type 'spelunk-history-record new-tree))
  (setf (map-elt spelunk--trees-per-project (car (spelunk--current-project-tree-key))) new-tree))

(defun spelunk--retrieve-navigation-tree-creation-time (&optional tree-key)
  "Produce the time at which the project identified by TREE-KEY was created."
  (cl-declare (type 'string tree-key))
  (if tree-key (map-elt spelunk--tree-creation-times-per-project tree-key)
    (map-elt spelunk--tree-creation-times-per-project (car (spelunk--current-project-tree-key)))))

(defun spelunk--current-project-tree-key ()
  "Produce the key for the current project's tree.

Produce a new key as an additional value."
  (let* ((candidate-projects (project-roots (project-current)))
         ;; TODO: assuming that it's the first project here.
         (new-tree-key       (car candidate-projects))
         (existing-tree-key  (thread-last candidate-projects
                               (cl-remove-if-not (apply-partially #'map-contains-key
                                                                  spelunk--trees-per-project))
                               ;; TODO: what if multiple trees match?
                               ;; Do I need to store the mode as well?
                               (car))))
    (cl-values existing-tree-key new-tree-key)))

(defun spelunk--retrieve-navigation-tree ()
  "Find the navigation tree applicable for the current `default-directory'."
  (cl-multiple-value-bind (existing-tree-key new-tree-key) (spelunk--current-project-tree-key)
    (if (and existing-tree-key (< (float-time (time-subtract (current-time)
                                                             (spelunk--retrieve-navigation-tree-creation-time
                                                              existing-tree-key)))
                                  spelunk-transient-tree-duration))
        (progn
          (setf (map-elt spelunk--tree-creation-times-per-project new-tree-key) (current-time))
          (map-elt spelunk--trees-per-project existing-tree-key))
      (setf (map-elt spelunk--tree-creation-times-per-project new-tree-key) (current-time)
            (map-elt spelunk--trees-per-project new-tree-key) (let ((tree (make-instance 'spelunk-tree
                                                                                         :node-tag 'root
                                                                                         :sub-nodes '())))
                                                                (setf (slot-value tree 'parent) tree)
                                                                (cons tree tree))))))



;; Example tree
'(let* ((right-sub-2 (make-instance 'spelunk-tree
                                    :node-tag 'blerg
                                    :sub-nodes '()))
        (right-sub-1 (make-instance 'spelunk-tree
                                    :node-tag 'test
                                    :sub-nodes '()))
        (right-tree (make-instance 'spelunk-tree
                                   :node-tag 'teehee
                                   :sub-nodes '()))
        (left-sub-1-1-4 (make-instance 'spelunk-tree
                                       :node-tag 'deepest-yet
                                       :sub-nodes '()))
        (left-sub-1-1-3 (make-instance 'spelunk-tree
                                       :node-tag 'even-deeper
                                       :sub-nodes '()))
        (left-sub-1-1-2 (make-instance 'spelunk-tree
                                       :node-tag 'deeper-on-left
                                       :sub-nodes '()))
        (left-sub-1-1 (make-instance 'spelunk-tree
                                     :node-tag 'another-one
                                     :sub-nodes '()))
        (left-sub-1-2 (make-instance 'spelunk-tree
                                     :node-tag 'haha
                                     :sub-nodes '()))
        (left-sub-1-3 (make-instance 'spelunk-tree
                                     :node-tag 'hehe
                                     :sub-nodes '()))
        (left-tree (make-instance
                    'spelunk-tree
                    :node-tag 'blah-blah
                    :sub-nodes '()))
        (tree (make-instance
               'spelunk-tree
               :node-tag 'blah
               :sub-nodes '())))
   (setf (slot-value right-sub-1 'parent) right-tree)
   (setf (slot-value right-tree 'sub-nodes) (list right-sub-1))

   (setf (slot-value right-sub-2 'parent) right-sub-1)
   (setf (slot-value right-sub-1 'sub-nodes) (list right-sub-2))
   
   (setf (slot-value left-sub-1-1 'parent) left-tree)
   (push left-sub-1-1 (slot-value left-tree 'sub-nodes))
   
   (setf (slot-value left-sub-1-2 'parent) left-tree)
   (push left-sub-1-2 (slot-value left-tree 'sub-nodes))
   
   (setf (slot-value left-sub-1-3 'parent) left-tree)
   (push left-sub-1-3 (slot-value left-tree 'sub-nodes))
   
   (setf (slot-value left-sub-1-1-2 'parent) left-sub-1-1)
   (setf (slot-value left-sub-1-1 'sub-nodes) (list left-sub-1-1-2))
   
   (setf (slot-value left-sub-1-1-3 'parent) left-sub-1-1-2)
   (setf (slot-value left-sub-1-1-2 'sub-nodes) (list left-sub-1-1-3))

   (setf (slot-value tree 'parent) tree)
   (setf (slot-value tree 'sub-nodes) (list left-tree right-tree))

   (spelunk--print-tree tree right-tree))

(provide 'code-spelunk)
;;; code-spelunk ends here
