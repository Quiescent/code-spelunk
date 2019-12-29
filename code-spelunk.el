;;; code-spelunk --- Visualise your code exploration sessions -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'map)

(defvar spelunk--trees-per-project (make-hash-table :test #'equal)
  "A record of the navigations made per project.")

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
  (declaim (type (or 'string 'null) identifier))
  (pcase (spelunk--retrieve-navigation-tree)
    (`(,root . ,current-node)
     (spelunk--update-navigation-tree
      (if identifier
          (let ((key (intern identifier)))
            (if (map-contains-key current-node key)
                (cons root (map-elt current-node key))
              (let* ((sub-node '(nil))
                     (new-node (cons key sub-node)))
                (setcdr current-node (cons new-node (cdr current-node)))
                (cons root sub-node))))
        (let ((parent-node (spelunk--find-identifier (car current-node) root)))
          (cons root parent-node)))))))

(defun spelunk--start-recording ()
  "Start recording code navigation events on a per-project basis.

See: `spelunk--record-navigation-event'."
  (advice-add #'xref-find-definitions :before #'spelunk--record-navigation-event)
  (advice-add #'xref-pop-marker-stack :before #'spelunk--record-navigation-event))

(defun spelunk--stop-recording ()
  "Remove advice which records code navigation events on a per-project basis."
  (advice-remove #'xref-find-definitions #'spelunk--record-navigation-event)
  (advice-remove #'xref-pop-marker-stack #'spelunk--record-navigation-event))

(defun spelunk--find-identifier (identifier tree)
  "Find the node which is identified by IDENTIFIER in TREE."
  (declaim (type 'symbol identifier))
  (or (and (map-contains-key tree identifier) (map-elt tree identifier))
      (find-if (apply-partially #'spelunk--find-identifier identifier)
               (mapcar #'cdr tree))))

;; Example tree
(let ((tree (list (cons (intern "blah") (list (cons (intern"haha") '())
                                              (cons (intern "hehe") '())))
                  (cons (intern "test") (list (cons (intern "blerg") '()))))))
  (spelunk--find-identifier (intern "blerg") (car (cons tree tree))))

(defun spelunk--update-navigation-tree (new-tree)
  "Set the current tree for this project to NEW-TREE."
  (declaim (type 'cons new-tree))
  (let ((existing-tree-key  (thread-last (project-roots (project-current))
                              (remove-if-not (apply-partially #'map-contains-key
                                                              spelunk--trees-per-project))
                              ;; TODO: what if multiple trees match?
                              ;; Do I need to store the mode as well?
                              (car))))
    (setf (gethash existing-tree-key spelunk--trees-per-project)
          new-tree)))

(defun spelunk--retrieve-navigation-tree ()
  "Find the navigation tree applicable for the current `default-directory'."
  (let* ((candidate-projects (project-roots (project-current)))
         (existing-tree-key  (thread-last candidate-projects
                               (remove-if-not (apply-partially #'map-contains-key
                                                               spelunk--trees-per-project))
                               ;; TODO: what if multiple trees match?
                               ;; Do I need to store the mode as well?
                               (car))))
    (if existing-tree-key
        (gethash existing-tree-key spelunk--trees-per-project)
      ;; TODO: assuming that it's the first project here.  See
      ;; previous note.
      (setf (map-elt spelunk--trees-per-project (car candidate-projects))
            (let ((tree '(nil)))
              (cons tree tree))))))

(provide 'code-spelunk)
;;; code-spelunk ends here
