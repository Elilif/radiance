;;; radiance.el -*- lexical-binding: t; -*-

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/radiance

;; Version: 0.1
;; Package-Requires: ((emacs "28.2"))
;; Keywords: multiple-cursor, editing
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Combine kmacro and overlays to make multiple-cursor editing more fluent.

;;; Code:

(require 'rect)
(require 'cl-lib)

;;;; customizations

(defgroup radiance nil
  "Operations on target object will affect all same objects."
  :group 'radiance)

(defface radiance-mark-face-1
  '((t (:background "pink")))
  "Face for marked objects."
  :group 'radiance)

(defface radiance-mark-face-2
  '((t (:background "orange")))
  "Face for marked objects."
  :group 'radiance)

(defface radiance-mark-face-3
  '((t (:background "tan")))
  "Face for marked objects."
  :group 'radiance)

(defface radiance-mark-face-4
  '((t (:background "yellow green")))
  "Face for marked objects."
  :group 'radiance)

(defface radiance-mark-face-5
  '((t (:background "purple")))
  "Face for marked objects."
  :group 'radiance)

(defface radiance-region-face
  '((t (:background "lightblue")))
  "Face for marked regions."
  :group 'radiance)

(defcustom radiance-mark-faces '(radiance-mark-face-1
                                 radiance-mark-face-2
                                 radiance-mark-face-3
                                 radiance-mark-face-4
                                 radiance-mark-face-5)
  "Faces for marked objects."
  :type '(repeat face)
  :group 'radiance)

;;;; utilities

(defvar-local radiance-overlays-alist nil)
(defvar-local radiance-regions nil)
(defvar-local radiance-current-overlay nil)

(defvar radiance-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "s-\\" #'radiance-start)
    (keymap-set map "s-s" #'radiance-swap-object)
    (keymap-set map "s-x" #'radiance-exit)
    (keymap-set map "s-n" #'radiance-next-object)
    (keymap-set map "s-p" #'radiance-previous-object)
    (keymap-set map "s-m" #'radiance-unmark)
    map)
  "Keymap automatically activated inside overlays.
You can re-bind the commands to any keys you prefer.")

(defun radiance--get-current-mark-ov ()
  "Return the first radiance overlay under point."
  (cl-find-if
   (lambda (ov)
     (memq (overlay-get ov 'face)
           radiance-mark-faces))
   (overlays-in (1- (point)) (1+ (point)))
   :from-end t))

(defun radiance--get-all-ovs ()
  (let* ((current-overlay (radiance--get-current-mark-ov))
         (face (overlay-get current-overlay 'face)))
    (assoc face radiance-overlays-alist)))

(defun radiance--set-current-ov ()
  (let ((ov-at-point (radiance--get-current-mark-ov))
        (last-ov (car (last (cddr (car (last radiance-overlays-alist)))))))
    (setq radiance-current-overlay (or ov-at-point last-ov))))

(defun radiance--collect-in-region (beg end reg)
  "Highlight all matching objects for REG between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((face (or (car (cl-find-if (lambda (cand)
                                       (string= (cadr cand) reg))
                                     radiance-overlays-alist))
                    (car (cl-set-difference
                          radiance-mark-faces
                          (mapcar #'car radiance-overlays-alist)))))
          radiance-overlays)
      (while (and (not (eobp))
                  (re-search-forward reg end t)
                  (not (radiance--get-current-mark-ov)))
        (if-let* ((mbeg (match-beginning 0))
                  (mend (match-end 0))
                  (not-empty? (not (eq mbeg mend)))
                  (ov (make-overlay mbeg mend nil nil t)))
            (progn
              (overlay-put ov 'face face)
              (overlay-put ov 'priority 1)
              (overlay-put ov 'keymap radiance-map)
              (push ov radiance-overlays))
          (when (< (point) end)
            (forward-char 1))))
      (when (setq radiance-overlays (reverse radiance-overlays))
        (if-let (cand (alist-get face radiance-overlays-alist))
            (setf (cddr cand) (append (cddr cand) radiance-overlays))
          (add-to-list
           'radiance-overlays-alist
           (cons face (cons reg radiance-overlays))
           t))))))

(defun radiance-collect (reg)
  "Highlight all matching parts for REG."
  (cond
   ((and radiance-regions
         (cl-find-if #'overlay-buffer radiance-regions))
    (dolist (region radiance-regions)
      (let ((beg (overlay-start region))
            (end (overlay-end region)))
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (radiance--collect-in-region beg end reg))))))
   (t
    (radiance--collect-in-region (point-min) (point-max) reg))))

(defmacro radiance--perform (&rest body)
  (declare (indent defun))
  `(progn
     (when (region-active-p) (deactivate-mark))
     (when defining-kbd-macro
       (end-kbd-macro))
     ,@body
     (radiance--set-current-ov)
     (let ((length (length (cdr (radiance--symbol-position)))))
       (message "There are %s overlays." length))))

(defun radiance-delete-overlays (arg)
  "With ARG, delete radiance marked overlays and `radiance-regions',
delete radiance marked overlays only otherwise."
  (when radiance-overlays-alist
    (dolist (radiance-overlays radiance-overlays-alist)
      (dolist (overlay (cddr radiance-overlays))
        (delete-overlay overlay)))
    (setq radiance-overlays-alist nil))
  (when (and arg radiance-regions)
    (dolist (overlay radiance-regions)
      (delete-overlay overlay))
    (setq radiance-regions nil)))

(defun radiance--symbol-position ()
  (let* ((ovs (cddr (radiance--get-all-ovs))))
    (cons (seq-position ovs (radiance--get-current-mark-ov)) ovs)))

;;;; interactive functions

;;;###autoload
(defun radiance-mark-strings (string)
  "Mark all the same strings.

When the region is active, use the text in the region. Otherwise,
use the word at point."
  (interactive (list (cond
                      ((use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end)))
                      (t (or (thing-at-point 'word)
                             (error "No word at point!"))))))
  (radiance--perform
    (radiance-collect (regexp-quote string))))

;;;###autoload
(defun radiance-mark-symbols (symbol)
  "Mark all symbols the same as the one under the current cursor.

If there are marked regions, mark all symbols in these regions
only."
  (interactive (list (or (thing-at-point 'symbol)
                         (error "No symbol at point!"))))
  (radiance--perform
    (radiance-collect (format "\\_<%s\\_>" (regexp-quote symbol)))))

;;;###autoload
(defun radiance-mark-lines ()
  "Mark all lines.

If there are marked regions, mark all lines in these regions
only."
  (interactive)
  (radiance--perform
    (radiance-collect "^.*$")))

;;;###autoload
(defun radiance-mark-region (beg end)
  "Mark the region between BEG and END.

This command is applicable to both normal regions and
`rectangle-mark-mode'. You can also mark multiple region at once."
  (interactive "r")
  (if (cl-find-if (lambda (ov)
                    (eq (overlay-get ov 'face)
                        'radiance-region-face))
                  (overlays-in (1- beg) (1+ end)))
      (error "Regions overlapped!")
    (let ((ov-adder (lambda (beg end)
                      (let ((ov (make-overlay beg end nil nil t)))
                        (overlay-put ov 'face 'radiance-region-face)
                        (add-to-list 'radiance-regions ov t)))))
      (cond
       ((bound-and-true-p rectangle-mark-mode)
        (let ((bounds (extract-rectangle-bounds beg end)))
          (dolist (bound bounds)
            (funcall ov-adder (car bound) (cdr bound)))))
       (t
        (funcall ov-adder beg end)))
      (deactivate-mark))))

;;;###autoload
(defun radiance-start ()
  "Record subsequent keyboard input, defining a keyboard macro."
  (interactive)
  (if (not radiance-overlays-alist)
      (message "Nothing is selected.")
    (let ((current-overlay (radiance--get-current-mark-ov)))
      (goto-char (overlay-start (or current-overlay
                                    radiance-current-overlay)))
      (radiance-macro-mode 1)
      (kmacro-start-macro 0))))

;;;###autoload
(defun radiance-exit (&optional arg)
  "Quit out of recording the macro or delete overlays.

With ARG, delete `radiance-overlays' and `radiance-regions',
delete `radiance-overlays' only otherwise."
  (interactive "P")
  (radiance-delete-overlays arg)
  (radiance-macro-mode -1)

  ;; taken from `keyboard-quit'.
  (setq saved-region-selection nil)
  (let (select-active-regions)
    (deactivate-mark))
  (if (fboundp 'kmacro-keyboard-quit)
      (kmacro-keyboard-quit))
  (if defining-kbd-macro
      (force-mode-line-update t))
  (setq defining-kbd-macro nil))

;;;###autoload
(defun radiance-finish (arg)
  "Finish defining a keyboard macro and apply the macro to all overlays."
  (interactive "P")
  (end-kbd-macro)
  (save-excursion
    (if arg
        (let ((current-overlay (radiance--get-current-mark-ov)))
          (dolist (radiance-overlays radiance-overlays-alist)
            (dolist (ov (cddr radiance-overlays))
              (unless (eq ov current-overlay)
                (goto-char (overlay-start ov))
                (call-last-kbd-macro)))))
      (let* ((ovs (cddr (radiance--get-all-ovs))))
        (dolist (ov ovs)
          (unless (eq ov current-overlay)
            (goto-char (overlay-start ov))
            (call-last-kbd-macro))))))
  (radiance-macro-mode -1))

;;;###autoload
(defun radiance-end-of-line ()
  "Similar to `end-of-line' but respect the radiance region."
  (interactive)
  (let ((ov (radiance--get-current-mark-ov)))
    (goto-char (overlay-end ov))))

;;;###autoload
(defun radiance-beginning-of-line ()
  "Similar to `beginning-of-line' but respect the radiance retion."
  (interactive)
  (let ((ov (radiance--get-current-mark-ov)))
    (goto-char (overlay-start ov))))

;;;###autoload
(define-minor-mode radiance-macro-mode
  "Minor mode for radiance kmacro."
  :keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "s-`" #'radiance-finish)
    (keymap-set map "C-g" #'radiance-exit)
    (keymap-set map "C-a" #'radiance-beginning-of-line)
    (keymap-set map "C-e" #'radiance-end-of-line)
    map))

;;;###autoload
(defun radiance-swap-object ()
  "Swap objects."
  (interactive)
  (unless (length= radiance-overlays-alist 2)
    (user-error "Can only swap two symbols!"))
  (save-excursion
    (dotimes (i 2)
      (let ((cand (nth i radiance-overlays-alist))
            (new (string-trim
                  (cadr (nth (- 1 i) radiance-overlays-alist))
                  "\\\\_<" "\\\\_>")))
        (dolist (ov (cddr cand))
          (goto-char (overlay-start ov))
          (when (re-search-forward (cadr cand) (overlay-end ov) t)
            (replace-match new)))))
    (cl-rotatef (cadr (nth 0 radiance-overlays-alist))
                (cadr (nth 1 radiance-overlays-alist)))))

;;;###autoload
(defun radiance-unmark ()
  "Remove all highlights of symbol at point."
  (interactive)
  (let* ((cand (radiance--get-all-ovs))
         (face (car cand))
         (ovs (cddr cand)))
    (dolist (ov ovs)
      (delete-overlay ov))
    (setf (alist-get face radiance-overlays-alist nil t) nil)))

;;;###autoload
(defun radiance-next-object ()
  "Jump to the next location of symbol at point."
  (interactive)
  (let* ((result (radiance--symbol-position))
         (pos (car result))
         (ovs (cdr result))
         (index (if (length= ovs (1+ pos)) 0 (1+ pos))))
    (goto-char (overlay-start (nth index ovs)))
    (message "Current overlay: %s/%s" (1+ index) (length ovs))))

;;;###autoload
(defun radiance-previous-object ()
  "Jump to the previous location of symbol at point."
  (interactive)
  (let* ((result (radiance--symbol-position))
         (pos (car result))
         (ovs (cdr result))
         (index (if (= pos 0) (1- (length ovs)) (1- pos))))
    (goto-char (overlay-start (nth index ovs)))
    (message "Current overlay: %s/%s" (1+ index) (length ovs))))

(provide 'radiance)
;;; radiance.el ends here
