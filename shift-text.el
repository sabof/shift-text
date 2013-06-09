;;; shift-text.el --- Move the region in 4 directions, in a way similar to Eclipse's
;;; Version: 0.2
;;; Author: sabof
;;; URL: https://github.com/sabof/shift-text
;;; Package-Requires: ((cl-lib "1.0") (es-lib "0.3"))

;;; Commentary:

;; The project is hosted at https://github.com/sabof/shift-text
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'es-lib)

(defun st--virtualize-overlay (ov)
  (prog1 (append (list (overlay-start ov) (overlay-end ov))
                 (overlay-properties ov))
    (delete-overlay ov)))

(defun st--realize-overlay (ov-spec)
  (cl-destructuring-bind
      (start end &rest props)
      ov-spec
    (let ((ov (make-overlay start end)))
      (while props (overlay-put ov (pop props) (pop props)))
      ov)))

(defun st--current-mode-indent-step ()
  (cl-case major-mode
    (otherwise 1)))

(defun st--section-marking-end-of-line (&optional pos)
  (save-excursion
    (when pos
      (goto-char pos))
    (if (and (region-active-p) (equal (current-column) 0))
        (point)
        (min (point-max) (1+ (es-total-line-end-position))))))

(defun st--normalize-pos (pos)
  (min (point-max) (max (point-min) pos)))

(defun st--shift-text-internal (arg)
  (let* (( was-active (region-active-p))
         ( first-line-was-folded
           (save-excursion
             (when was-active
               (goto-char (region-beginning)))
             (es-line-folded-p)))
         ( initial-column (current-column))
         ( start (es-total-line-beginning-position
                  (if was-active
                      (region-beginning)
                      (point))))
         ( end (st--section-marking-end-of-line
                (if was-active
                    (region-end)
                    (point))))
         ( virtual-overlays
           (mapcar 'st--virtualize-overlay (overlays-in start end)))
         ( text (delete-and-extract-region start end))
         new-start
         difference)
    (es-total-forward-line arg)
    (setq new-start (point)
          difference (- new-start start))
    (insert text)
    (unless (equal (aref text (1- (length text)))
                   (aref "\n" 0))
      (insert "\n"))
    (cl-dolist (ov virtual-overlays)
      (setf (nth 0 ov) (st--normalize-pos (+ (nth 0 ov) difference)))
      (setf (nth 1 ov) (st--normalize-pos (+ (nth 1 ov) difference))))
    (mapc 'st--realize-overlay virtual-overlays)
    (set-mark new-start)
    (exchange-point-and-mark)
    (if (or was-active first-line-was-folded)
        (setq deactivate-mark nil
              cua--explicit-region-start nil)
        (progn (move-to-column initial-column t)
               (deactivate-mark)))))

(defun st--indent-rigidly-internal (arg)
  (cond ( (region-active-p)
          (let (( start
                  (es-total-line-beginning-position
                   (region-beginning)))
                ( end
                  (st--section-marking-end-of-line
                   (region-end))))
            (set-mark end)
            (goto-char start)
            (indent-rigidly start end arg)
            (setq deactivate-mark nil)))
        ( (es-line-empty-p)
          (let* (( cur-column (current-column))
                 ( step (abs arg))
                 ( rest (mod cur-column step))
                 ( new-indent
                   (max 0 (if (zerop rest)
                              (+ cur-column arg)
                              (if (plusp arg)
                                  (+ cur-column rest)
                                  (- cur-column (- step rest)))))))
            (if (> new-indent cur-column)
                (indent-to new-indent)
                (goto-char (+ new-indent (line-beginning-position)))
              (delete-region (point) (line-end-position))
              )))
        ( t (indent-rigidly
             (es-total-line-beginning-position (point))
             (st--section-marking-end-of-line (point))
             arg))))

;;;###autoload
(defun shift-text-down ()
  "Move region or the current line down."
  (interactive)
  (st--shift-text-internal 1))

;;;###autoload
(defun shift-text-up ()
  "Move region or the current line up."
  (interactive)
  (st--shift-text-internal -1))

;;;###autoload
(defun shift-text-left ()
  "Move region or the current line left."
  (interactive)
  (st--indent-rigidly-internal
   (* -1 (st--current-mode-indent-step))))

;;;###autoload
(defun shift-text-right ()
  "Move region or the current line right."
  (interactive)
  (st--indent-rigidly-internal
   (st--current-mode-indent-step)))

(provide 'shift-text)
;;; shift-text.el ends here
