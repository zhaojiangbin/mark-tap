;;; Mark things at point.

;; Copyright (C) 2015, 2018 Jiangbin Zhao

;; Author: Jiangbin Zhao (zhaojiangbin@gmail.com)
;; Version: 0.1
;; Keywords: convenience

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3 as
;; published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Usage:
;;
;; (load "/path/to/mark-tap")
;; (require 'mark-tap)
;; (global-set-key "C-c m" 'mark-tap-hydra/body)

(require 'hydra)
(require 'thingatpt)

(defvar mark-tap-things '((c-mode          . (symbol line sexp defun word))
                          (c++-mode        . (symbol line sexp defun word))
                          (emacs-lisp-mode . (symbol sexp list defun))
                          (text-mode       . (word line sentence url))
                          (org-mode        . (word line sentence url))
                          (t               . (word symbol line sexp list sentence defun)))
  "Things to mark.")

(defvar mark-tap--orig-pt nil
  "Point before marking.")
(defvar mark-tap--things-idx 0
  "Index of things to mark.")
(defvar mark-tap--dir-thing nil
  "Thing selected directly.")
(defvar mark-tap--steps 1
  "Mark this many things forward when positive, backward when negative.")
(defvar mark-tap--last-thing nil
  "Last thing marked.")

(defun mark-tap--mode-things ()
  (let ((things (cdr (assoc major-mode mark-tap-things))))
    (unless things (setq things (cdr (assoc t mark-tap-things))))
    (unless things (setq things '(word line sentence)))
    things))

(defun mark-tap--mark-thing (thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (setq mark-tap--last-thing thing)
    (if bounds
        (let ((start (cdr bounds))
              (end   (car bounds)))
          (goto-char start)
          (set-mark end))
      (message "No %s at point." (symbol-name thing)))))

(defun mark-tap--mark-thing1 (thing)
  (setq mark-tap--dir-thing thing)
  (if (and (region-active-p) (eq thing mark-tap--last-thing))
      (let ((arg (prefix-numeric-value current-prefix-arg)))
        (forward-thing thing (* arg mark-tap--steps)))
    (goto-char mark-tap--orig-pt)
    (mark-tap--mark-thing thing))
  (setq mark-tap--things-idx
        (or (cl-position thing (mark-tap--mode-things)) -1)))

(defun mark-tap--switch-direction ()
  (interactive)
  (let ((last-cmd last-command))
    (exchange-point-and-mark)
    (setq mark-tap--steps (- mark-tap--steps))
    (setq this-command last-command)))

(defun mark-tap--get-symbol (next)
  (if (> mark-tap--things-idx -1)
      (let* ((things (mark-tap--mode-things))
             (length (length things)))
        (cl-incf mark-tap--things-idx next)
        (cond ((and (> next 0) (= mark-tap--things-idx length))
               (setq mark-tap--things-idx 0))
              ((and (< next 0) (= mark-tap--things-idx -1))
               (setq mark-tap--things-idx (1- length))))
        (elt things mark-tap--things-idx))
    (setq mark-tap--things-idx 0)
    mark-tap--dir-thing))

(defun mark-tap--cur-tsym ()
  (mark-tap--get-symbol 0))

(defun mark-tap--prev-tsym ()
  (mark-tap--get-symbol -1))

(defun mark-tap--next-tsym ()
  (mark-tap--get-symbol 1))

(defun mark-tap--cur-name ()
  (symbol-name (mark-tap--cur-tsym)))

(defun mark-tap--mark-larger ()
  (interactive)
  (goto-char mark-tap--orig-pt)
  (mark-tap--mark-thing (mark-tap--next-tsym)))

(defun mark-tap--mark-smaller ()
  (interactive)
  (goto-char mark-tap--orig-pt)
  (mark-tap--mark-thing (mark-tap--prev-tsym)))

(defvar mark-tap--labels)
(setq mark-tap--labels
      '((symbol   . ("s" . "symbol"))
        (sexp     . ("x" . "s-expr"))
        (list     . ("l" . "list"))
        (defun    . ("f" . "function"))
        (word     . ("w" . "word"))
        (line     . ("n" . "line"))
        (sentence . ("t" . "sentence"))
        (url      . ("u" . "url"))))

(defun mark-tap--init-mark ()
  (let* ((mark-index 0)
         (thing (mark-tap--cur-tsym))
         (head-key (car (cdr (assoc thing mark-tap--labels))))
         (head-sym (intern (format "mark-tap-hydra/lambda-%s" head-key)))
         (things (mark-tap--mode-things))
         (sexp-idx (cl-position 'sexp things))
         (at-brace (or (= (char-after) ?\()
                       (= (char-after) ?\{)
                       (= (char-after) ?\<)
                       (= (char-after) ?\[))))
    ;; (message "at-brace: %s, sexp-idx: %s, mark-index: %d" at-brace sexp-idx mark-index)
    (if (and at-brace sexp-idx) (setq mark-index sexp-idx))
    ;; (message "at-brace: %s, sexp-idx: %s, mark-index: %d" at-brace sexp-idx mark-index)
    (setq mark-tap--things-idx mark-index)
    (mark-tap--mark-thing thing)
    ;; For hydra-repeat
    (setq this-command head-sym)))

;;;###autoload
(defhydra mark-tap-hydra
  (:body-pre
   (progn
     (setq mark-tap--orig-pt (point)
           mark-tap--steps 1)
     (mark-tap--init-mark))
   :hint
   nil)
  "
_s_: symbol  _x_: sexp  _l_: list  _f_: defun  _w_: word  _n_: line _t_: sentence [%s(mark-tap--cur-name)]

"
  ("SPC"   mark-tap--mark-larger             "more")
  ("S-SPC" mark-tap--mark-smaller            "less")
  ("C-x"   mark-tap--switch-direction        "exchange")
  ("."     hydra-repeat                      "repeat")
  ("s"     (mark-tap--mark-thing1 'symbol))
  ("x"     (mark-tap--mark-thing1 'sexp))
  ("l"     (mark-tap--mark-thing1 'list))
  ("f"     (mark-tap--mark-thing1 'defun))
  ("w"     (mark-tap--mark-thing1 'word))
  ("n"     (mark-tap--mark-thing1 'line))
  ("t"     (mark-tap--mark-thing1 'sentence))
  ("<return>" nil)
  ("<escape>" nil))

(provide 'mark-tap)
;; end of mark-top.el
