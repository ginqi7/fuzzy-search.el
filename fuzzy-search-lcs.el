;;; fuzzy-search-lcs.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun fuzzy-search-lcs-make-dp (m n)
  "Creates and returns a two-dimensional vector of size m by n filled with zeros, intended for use in LCS (Longest Common Subsequence) dynamic programming calculations."
  (let ((dp (make-vector m nil)))
    (dotimes (i m dp)
      (setf (elt dp i) (make-vector n 0)))))

(defun fuzzy-search-lcs-build-dp-suffix (a b &optional test)
  "dp[i][j] = LCS length of a[i..] and b[j..]. a/b are vectors."
  (unless test
    (setq test #'equal))
  (let* ((m (length a))
         (n (length b))
         (dp (fuzzy-search-lcs-make-dp (1+ m) (1+ n)))
         (i (1- m))
         (j (1- n)))
    (while (>= i 0)
      (setq j (1- n))
      (while (>= j 0)
        (setf (elt (elt dp i) j)
              (if (funcall test (elt a i) (elt b j))
                  (progn
                   (1+ (elt (elt dp (1+ i)) (1+ j))))
                (max (elt (elt dp (1+ i)) j)
                     (elt (elt dp i) (1+ j)))))
        (setq j (1- j)))
      (setq i (1- i)))
    dp))

(defun fuzzy-search-lcs--diff-item-builder (type element)
  "Diff item builder."
  (cons type element))

(defun fuzzy-search-lcs-diff (lst1 lst2 &optional test item-builder)
  "Left-to-right diff using suffix-LCS dp."
  (setq test (or test #'equal))
  (setq item-builder (or item-builder #'fuzzy-search-lcs--diff-item-builder))
  (let* ((a (vconcat lst1))
         (b (vconcat lst2))
         (m (length a))
         (n (length b))
         (dp (fuzzy-search-lcs-build-dp-suffix a b test))
         (i 0)
         (j 0)
         differences)
    (while (or (< i m) (< j n))
      (cond
       ((and (< i m) (< j n)
             (funcall test (aref a i) (aref b j)))
        (push (funcall item-builder 'equal (aref a i)) differences)
        (setq i (1+ i) j (1+ j)))

       ((or (= j n)
            (and (< i m)
                 (>= (aref (aref dp (1+ i)) j)
                     (aref (aref dp i) (1+ j)))))
        (push (funcall item-builder 'less (aref a i)) differences)
        (setq i (1+ i)))

       (t
        (push (funcall item-builder 'more (aref b j)) differences)
        (setq j (1+ j)))))
    (nreverse differences)))

;; (fuzzy-search-lcs-diff '(1 2 3) '(1 2 4 4))

(provide 'fuzzy-search-lcs)
;;; fuzzy-search-lcs.el ends here
