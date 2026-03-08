;;; fuzzy-search-lcs-tests.el --- Unit tests for fuzzy-search-lcs.el  -*- lexical-binding: t; -*-

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

;; Unit tests for fuzzy-search-lcs.el

;;; Code:

(require 'ert)

;; Load dependencies
(defun read-file-lines (file)
  "Read lines from FILE and return them as a list."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (split-string (buffer-string) "\n" t))))

(defun load-dependencies-path ()
  "Add all dependencies to load-path."
  (let* ((file-name (file-name-concat
                     (or load-file-name
                         (buffer-file-name))
                     ".."
                     ".."
                     "dependencies.txt"))
         (urls (read-file-lines file-name))
         (names (mapcar (lambda (url) (car (last (split-string url "/")))) urls)))
    (dolist (name names)
      (add-to-list 'load-path (format "~/.emacs.d/lisp/%s" name)))))

(load-dependencies-path)
(require 'fuzzy-search-lcs)

;; ==============================================================================
;; Tests for fuzzy-search-lcs-make-dp
;; ==============================================================================

(ert-deftest test-fuzzy-search-lcs-make-dp--zero-dimensions ()
  "Test make-dp with zero dimensions."
  (should (equal (fuzzy-search-lcs-make-dp 0 0) [])))

(ert-deftest test-fuzzy-search-lcs-make-dp--single-cell ()
  "Test make-dp with 1x1 dimensions."
  (let ((dp (fuzzy-search-lcs-make-dp 1 1)))
    (should (equal (length dp) 1))
    (should (equal (length (elt dp 0)) 1))
    (should (equal (elt dp 0) [0]))))

(ert-deftest test-fuzzy-search-lcs-make-dp--rectangular ()
  "Test make-dp creates correct rectangular structure."
  (let ((dp (fuzzy-search-lcs-make-dp 3 4)))
    (should (equal (length dp) 3))
    (dotimes (i 3)
      (should (equal (length (elt dp i)) 4))
      (dotimes (j 4)
        (should (equal (elt (elt dp i) j) 0))))))

(ert-deftest test-fuzzy-search-lcs-make-dp--independent-rows ()
  "Test that rows in make-dp are independent vectors."
  (let ((dp (fuzzy-search-lcs-make-dp 2 2)))
    (setf (elt (elt dp 0) 0) 99)
    (should (equal (elt (elt dp 0) 0) 99))
    (should (equal (elt (elt dp 1) 0) 0))))

;; ==============================================================================
;; Tests for fuzzy-search-lcs-build-dp-suffix
;; ==============================================================================

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--empty-arrays ()
  "Test build-dp-suffix with empty arrays."
  (let ((dp (fuzzy-search-lcs-build-dp-suffix [] [])))
    (should (equal (length dp) 1))
    (should (equal (length (elt dp 0)) 1))
    (should (equal (elt (elt dp 0) 0) 0))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--identical-single-char ()
  "Test build-dp-suffix with identical single characters."
  (let ((dp (fuzzy-search-lcs-build-dp-suffix ["a"] ["a"])))
    ;; dp[0][0] = 1 + dp[1][1] = 1 + 0 = 1
    (should (equal (elt (elt dp 0) 0) 1))
    ;; Border should be 0
    (should (equal (elt (elt dp 1) 0) 0))
    (should (equal (elt (elt dp 0) 1) 0))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--different-single-char ()
  "Test build-dp-suffix with different single characters."
  (let ((dp (fuzzy-search-lcs-build-dp-suffix ["a"] ["b"])))
    (should (equal (elt (elt dp 0) 0) 0))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--identical-strings ()
  "Test build-dp-suffix with identical strings."
  (let* ((a ["a" "b" "c"])
         (b ["a" "b" "c"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    ;; LCS of full strings should be 3
    (should (equal (elt (elt dp 0) 0) 3))
    ;; LCS of "bc" and "bc" should be 2
    (should (equal (elt (elt dp 1) 1) 2))
    ;; LCS of "c" and "c" should be 1
    (should (equal (elt (elt dp 2) 2) 1))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--no-common-subsequence ()
  "Test build-dp-suffix with no common elements."
  (let* ((a ["a" "b" "c"])
         (b ["d" "e" "f"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    (should (equal (elt (elt dp 0) 0) 0))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--partial-match ()
  "Test build-dp-suffix with partial matching."
  (let* ((a ["a" "b" "c"])
         (b ["a" "x" "c"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    ;; LCS should be 2 ("a" and "c")
    (should (equal (elt (elt dp 0) 0) 2))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--second-longer ()
  "Test build-dp-suffix when second array is longer."
  (let* ((a ["a" "b"])
         (b ["a" "b" "c" "d"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    (should (equal (elt (elt dp 0) 0) 2))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--first-longer ()
  "Test build-dp-suffix when first array is longer."
  (let* ((a ["a" "b" "c" "d"])
         (b ["a" "b"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    (should (equal (elt (elt dp 0) 0) 2))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--custom-test-function ()
  "Test build-dp-suffix with custom test function (case-insensitive)."
  (let* ((a ["A" "B"])
         (b ["a" "b"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b (lambda (x y)
                                                      (equal (downcase x)
                                                             (downcase y))))))
    (should (equal (elt (elt dp 0) 0) 2))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--numbers ()
  "Test build-dp-suffix with number arrays."
  (let* ((a [1 2 3])
         (b [1 2 3])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    (should (equal (elt (elt dp 0) 0) 3))))

(ert-deftest test-fuzzy-search-lcs-build-dp-suffix--repeated-elements ()
  "Test build-dp-suffix with repeated elements."
  (let* ((a ["a" "a" "a"])
         (b ["a" "a"])
         (dp (fuzzy-search-lcs-build-dp-suffix a b)))
    (should (equal (elt (elt dp 0) 0) 2))))

;; ==============================================================================
;; Tests for fuzzy-search-lcs-diff
;; ==============================================================================

(ert-deftest test-fuzzy-search-lcs-diff--identical-lists ()
  "Test diff with identical lists."
  (let ((result (fuzzy-search-lcs-diff '("a" "b" "c") '("a" "b" "c"))))
    (should (equal result
                   '((equal . "a") (equal . "b") (equal . "c"))))))

(ert-deftest test-fuzzy-search-lcs-diff--empty-first-list ()
  "Test diff when first list is empty (all additions)."
  (let ((result (fuzzy-search-lcs-diff '() '("a" "b"))))
    (should (equal result
                   '((more . "a") (more . "b"))))))

(ert-deftest test-fuzzy-search-lcs-diff--empty-second-list ()
  "Test diff when second list is empty (all deletions)."
  (let ((result (fuzzy-search-lcs-diff '("a" "b") '())))
    (should (equal result
                   '((less . "a") (less . "b"))))))

(ert-deftest test-fuzzy-search-lcs-diff--single-insertion ()
  "Test diff with single insertion."
  (let ((result (fuzzy-search-lcs-diff '("a" "c") '("a" "b" "c"))))
    (should (equal result
                   '((equal . "a") (more . "b") (equal . "c"))))))

(ert-deftest test-fuzzy-search-lcs-diff--single-deletion ()
  "Test diff with single deletion."
  (let ((result (fuzzy-search-lcs-diff '("a" "b" "c") '("a" "c"))))
    (should (equal result
                   '((equal . "a") (less . "b") (equal . "c"))))))

(ert-deftest test-fuzzy-search-lcs-diff--substitution ()
  "Test diff with substitution (delete + insert)."
  (let ((result (fuzzy-search-lcs-diff '("a" "x" "c") '("a" "b" "c"))))
    (should (equal result
                   '((equal . "a") (less . "x") (more . "b") (equal . "c"))))))

(ert-deftest test-fuzzy-search-lcs-diff--multiple-changes ()
  "Test diff with multiple changes."
  (let ((result (fuzzy-search-lcs-diff '("a" "b" "c" "d") '("a" "x" "y" "d"))))
    (should (equal result
                   '((equal . "a")
                     (less . "b")
                     (less . "c")
                     (more . "x")
                     (more . "y")
                     (equal . "d"))))))

(ert-deftest test-fuzzy-search-lcs-diff--numbers ()
  "Test diff with number lists."
  (let ((result (fuzzy-search-lcs-diff '(1 2 3) '(1 2 3))))
    (should (equal result
                   '((equal . 1) (equal . 2) (equal . 3))))))

(ert-deftest test-fuzzy-search-lcs-diff--custom-test-function ()
  "Test diff with custom test function (case-insensitive)."
  (let ((result (fuzzy-search-lcs-diff '("A" "B") '("a" "b")
                                        (lambda (x y)
                                          (equal (downcase x)
                                                 (downcase y))))))
    (should (equal result
                   '((equal . "A") (equal . "B"))))))

(ert-deftest test-fuzzy-search-lcs-diff--custom-item-builder ()
  "Test diff with custom item builder."
  (let ((result (fuzzy-search-lcs-diff '("a") '("b")
                                        nil
                                        (lambda (type elem)
                                          (list type elem)))))
    (should (equal result
                   '((less "a") (more "b"))))))

(ert-deftest test-fuzzy-search-lcs-diff--prefix-removed ()
  "Test diff when prefix is removed."
  (let ((result (fuzzy-search-lcs-diff '("a" "b" "c") '("b" "c"))))
    (should (equal result
                   '((less . "a") (equal . "b") (equal . "c"))))))

(ert-deftest test-fuzzy-search-lcs-diff--suffix-added ()
  "Test diff when suffix is added."
  (let ((result (fuzzy-search-lcs-diff '("a" "b") '("a" "b" "c"))))
    (should (equal result
                   '((equal . "a") (equal . "b") (more . "c"))))))

(ert-deftest test-fuzzy-search-lcs-diff--interleaved-changes ()
  "Test diff with interleaved changes."
  (let ((result (fuzzy-search-lcs-diff '("a" "x" "b") '("a" "y" "b"))))
    (should (equal result
                   '((equal . "a") (less . "x") (more . "y") (equal . "b"))))))

(ert-deftest test-fuzzy-search-lcs-diff--complex-case ()
  "Test diff with complex case from the original comment."
  (let ((result (fuzzy-search-lcs-diff '(1 2 3) '(1 2 4 4))))
    (should (equal result
                   '((equal . 1) (equal . 2) (less . 3) (more . 4) (more . 4))))))

;; ==============================================================================
;; Test Runner
;; ==============================================================================

(defun run-fuzzy-search-lcs-tests ()
  "Run all fuzzy-search-lcs tests."
  (interactive)
  (ert-run-tests-batch-and-exit 'fuzzy-search-lcs))

(provide 'fuzzy-search-lcs-tests)
;;; fuzzy-search-lcs-tests.el ends here
