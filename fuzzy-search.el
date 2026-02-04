;;; my-search.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin  <ginqi7@gmail.com>
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

(require 'cl-lib)
(require 'eieio)
(require 'fuzzy-search-lcs)

;;; Custom Faces:

;;; Internal Variables:

(defvar fuzzy-search--token-overlays nil
  "List of overlays currently used to color tokens inside the target region.")

;;; Internal Functions:

(defclass fuzzy-search-token ()
  ((text :initarg :text :reader fuzzy-search-token-text)
   (begin :initarg :begin :reader fuzzy-search-token-begin)
   (end :initarg :end :reader fuzzy-search-token-end)
   (diff-type :initarg :diff-type :reader fuzzy-search-token-diff-type))
  "Class representing a : single token in fuzzy search input or result.
Each instance stores:
text:      the actual string content of the token
begin:     starting position (character index) of this token in the original string
end:       ending position (exclusive) of this token in the original string
diff-type: equal, less or more
Used to preserve positional information for accurate highlighting and matching.")

(defclass fuzzy-search-token-differences ()
  ((tokens :initarg :tokens :reader fuzzy-search-token-differences-tokens)
   (equal-size :initarg :equal-size :reader fuzzy-search-token-differences-euqal-size))
  "Defines a class that holds information about token differences, including the tokens themselves and a measure of their equal size. The 'tokens' slot stores the relevant tokens, and the 'equal-size' slot indicates how many tokens match.")

(cl-defmethod fuzzy-search-token-differences-first ((lst fuzzy-search-token-differences))
  "Returns the first token from the tokens in the specified fuzzy-search-token-differences instance."
  (cl-first (fuzzy-search-token-differences-tokens lst)))

(cl-defmethod fuzzy-search-token-differences-last ((lst fuzzy-search-token-differences))
  "Returns the last token from the tokens in the specified fuzzy-search-token-differences instance."
  (car (last (fuzzy-search-token-differences-tokens lst))))

(cl-defmethod fuzzy-search-token-equal ((token1 fuzzy-search-token)
                                        (token2 fuzzy-search-token))
  "Compares the text of two fuzzy-search-token objects for equality, returning T if they match."
  (string= (fuzzy-search-token-text token1)
           (fuzzy-search-token-text token2)))

(defun fuzzy-search-token--builder (diff-type token)
  "Builds a new fuzzy-search-token object using the specified diff-type and retaining the text, begin, and end values from the provided token."
  (fuzzy-search-token
        :diff-type diff-type
        :text (fuzzy-search-token-text token)
        :begin (fuzzy-search-token-begin token)
        :end (fuzzy-search-token-end token)))

(cl-defmethod fuzzy-search-min-difference ((lst1 fuzzy-search-token-differences)
                                           (lst2 fuzzy-search-token-differences))
  "Compares the two fuzzy-search-token-differences objects by their equal-size values, returning the one with the larger equal-size. If they have the same value, it returns the second object."
  (let ((equal-size1 (fuzzy-search-token-differences-euqal-size lst1))
        (equal-size2 (fuzzy-search-token-differences-euqal-size lst2))
        (size1 (length (fuzzy-search-token-differences-tokens lst1)))
        (size2 (length (fuzzy-search-token-differences-tokens lst2))))
    (cond ((> equal-size1 equal-size2) lst1)
          (t lst2))))

(defun fuzzy-search--pre-less-token (tokens token)
  "Searches backward from the position of the given token in tokens for the first token whose diff-type is 'less', returning that token or nil if none is found."
  (let ((index (cl-position token tokens)))
    (cl-loop for i downfrom index to 0
             do
             (when (equal 'less (fuzzy-search-token-diff-type (nth i tokens)))
               (cl-return (nth i tokens))))))

(defun fuzzy-search-compare-tokens (tokens1 tokens2)
  "Compares two lists of fuzzy search tokens pairwise, creates a list of token pair comparisons, counts the number of matches, and returns a token match list containing the comparison results and the total match count."
  (let* ((m (length tokens1))
         (n (length tokens2))
         (tokens (fuzzy-search-lcs-diff tokens1 tokens2
                                        #'fuzzy-search-token-equal
                                        #'fuzzy-search-token--builder))
         (equal-size (cl-count-if (lambda (token) (equal 'equal (fuzzy-search-token-diff-type token))) tokens))
         (pre-less-token))
    ;; (print equal-size)
    ;; (print tokens)
    (dolist (token tokens)
      (when (equal 'more (fuzzy-search-token-diff-type token))
        (setq pre-less-token (fuzzy-search--pre-less-token tokens token))
        (when pre-less-token
          (eieio-oset token 'begin (fuzzy-search-token-begin pre-less-token))
          (eieio-oset token 'end (fuzzy-search-token-end pre-less-token)))))
    (fuzzy-search-token-differences
     :tokens tokens
     :equal-size equal-size)))

(defun fuzzy-search--clear-highlights ()
  "Remove all highlighted overlays related to fuzzy search."
  (when fuzzy-search--token-overlays
    (dolist (ov fuzzy-search--token-overlays)
      (when (overlayp ov) (delete-overlay ov)))
    (setq fuzzy-search--token-overlays nil)))

(defun fuzzy-search--highlight-token (beg end token)
  "Create a highlight overlay within the BEG..END range. "
  ;; (print token)
  (let ((ov (make-overlay beg end))
        (diff-type (fuzzy-search-token-diff-type token)))
    (pcase diff-type
      ('less
       (overlay-put ov 'face 'diff-removed))
      ('equal
       (overlay-put ov 'face 'diff-index))
      ('more
       (overlay-put ov 'after-string
                    (propertize (format "(%s)" (fuzzy-search-token-text token))
                                'face (list 'diff-added '(:slant italic))))))

    (push ov fuzzy-search--token-overlays)
    ov))

(defun fuzzy-search--region-tokens (start end)
  "Extracts tokens from the specified region between start and end positions, creates a list of tokens with their text, start, and end positions, and returns the tokens in reverse order."
  (let ((region-tokens))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\([[:alnum:]'’]+\\)" end t)
        (push
         (fuzzy-search-token
          :text (downcase (match-string-no-properties 1))
          :begin (match-beginning 1)
          :end (match-end 1))
         region-tokens)))
    (reverse region-tokens)))

(defun fuzzy-search--query-tokens (str)
  "Generates a list of tokens from the given string by treating the string as a temporary buffer and extracting tokens with their text, start, and end positions."
  (with-temp-buffer
    (insert str)
    (fuzzy-search--region-tokens (point-min) (point-max))))

;;;
(defun fuzzy-search (query &optional threshold)
  "Performs a fuzzy search for a given query within a specified text region or window, highlights matching and non-matching tokens, and navigates to the match if it meets the specified threshold. Allows for optional threshold adjustment and handles cases where the query contains no valid characters or no matches are found."
  (interactive
   (list (read-string "Query: ")
         (let ((v (read-number "Threshold (0-1, default 0.5): " 0.5)))
           (max 0.0 (min 1.0 v)))))
  ;; (print query)
  (let* ((threshold (or threshold 0.5))
         (threshold (max 0.0 (min 1.0 threshold)))
         (region-p (region-active-p))
         (start (if region-p (region-beginning) (window-start)))
         (end   (if region-p (region-end)       (window-end)))
         (query-tokens (fuzzy-search--query-tokens query))
         (region-tokens (fuzzy-search--region-tokens start end))
         (n (length query-tokens))
         (m (length region-tokens))
         (differences)
         (i 0)
         (k 1))
    ;; (print tokens)
    (fuzzy-search--clear-highlights)
    (if (<= n 0)
        (user-error "The query contains no available alphanumeric characters.")
      (while (< (+ i n) m)
        (if differences
            (setq differences
                  (fuzzy-search-min-difference
                   differences
                   (fuzzy-search-compare-tokens (cl-subseq region-tokens i (+ i n)) query-tokens)))
          (setq differences (fuzzy-search-compare-tokens (cl-subseq region-tokens i (+ i n)) query-tokens)))
        (setq i (1+ i)))
      (if (or (not (fuzzy-search-token-differences-p differences))
              (< (fuzzy-search-token-differences-euqal-size differences) k))
          (message "Fuzzy matching below the threshold")
        (goto-char (fuzzy-search-token-end (fuzzy-search-token-differences-last differences)))
        (recenter)
        (dolist (token (fuzzy-search-token-differences-tokens differences))
          (fuzzy-search--highlight-token
             (fuzzy-search-token-begin token)
             (fuzzy-search-token-end token)
             token))))))

(provide 'fuzzy-search)
;;; fuzzy-search.el ends here
