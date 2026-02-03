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

;;; Custom Faces:

(defface fuzzy-search-error '((t :background "red" :foreground "black"))
  "Face used to highlight errors or failed matches in fuzzy search results.")

(defface fuzzy-search-highlight '((t :background "green" :foreground "black"))
  "Face used to highlight errors or failed matches in fuzzy search results.")

;;; Internal Variables:

(defvar fuzzy-search--token-overlays nil
  "List of overlays currently used to color tokens inside the target region.")

;;; Internal Functions:

(defclass fuzzy-search-token ()
  ((text :initarg :text :reader fuzzy-search-token-text)
   (begin :initarg :begin :reader fuzzy-search-token-begin)
   (end :initarg :end :reader fuzzy-search-token-end))
  "Class representing a : single token in fuzzy search input or result.
Each instance stores:
text:   the actual string content of the token
begin:  starting position (character index) of this token in the original string
end:    ending position (exclusive) of this token in the original string
Used to preserve positional information for accurate highlighting and matching.")

(defclass fuzzy-search-token-pair ()
  ((from :initarg :from :reader fuzzy-search-token-pair-from)
   (to :initarg :to :reader fuzzy-search-token-pair-to)
   (match :initarg :match :reader fuzzy-search-token-pair-match-p))
  "Class representing a paired mapping between one input token and one candidate token in fuzzy matching.
Slots:
- from:   the input-side fuzzy-search-token (the user's search query token)
- to:     the candidate-side fuzzy-search-token (the matched token from target text)
- match:  boolean indicating whether this pair is considered a successful match
Used to record individual token-level alignment decisions when building the overall fuzzy search correspondence.")

(defclass fuzzy-search-token-match-list ()
  ((items :initarg :items :reader fuzzy-search-token-match-list-items)
   (match-size :initarg :match-size :reader fuzzy-search-token-match-list-match-size))
  "Class representing the complete matching result for one input token against all candidates.
Contains:
- items:       list of fuzzy-search-token-match objects (each records how one candidate matched this token)
- match-size:  the best (maximum) match length achieved by any candidate for this token
Mainly used as an intermediate grouping structure when computing multi-token fuzzy search results.")

(cl-defmethod fuzzy-search-token-match-list-first ((lst fuzzy-search-token-match-list))
  "Returns the first fuzzy-search-token-match object from the match list."
  (cl-first (fuzzy-search-token-match-list-items lst)))

(cl-defmethod fuzzy-search-token-match-list-last ((lst fuzzy-search-token-match-list))
  "Returns the last fuzzy-search-token-match object from the match list."
  (car (last (fuzzy-search-token-match-list-items lst))))

(cl-defmethod fuzzy-search-compare-token ((token1 fuzzy-search-token)
                                          (token2 fuzzy-search-token))
  "Compares two fuzzy search tokens and returns a fuzzy search token pair containing the source token, target token, and a match status indicating whether their text values are equal."
  (fuzzy-search-token-pair
   :from token1
   :to token2
   :match (string= (fuzzy-search-token-text token1)
                   (fuzzy-search-token-text token2))))

(cl-defmethod fuzzy-search-max-token-match-list ((lst1 fuzzy-search-token-match-list)
                                                 (lst2 fuzzy-search-token-match-list))
  "Compares two fuzzy-search-token-match-list objects and returns the one considered better.
Comparison logic:
1. The list with larger match-size wins immediately
2. If match-size is equal, prefers the list whose first match has match-p = t (i.e. contains an actual matched pair)
3. If still tied, returns the second list
Used to select the stronger matching result when merging or choosing between candidate match groups for the same input token."
  (let ((n1 (fuzzy-search-token-match-list-match-size lst1))
        (n2 (fuzzy-search-token-match-list-match-size lst2)))
    (cond
     ((> n1 n2) lst1)
     ((< n1 n2) lst2)
     ((fuzzy-search-token-pair-match-p (fuzzy-search-token-match-list-first lst1)) lst1)
     (t lst2))))

(defun fuzzy-search-compare-tokens (tokens1 tokens2)
  "Compares two lists of fuzzy search tokens pairwise, creates a list of token pair comparisons, counts the number of matches, and returns a token match list containing the comparison results and the total match count."
  (let* ((items (mapcar (lambda (pair) (fuzzy-search-compare-token (car pair) (cdr pair)))
                        (cl-pairlis tokens1 tokens2)))
         (match-size (cl-count-if #'fuzzy-search-token-pair-match-p items)))
    (fuzzy-search-token-match-list
     :items items
     :match-size match-size)))

(defun fuzzy-search--clear-highlights ()
  "Remove all highlighted overlays related to fuzzy search."
  (when fuzzy-search--token-overlays
    (dolist (ov fuzzy-search--token-overlays)
      (when (overlayp ov) (delete-overlay ov)))
    (setq fuzzy-search--token-overlays nil)))

(defun fuzzy-search--highlight-token (beg end face priority &optional error-text)
  "Create a highlight overlay within the BEG..END range. "
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'priority priority)
    (when error-text
      (overlay-put ov 'after-string (propertize (format "(%s)" error-text)
                                                'face (list face '(:slant italic)))))
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
         (token-match-list)
         (i 0)
         (k 1))
    ;; (print tokens)
    (fuzzy-search--clear-highlights)
    (if (<= n 0)
        (user-error "The query contains no available alphanumeric characters.")
      (while (< (+ i n) m)
        (if token-match-list
            (setq token-match-list
                  (fuzzy-search-max-token-match-list
                   token-match-list
                   (fuzzy-search-compare-tokens (cl-subseq region-tokens i (+ i n)) query-tokens)))
          (setq token-match-list (fuzzy-search-compare-tokens (cl-subseq region-tokens i (+ i n)) query-tokens)))
        (setq i (1+ i)))
      (if (or (not (fuzzy-search-token-match-list-p token-match-list))
              (< (fuzzy-search-token-match-list-match-size token-match-list) k))
          (message "Fuzzy matching below the threshold")
        (fuzzy-search--highlight-token
         (fuzzy-search-token-begin (fuzzy-search-token-pair-from (fuzzy-search-token-match-list-first token-match-list)))
         (fuzzy-search-token-end (fuzzy-search-token-pair-from (fuzzy-search-token-match-list-last token-match-list)))
         'fuzzy-search-highlight
         1000)
        (goto-char (fuzzy-search-token-end (fuzzy-search-token-pair-from (fuzzy-search-token-match-list-last token-match-list))))
        (recenter)
        (dolist (pair (fuzzy-search-token-match-list-items token-match-list))
          (unless (fuzzy-search-token-pair-match-p pair)
            (fuzzy-search--highlight-token
             (fuzzy-search-token-begin (fuzzy-search-token-pair-from pair))
             (fuzzy-search-token-end (fuzzy-search-token-pair-from pair))
             'fuzzy-search-error
             2000
             (fuzzy-search-token-text (fuzzy-search-token-pair-to pair)))))))))

(provide 'fuzzy-search)
;;; fuzzy-search.el ends here
