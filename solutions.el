;;; solutions.el --- Solutions for AoC 2025 -*- mode: emacs-lisp; lexical-binding: t -*-

;; Copyright (C) 2025 Lucius Martius

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These are my solutions for 2025's Advent of Code.

;; Each solution is an s-expression that can be evaluated in Emacs by moving point at the end of
;; the expression and hitting C-x C-e (eval-last-sexp).
;; I've considered making them executable scripts, but this way it's just so much easier to work
;; with.

;;; Code:

;; Day 1 - Part 1
(with-temp-buffer
  (insert-file-contents-literally "input/day1")
  (goto-char (point-min))
  (let ((count 50)
        (pw 0))
    (while (not (= (point) (point-max)))
      (looking-at (rx bol (group (or "R" "L")) (group (+ num)) eol))
      (let* ((fn (pcase (match-string 1)
                   ("R" #'+)
                   ("L" #'-)))
             (num (string-to-number (match-string 2))))
        (setq count (funcall fn count num))
        (when (zerop (% count 100))
          (setq pw (1+ pw))))
      (forward-line))
    pw))

;; Day 1 - Part 2
(with-temp-buffer
  (insert-file-contents-literally "input/day1")
  (goto-char (point-min))
  (let ((count 50)
        (pw 0))
    (while (not (= (point) (point-max)))
      (looking-at (rx bol (group (or "R" "L")) (group (+ num)) eol))
      (let* ((fn (pcase (match-string 1)
                   ("R" #'+)
                   ("L" #'-)))
             (num (string-to-number (match-string 2)))
             (hits (/ num 100))
             (oldcount count)
             (modulo (% num 100)))
        (when (> hits 0)
          (setq pw (+ pw hits)))

        (setq count (funcall fn count modulo))
        (cond ((= oldcount 0)
               (when (< count 0)
                 (setq count (+ 100 count))))
              ((< count 0)
               (setq pw (1+ pw))
               (setq count (+ 100 count)))
              ((= count 0)
               (setq pw (1+ pw)))
              ((>= count 100)
               (setq pw (1+ pw))
               (setq count (- count 100))))
        )
      (forward-line))
    pw))

;; Day 2 - Part 1
(with-temp-buffer
  (insert-file-contents-literally "input/day2")
  (goto-char (point-min))
  (let ((acc 0))
    (while (not (= (point) (point-max)))
      (looking-at (rx (group (+ num)) "-" (group (+ num)) (? ",") (? "\n")))
      (let* ((start (string-to-number (match-string 1)))
             (end (string-to-number (match-string 2)))
             (count start))
        (while (/= count (1+ end))
          (let* ((strcount (number-to-string count)))
            (when (and (zerop (% (length strcount) 2))
                       (string= (substring strcount 0 (/ (length strcount) 2))
                                (substring strcount (/ (length strcount) 2))))
              (setq acc (+ acc count))))
          (setq count (1+ count))))
      (goto-char (match-end 0)))
    acc))

;; Day 2 - Part 2
;; --------------
;; Not the fastest solution. Maybe I have overlooked something that makes this easier.
;; TODO: Look at other peoples solution (also for Part 1)
(with-temp-buffer
  (insert-file-contents-literally "input/day2")
  (goto-char (point-min))
  (let ((acc 0))
    (while (not (= (point) (point-max)))
      (looking-at (rx (group (+ num)) "-" (group (+ num)) (? ",") (? "\n")))
      (let* ((start (string-to-number (match-string 1)))
             (end (string-to-number (match-string 2)))
             (count start))
        (while (/= count (1+ end))
          (let* ((strcount (number-to-string count)))
            (seq-find (lambda (div)
                        (when (zerop (% (length strcount) div))
                          (let* ((partlen (/ (length strcount) div))
                                 (seq (seq-partition strcount partlen)))
                            (unless (seq-find (lambda (elt)
                                                (not (string= (car seq) elt)))
                                              (cdr seq))
                              (setq acc (+ acc count))))))
                      (number-sequence 2 (length strcount))))
          (setq count (1+ count))))
      (goto-char (match-end 0)))
    acc))

;; Day 3 - Part 1
;; --------------
;; Simple solution based on the fact that we can wait for the last digit to commit to the
;; second number. This wouldn't work in Part 2 as I immediately found out. The solution
;; of Part 2 can solve this one too if the num-batteries parameter is changed to 2.
(with-temp-buffer
  (insert-file-contents-literally "input/day3")
  (goto-char (point-min))
  (let ((acc 0))
    (while (not (= (point) (point-max)))
      (let ((first 0) second)
        (while (not (eq (char-after) ?\n))
          (let ((digit (- (char-after) ?0)))
            (cond ((and (not (eq (char-after (1+ (point))) ?\n))
                        (> digit first))
                   (setq first digit)
                   (setq second 0))
                  ((> digit second)
                   (setq second digit))))
          (forward-char))
        (setq acc (+ acc (* first 10) second)))
      (forward-line))
    acc))

;; Day 3 - Part 2
;; --------------
;; This took some more thinking to get to the solution of trimming out the weakest digits.
;; However, this makes this a general solution that can solve Part 1 too.
(let ((num-batteries 12))
  (with-temp-buffer
    (insert-file-contents-literally "input/day3")
    (goto-char (point-min))
    (let ((acc 0))
      (while (not (= (point) (point-max)))
        (let ((totrim (- (line-end-position) (line-beginning-position) num-batteries)))
          (while (not (zerop totrim))
            (let ((digit (- (char-after) ?0)))
              (cond ((eq (char-after) ?\n)
                     (delete-char -1)
                     (setq totrim (1- totrim)))
                    ((> (- (char-after (1+ (point))) ?0) digit)
                     (delete-char 1)
                     (goto-char (line-beginning-position))
                     (setq totrim (1- totrim)))
                    (t (forward-char)))))
          (setq acc (+ acc (string-to-number (buffer-substring (line-beginning-position) (line-end-position))))))
        (forward-line))
      acc)))

;; Day 4 - Part 1
;; --------------
;; Like most problems operating on random coordinates in two-dimensional data, this problem isn't
;; very well suited to Emacs-lisp. The usual approach of working directly in the input-buffer
;; still works, but helper-functions were needed to not make this overly convoluted because Emacs
;; otherwise lacks the high-level functions for 2D navigation.
(cl-labels ((char-at (x y)
              (let ((line (line-number-at-pos))
                    (col (current-column)))
                (save-excursion
                  (forward-line y)
                  (goto-char (+ (point) col x))
                  (if (and (= (+ col x) (current-column))
                           (= (+ line y) (line-number-at-pos)))
                      (or (char-after) ?\0)
                    ?\0))))
            (count-adjacent (char)
              (+ (if (= (char-at -1 0) char) 1 0)
                 (if (= (char-at -1 -1) char) 1 0)
                 (if (= (char-at 0 -1) char) 1 0)
                 (if (= (char-at 1 -1) char) 1 0)
                 (if (= (char-at 1 0) char) 1 0)
                 (if (= (char-at 1 1) char) 1 0)
                 (if (= (char-at 0 1) char) 1 0)
                 (if (= (char-at -1 1) char) 1 0))))
  (with-temp-buffer
    (insert-file-contents-literally "input/day4")
    (goto-char (point-min))
    (let ((acc 0))
      (while (not (= (point) (point-max)))
        (when (and (= (char-after) ?@)
                   (< (count-adjacent ?@) 4))
          (setq acc (1+ acc)))
        (forward-char))
      acc)))

;; Day 4 - Part 2
;; --------------
;; This is essentially just the same solution as before, but with replacing the chars directly
;; in the input buffer and inefficient backtracking whenever a character could be cleared.
;; There is probably a better solution. Maybe look it up.
(cl-labels ((char-at (x y)
              (let ((line (line-number-at-pos))
                    (col (current-column)))
                (save-excursion
                  (forward-line y)
                  (goto-char (+ (point) col x))
                  (if (and (= (+ col x) (current-column))
                           (= (+ line y) (line-number-at-pos)))
                      (or (char-after) ?\0)
                    ?\0))))
            (count-adjacent (char)
              (+ (if (= (char-at -1 0) char) 1 0)
                 (if (= (char-at -1 -1) char) 1 0)
                 (if (= (char-at 0 -1) char) 1 0)
                 (if (= (char-at 1 -1) char) 1 0)
                 (if (= (char-at 1 0) char) 1 0)
                 (if (= (char-at 1 1) char) 1 0)
                 (if (= (char-at 0 1) char) 1 0)
                 (if (= (char-at -1 1) char) 1 0))))
  (with-temp-buffer
    (insert-file-contents-literally "input/day4")
    (let ((acc 0) done)
      (while (not done)
        (setq done t)
        (goto-char (point-min))
        (while (not (= (point) (point-max)))
          (when (and (= (char-after) ?@)
                     (< (count-adjacent ?@) 4))
            (delete-char 1)
            (insert ?.)
            (setq done nil)
            (setq acc (1+ acc)))
          (forward-char)))
      acc)))

;; Day 5 - Part 1
;; --------------
;; This was pretty straight forward. Parsing all the ranges is the simplest thing and then
;; it's just a few lookups if the ids are in any of the ranges.
;; However this is a really bad Part 1 for the clusterfuck that is Part 2. Nothing can be
;; reused and nothing can be generalized.
(with-temp-buffer
  (insert-file-contents-literally "input/day5")
  (goto-char (point-min))
  (let ((count 0)
        (ranges '())
        (ingredients '()))
    (while (not (= (point) (point-max)))
      (cond ((looking-at (rx bol (group (+ num)) ?- (group (+ num)) eol))
             (push (cons (string-to-number (match-string 1))
                         (string-to-number (match-string 2)))
                   ranges))
            ((looking-at (rx bol (group (+ num)) eol))
             (push (string-to-number (match-string 1)) ingredients)))
      (forward-line))
    (dolist (ingr ingredients)
      (when (seq-some (lambda (range)
                        (and (>= ingr (car range))
                             (<= ingr (cdr range))))
                      ranges)
        (setq count (1+ count))))
    count))

;; Day 5 - Part 2
;; --------------
;; So far the most annoying Puzzle yet, mostly due to the example being really, really bad.
;; It doesn't even touch half the corner cases you have to consider in the real input data.
;; I've added a second version of the example (day5.example2) that serves as a full test-case
;; The solution is fast, but not the most readable. Once I got the correct solution I had lost
;; all motivation to clean it up a bit aftewards.
;; TODO: Better solution is sorting the ranges by the starting value and tracking the highest
;; processed number as a threshold.
(with-temp-buffer
  (insert-file-contents-literally "input/day5")
  (goto-char (point-min))
  (let ((count 0)
        (ranges '())
        (ingredients '()))
    (while (not (= (point) (point-max)))
      (cond ((looking-at (rx bol (group (+ num)) ?- (group (+ num)) eol))
             (push (cons (string-to-number (match-string 1))
                         (string-to-number (match-string 2)))
                   ranges)))
      (forward-line))
    (seq-do-indexed (lambda (range idx)
                      (when (not (zerop (cdr range)))
                        (let ((repeat t))
                          (while repeat
                            (setq repeat nil)
                            (mapc (lambda (elt)
                                    (when (and (> (cdr elt) (cdr range))
                                               (<= (car elt) (cdr range)))
                                      (when (< (car elt) (car range))
                                        (setf (car range) (car elt))
                                        (setq repeat t))
                                      (setf (car elt) (1+ (cdr range))))
                                    (when (and (< (car elt) (car range))
                                               (>= (cdr elt) (car range)))
                                      (setf (cdr elt) (1- (car range))))
                                    (when (and (>= (car elt) (car range))
                                               (<= (cdr elt) (cdr range)))
                                      (setf (car elt) 1
                                            (cdr elt) 0)))
                                  (nthcdr (1+ idx) ranges))))
                        (setq count (+ count (- (1+ (cdr range)) (car range))))))
                    ranges)
    count))

;; Does not yet work
(with-temp-buffer
  (insert-file-contents-literally "input/day5")
  (goto-char (point-min))
  (let ((count 0)
        (threshold 0)
        (ranges '())
        (ingredients '()))
    (while (not (= (point) (point-max)))
      (cond ((looking-at (rx bol (group (+ num)) ?- (group (+ num)) eol))
             (push (cons (string-to-number (match-string 1))
                         (string-to-number (match-string 2)))
                   ranges)))
      (forward-line))
    (seq-sort-by 'car '< ranges)
    (dolist (range ranges)
      (setcar range (max (car range) threshold))
      (when (>= (cdr range) (car range))
        (setq count (+ count (- (cdr range) (car range)) 1))
        (setq threshold (1+ (cdr range)))))
    count))

;; Day 6 - Part 1
;; --------------
;; Unlike Part 2 this was pretty interesting and elegant to solve in Elisp.
;; It's reading the data into a transposed nested list and then evals it as elisp code.
;; Essentially it's a Cephalopod-math to Elisp transpiler.
(with-temp-buffer
  (insert-file-contents-literally "input/day6")
  (goto-char (point-min))
  (let ((data (make-list (count-words (point) (line-end-position)) nil))
        (idx 0))
    (while (not (= (point) (point-max)))
      (cond ((looking-at (rx (*? whitespace) eol))
             (setq idx 0)
             (forward-line))
            ((looking-at (rx (*? whitespace) (group (+ num))))
             (push (string-to-number (match-string 1)) (car (nthcdr idx data)))
             (setq idx (1+ idx))
             (goto-char (match-end 0)))
            ((looking-at (rx (*? whitespace) (group (or ?* ?+))))
             (push (pcase (match-string 1) ("*" #'*) ("+" #'+))
                   (car (nthcdr idx data)))
             (setq idx (1+ idx))
             (goto-char (match-end 0)))))
    (push #'+ data)
    (eval data)))

;; Day 6 - Part 2
;; --------------
;; This was some bullshit.
;; Back-asswards-parsing of numbers arranged in columns. Worst-case scenario in Emacs-Lisp.
;; Most of the time I spent looking into why (next-line) wouldn't just move down one row
;; without changing the column. These functions just aren't meant for this and trip on any
;; and all whitespace. What worked was going to the beginning of the next line and then moving
;; forward to the column.
(with-temp-buffer
  (insert-file-contents-literally "input/day6")
  (goto-char (point-min))
  (let ((col (- (line-end-position) (point)))
        (temp 0)
        (data nil)
        (total 0))
    (while (>= col 0)
      (goto-char (+ (point-min) col))
      (while (not (= (point) (point-max)))
        (cond ((or (= ?+ (char-after))
                   (= ?* (char-after)))
               (unless (= temp 0)
                 (push temp data))
               (setq total (+ total (apply (pcase (char-after) (?+ #'+) (?* #'*)) data))
                     data nil
                     temp 0))
              ((or (= ?\s (char-after))
                   (= ?\n (char-after)))
               (unless (= temp 0)
                 (push temp data)
                 (setq temp 0)))
              ((>= (char-after) ?0)
               (setq temp (+ (* temp 10) (- (char-after) ?0)))))
        (forward-line)
        (goto-char (+ (point) col)))
      (setq col (1- col)))
    total))

;; Day 7 - Part 1
;; --------------
;; Much easier than day 6 - part 2 for now. And while it's not the most inspiring code it
;; translated well to emacs lisp.
(cl-labels ((char-after-line (offset)
              (let ((col (- (point) (line-beginning-position))))
                (save-excursion
                  (forward-line offset)
                  (goto-char (+ (point) col))
                  (if (= (point) (point-min)) 0 (char-after))))))
  (with-temp-buffer
    (insert-file-contents-literally "input/day7")
    (goto-char (point-min))
    (let ((count 0))
      (while (not (= (point) (point-max)))
        (cond ((and (= (char-after) ?.)
                    (or (= (char-after-line -1) ?|)
                        (= (char-after-line -1) ?S)))
               (delete-char 1)
               (insert ?|))
              ((and (= (char-after) ?^)
                    (= (char-after-line -1) ?|))
               (delete-char -1)
               (insert ?|)
               (forward-char)
               (delete-char 1)
               (insert ?|)
               (setq count (1+ count)))
              (t (forward-char))))
      count)))

;; Day 7 - Part 2
;; --------------
;; Timelines are treated instead like a beam-strength text-property. When timelines "merge",
;; their beam-strengths are added up.
;; It took me longer than I would like to admit to realize the off-by-one-error I ran into was
;; because we're now tracking "timelines", not splits, so we must start with 1 and not 0.
;; I think the solution ultimately turned out fine, but the code could use a little polishing,
;; adding another function or two for updating the text properties so the actual logic is free
;; from it, which would add a lot of readability.
;; NOTE: This could also work well as a general solution for both parts if I'd track the
;; "non-quantum" count seperately.
(cl-labels ((beam-at (offset)
              (let ((col (- (point) (line-beginning-position))))
                (save-excursion
                  (forward-line offset)
                  (goto-char (+ (point) col))
                  (if (= (point) (point-min)) ""
                    (buffer-substring (point) (1+ (point))))))))
  (with-temp-buffer
    (insert-file-contents-literally "input/day7")
    (goto-char (point-min))
    (let ((count 1))
      (while (not (= (point) (point-max)))
        (let ((current (beam-at 0))
              (last (beam-at -1)))
          (cond ((and (string= current ".")
                      (string-match (rx (or "S" "|")) last))
                 (delete-char 1)
                 (insert (propertize "|" :num (or (get-text-property 0 :num (match-string 0 last)) 1))))
                ((and (string= current "^")
                      (string-match (rx (or "S" "|")) last))
                 (let* ((beam-num (get-text-property 0 :num (match-string 0 last)))
                        (beam (propertize "|" :num beam-num)))
                   (if (= (char-before) ?|)
                       (let ((old-num (get-text-property (1- (point)) :num)))
                         (set-text-properties (1- (point)) (point)
                                              (list :num (+ beam-num old-num))))
                     (delete-char -1)
                     (insert beam))
                   (forward-char)
                   (delete-char 1)
                   (let ((above (beam-at -1)))
                     (when (string= above "|")
                       (let ((above-num (get-text-property 0 :num above)))
                         (setq beam (propertize "|" :num (+ beam-num above-num))))))
                   (insert beam)
                   (setq count (+ count beam-num))))
                (t (forward-char)))))
      count)))

;;; solutions.el ends here
