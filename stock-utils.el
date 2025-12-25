;;; stock-utils.el --- Utility functions for stock.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kinneyzhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/stock
;; Original Author: zakudriver <zy.hua1122@gmail.com>
;; Original URL: https://github.com/zakudriver/achive
;; Version: 1.0
;; Package-Requires: ((emacs "25.2"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides utility functions for stock.el, including:
;; - Plist-based stock data structure accessors and constructors
;; - Time and date utilities for trading hours detection
;; - Cache file read/write operations
;; - Data formatting and conversion utilities

;;; Code:

(require 'cl-lib)

;;;; Plist Data Structure

;; Stock data is represented as a plist with the following keys:
;; :code         - Stock code (e.g., "sh600036")
;; :name         - Stock name (e.g., "招商银行")
;; :price        - Current price
;; :change-percent - Change percentage (e.g., "+1.23%")
;; :high         - Today's highest price
;; :low          - Today's lowest price
;; :volume       - Trading volume (in lots, 1 lot = 100 shares)
;; :turnover     - Turnover amount (in 10000 yuan, unit: W)
;; :open         - Opening price
;; :yestclose    - Yesterday's closing price

(defun stock-plist-get (stock key)
  "Get value of KEY from STOCK plist.
STOCK is a plist containing stock data.
KEY is the property name (e.g., :code, :name, :price)."
  (plist-get stock key))

(defun stock-plist-put (stock key value)
  "Set VALUE for KEY in STOCK plist.
Returns a new plist with the updated value.
STOCK is a plist containing stock data.
KEY is the property name.
VALUE is the new value to set."
  (plist-put (copy-sequence stock) key value))

(defun stock-make-plist (code name price change-percent high low volume turnover open yestclose)
  "Create a stock plist from individual field values.
CODE is the stock code.
NAME is the stock name.
PRICE is the current price.
CHANGE-PERCENT is the change percentage string.
HIGH is today's highest price.
LOW is today's lowest price.
VOLUME is the trading volume.
TURNOVER is the turnover amount.
OPEN is the opening price.
YESTCLOSE is yesterday's closing price."
  (list :code code
        :name name
        :price price
        :change-percent change-percent
        :high high
        :low low
        :volume volume
        :turnover turnover
        :open open
        :yestclose yestclose))

(defun stock-plist-to-vector (stock)
  "Convert STOCK plist to vector for tabulated-list display.
Returns a vector with fields in display order:
[code name price change-percent high low volume turnover open yestclose]"
  (vector (plist-get stock :code)
          (plist-get stock :name)
          (plist-get stock :price)
          (plist-get stock :change-percent)
          (plist-get stock :high)
          (plist-get stock :low)
          (plist-get stock :volume)
          (plist-get stock :turnover)
          (plist-get stock :open)
          (plist-get stock :yestclose)))

(defun stock-plist-valid-p (stock)
  "Check if STOCK plist contains valid data.
Returns nil if the stock name is \"-\" (indicating invalid data)."
  (not (string= (plist-get stock :name) "-")))

;;;; Calculation Utilities

(defun stock-make-percent (price yestclose open)
  "Calculate percentage change from yesterday's close.
PRICE is the current price (number or string).
YESTCLOSE is yesterday's closing price (number or string).
OPEN is the opening price (number or string).
Returns a formatted string like \"+1.23%\" or \"-0.45%\".
Returns \"0.00%\" if OPEN is zero (market not opened)."
  (let ((price-num (if (numberp price) price (string-to-number price)))
        (yest-num (if (numberp yestclose) yestclose (string-to-number yestclose)))
        (open-num (if (numberp open) open (string-to-number open))))
    (if (zerop open-num)
        "0.00%"
      (let* ((price-f (float price-num))
             (yest-f (float yest-num))
             (result (/ (- price-f yest-f)
                        (if (zerop yest-f) 1.0 yest-f))))
        (format "%s%0.2f%%" (if (> result 0) "+" "") (* result 100))))))

(defun stock-format-volume (volume-str)
  "Format VOLUME-STR (raw volume) to display format (in lots).
Divides by 100 since 1 lot = 100 shares."
  (number-to-string (/ (string-to-number volume-str) 100)))

(defun stock-format-turnover (turnover-str)
  "Format TURNOVER-STR (raw turnover) to display format.
Divides by 10000 and appends 'W' suffix (万元)."
  (format "%dW" (/ (string-to-number turnover-str) 10000)))

;;;; Timer Utilities

(defmacro stock-set-timeout (callback seconds)
  "Schedule CALLBACK to run after SECONDS seconds.
Similar to JavaScript's setTimeout.
CALLBACK should be a function that accepts one argument (the timer)."
  `(let ((timer))
     (setq timer (run-with-timer ,seconds nil
                                 (lambda ()
                                   (cancel-timer timer)
                                   (funcall ,callback timer))))))

;;;; Time Utilities

(defun stock--time-list-index (word)
  "Get index of WORD in decoded time list.
WORD should be one of: seconds, minutes, hour, day, month, year, dow, dst, zone."
  (let ((words '("seconds" "minutes" "hour" "day" "month" "year" "dow" "dst" "zone")))
    (cl-position word words :test 'equal)))

(defun stock--decoded-time-get (time word)
  "Get component WORD from decoded TIME.
Compatible with Emacs versions before 27.1 which lack decoded-time-* functions."
  (nth (stock--time-list-index word) time))

(defun stock--hhmm-to-number (str)
  "Convert time string STR like \"12:00\" to number 1200."
  (if (stringp str)
      (string-to-number (replace-regexp-in-string ":" "" str))
    0))

(defun stock--hhmm-to-time (hhmm)
  "Convert HHMM (string or number) to Emacs time value.
HHMM can be \"12:00\" or 1200."
  (let ((hhmm-num (if (stringp hhmm)
                      (stock--hhmm-to-number hhmm)
                    hhmm)))
    (let* ((now (decode-time))
           (time-code (list 0
                            (% hhmm-num 100)
                            (/ hhmm-num 100)
                            (stock--decoded-time-get now "day")
                            (stock--decoded-time-get now "month")
                            (stock--decoded-time-get now "year")
                            (stock--decoded-time-get now "zone"))))
      (apply #'encode-time time-code))))

(defun stock--time-before-p (hhmm)
  "Return t if current time is before HHMM.
HHMM is a time string like \"9:00\"."
  (time-less-p (current-time) (stock--hhmm-to-time hhmm)))

(defun stock-trading-time-p ()
  "Check if current time is within A-share trading hours.
Trading hours are 9:00-11:30 (morning session) and 13:00-15:00 (afternoon session)."
  (or (and (not (stock--time-before-p "9:00"))
           (stock--time-before-p "11:30"))
      (and (not (stock--time-before-p "13:00"))
           (stock--time-before-p "15:00"))))

(defun stock-weekday-p ()
  "Return t if today is a weekday (Monday to Friday).
Note: This does not account for holidays."
  (let ((dow (format-time-string "%w")))
    (not (or (string= dow "0") (string= dow "6")))))

(defun stock-working-time-p (buffer-name)
  "Check if it's trading time and BUFFER-NAME window is visible.
Returns t only when both conditions are met."
  (and (get-buffer-window buffer-name)
       (stock-trading-time-p)))

;;;; Cache Utilities

(defun stock-read-cache (path)
  "Read stock codes list from cache file at PATH.
Returns nil if file doesn't exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (read (current-buffer)))))

(defun stock-write-cache (path codes)
  "Write stock CODES list to cache file at PATH."
  (with-temp-file path
    (prin1 codes (current-buffer))))

;;;; List Utilities

(defun stock-remove-nth-element (list index)
  "Remove element at INDEX from LIST.
Returns nil if INDEX is out of bounds."
  (when (< index (length list))
    (if (zerop index)
        (cdr list)
      (let ((prev (nthcdr (1- index) list)))
        (setcdr prev (cddr prev))
        list))))

;;;; Display Utilities

(defun stock-remove-text-properties (str)
  "Remove all text properties from STR.
Returns the plain string without any faces."
  (let ((end (length str)))
    (set-text-properties 0 end nil str)
    str))

;;;; Sorting Utilities

(defun stock-make-numeric-sorter (key)
  "Create a sort function for tabulated-list by KEY.
KEY is a plist key like :price, :change-percent.
Returns a comparison function for descending numeric sort."
  (lambda (a b)
    (let ((val-a (string-to-number
                  (plist-get (cadr a) key)))
          (val-b (string-to-number
                  (plist-get (cadr b) key))))
      (> val-a val-b))))

;; Legacy compatibility - keeping old function name for backward compatibility
(defalias 'stock-readcache 'stock-read-cache)
(defalias 'stock-writecache 'stock-write-cache)

(provide 'stock-utils)

;;; stock-utils.el ends here

