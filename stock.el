;;; stock.el --- A-stocks real-time data  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kinneyzhang

;; Author：Kinney Zhang <kinneyzhang666@gmail.com>
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

;; Stock is a plug-in based on api of Sina that creates a dashboard displaying real-time data of a-share indexs and stocks.
;; Thanks for the super-fast Sina api, and stock performs so well to update data automatically.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'url)
(require 'stock-utils)


(defvar url-http-response-status 0)


;;;; Customization

(defgroup stock nil
  "Settings for `stock'."
  :prefix "stock-"
  :group 'utils)


(defcustom stock-index-list '("sh000001" "sz399001" "sz399006")
  "List of composite index."
  :group 'stock
  :type 'list)


(defcustom stock-code-list '("sh600036" "sz000625")
  "List of stocks."
  :group 'stock
  :type 'list)


(defcustom stock-buffer-name "*A Chive*"
  "Buffer name of stock board."
  :group 'stock
  :type 'string)

(defcustom stock-search-buffer-name "*A Chive - results -*"
  "Buffer name of stock search board."
  :group 'stock
  :type 'string)


(defcustom stock-auto-refresh t
  "Whether to refresh automatically."
  :group 'stock
  :type 'boolean)


(defcustom stock-refresh-seconds 5
  "Seconds of automatic refresh time."
  :group 'stock
  :type 'integer)


(defcustom stock-cache-path (concat user-emacs-directory ".stock")
  "Path of cache."
  :group 'stock
  :type 'string)

(defcustom stock-colouring t
  "Whether to apply face.
If it's nil will be low-key, you can peek at it at company time."
  :group 'stock
  :type 'string)


(defcustom stock-modeline-stocks nil
  "List of stock codes to display in mode-line.
Example: \\='(\"sh600036\" \"sz000625\")."
  :group 'stock
  :type '(repeat string))


(defcustom stock-modeline-refresh-seconds 1
  "Seconds between mode-line refresh."
  :group 'stock
  :type 'integer)


(defcustom stock-modeline-format "[%n:%p]"
  "Format string for each stock in mode-line.
%n - stock name, %p - change percent."
  :group 'stock
  :type 'string)


(defcustom stock-headerline-stocks nil
  "List of stock codes to display in header-line.
Example: \\='(\"sh600036\" \"sz000625\")."
  :group 'stock
  :type '(repeat string))


(defcustom stock-headerline-refresh-seconds 1
  "Seconds between header-line refresh."
  :group 'stock
  :type 'integer)


(defcustom stock-headerline-format "[%n:%p]"
  "Format string for each stock in header-line.
%n - stock name, %p - change percent."
  :group 'stock
  :type 'string)

;;;;; faces

(defface stock-face-up
  '((t (:inherit (error))))
  "Face used when share prices are rising."
  :group 'stock)


(defface stock-face-down
  '((t :inherit (success)))
  "Face used when share prices are dropping."
  :group 'stock)


(defface stock-face-constant
  '((t :inherit (shadow)))
  "Face used when share prices are dropping."
  :group 'stock)


(defface stock-face-index-name
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face used for index name."
  :group 'stock)

;;;; constants

(defconst stock-api "https://hq.sinajs.cn"
  "Stocks Api.")


(defconst stock-field-index-list
  '((code . 0) (name . stock-make-name) (price . 4) (change-percent . stock-make-change-percent)
    (high . 5) (low . 6) (volume . stock-make-volume) (turn-volume . stock-make-turn-volume) (open . 2) (yestclose . 3))
  "Index or fucntion of each piece of data.")


(defconst stock-visual-columns (vector
                                 '("股票代码" 8 nil)
                                 '("名称" 10 nil)
                                 (list "当前价" 10 (stock-number-sort 2))
                                 (list "涨跌幅" 7 (stock-number-sort 3))
                                 (list "最高价" 10 (stock-number-sort 4))
                                 (list "最低价" 10 (stock-number-sort 5))
                                 (list "成交量" 10 (stock-number-sort 6))
                                 (list "成交额" 10 (stock-number-sort 7))
                                 (list "开盘价" 10 (stock-number-sort 8))
                                 (list "昨日收盘价" 10 (stock-number-sort 9)))
  "Realtime board columns.")

;;;;; variables

(defvar stock-prev-point nil
  "Point of before render.")


(defvar stock-search-codes nil
  "Search code list.")


(defvar stock-stocks nil
  "Realtime stocks code list.")


(defvar stock-pop-to-buffer-action nil
  "Action to use internally when `pop-to-buffer' is called.")


(defvar stock-entry-list nil
  "Cache data for manual render.")


(defvar stock-modeline-string ""
  "String to display in mode-line.")


(defvar stock-modeline-timer nil
  "Timer for mode-line refresh.")


(defvar stock-modeline-data nil
  "Cached stock data for mode-line display.")


(defvar stock-headerline-string ""
  "String to display in header-line.")


(defvar stock-headerline-timer nil
  "Timer for header-line refresh.")


(defvar stock-headerline-data nil
  "Cached stock data for header-line display.")

;;;;; functions

(defun stock-make-request-url (api parameter)
  "Make sina request url.
API: shares api.
PARAMETER: request url parameter."
  (format "%s/list=%s" api (string-join parameter ",")))


(defun stock-request (url callback)
  "Handle request by URL.
CALLBACK: function of after response."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/javascript;charset=UTF-8") ("Referer" . "https://finance.sina.com.cn"))))
    (url-retrieve url (lambda (_status)
                        (let ((inhibit-message t))
                          (message "stock: %s at %s" "The request is successful." (format-time-string "%T")))
                        (funcall callback)) nil 'silent)))


(defun stock-parse-response ()
  "Parse sina http response result by body."
  (if (/= 200 url-http-response-status)
      (error "Internal Server Error"))
  (let ((resp-gbcode (with-current-buffer (current-buffer)
                       (buffer-substring-no-properties (search-forward "\n\n") (point-max)))))
    (decode-coding-string resp-gbcode 'gb18030)))


(defun stock-format-content (codes resp-str)
  "Format response string to buffer string.
RESP-STR: string of response body.
CODES: stocks list of request parameters.
Return index and stocks data."
  (let ((str-list (cl-loop with i = 0
                           for it in codes
                           if (string-match (format "%s=\"\\([^\"]+\\)\"" it) resp-str)
                           collect (format "%s,%s" (nth i codes) (match-string 1 resp-str))
                           else
                           collect (nth i codes) end
                           do (cl-incf i))))
    (cl-loop for it in str-list
             with temp = nil
             do (setq temp (stock-format-row it))
             collect (list (nth 0 temp)
                           (apply 'vector temp)))))


(defun stock-format-row (row-str)
  "Format row content.
ROW-STR: string of row."
  (let ((value-list (split-string row-str ",")))
    (if (length= value-list 1)
        (append value-list (make-list 9 "-"))
      (cl-loop for (_k . v) in stock-field-index-list
               collect (if (functionp v)
                           (funcall v value-list stock-field-index-list)
                         (nth v value-list))))))


(defun stock-validate-request (codes callback)
  "Validate that the CODES is valid, then call CALLBACK function."
  (stock-request (stock-make-request-url stock-api codes)
                  (lambda ()
                    (funcall callback (seq-filter
                                       #'stock-valid-entry-p
                                       (stock-format-content codes (stock-parse-response)))))))


(defun stock-render-request (buffer-name codes &optional callback)
  "Handle request by stock CODES, and render buffer of BUFFER-NAME.
CALLBACK: callback function after the rendering."
  (stock-request (stock-make-request-url stock-api codes)
                  (lambda ()
                    (setq stock-entry-list (stock-format-content codes
                                                                   (stock-parse-response)))
                    (stock-render buffer-name)
                    
                    (if (functionp callback)
                        (funcall callback stock-entry-list)))))


(defun stock-render (buffer-name &optional manual)
  "Render visual buffer of BUFFER-NAME.
If MANUAL is t and `stock-colouring' is nil,
entry will remove face before render."
  (let ((entries (if stock-colouring
                     (mapcar #'stock-propertize-entry-face
                             stock-entry-list)
                   stock-entry-list)))

    (if (and manual (not stock-colouring))
        (setq entries (mapcar #'stock-remove-entry-face
                              stock-entry-list)))
    
    (with-current-buffer buffer-name
      (setq tabulated-list-entries entries)
      (tabulated-list-print t t))))


(defun stock-refresh ()
  "Referer stock visual buffer or stock search visual buffer."
  (if (get-buffer-window stock-buffer-name)
      (stock-render-request stock-buffer-name (append stock-index-list stock-stocks)))
  (if (get-buffer-window stock-search-buffer-name)
      (stock-render-request stock-search-buffer-name stock-search-codes)))


(defun stock-timer-alive-p ()
  "Check that the timer is alive."
  (get-buffer stock-buffer-name))


(defun stock-switch-visual (buffer-name)
  "Switch to visual buffer by BUFFER-NAME."
  (pop-to-buffer buffer-name stock-pop-to-buffer-action)
  (stock-visual-mode))


(defun stock-loop-refresh (_timer)
  "Loop to refresh."
  (if (and (stock-timer-alive-p) (stock-weekday-p))
      (if (stock-working-time-p stock-buffer-name)
          (stock-render-request stock-buffer-name
                                 (append stock-index-list stock-stocks)
                                 (lambda (_resp)
                                   (stock-handle-auto-refresh)))
        (stock-handle-auto-refresh))))


(defun stock-handle-auto-refresh ()
  "Automatic refresh."
  (stock-set-timeout #'stock-loop-refresh
                      stock-refresh-seconds))


(defun stock-init ()
  "Init program. Read cache codes from file."
  (let ((cache (stock-readcache stock-cache-path)))
    (unless cache
      (stock-writecache stock-cache-path stock-code-list)
      (setq cache stock-code-list))
    (setq stock-stocks cache)))


(defun stock-propertize-entry-face (entry)
  "Propertize ENTRY."
  (let* ((id (car entry))
         (data (cadr entry))
         (percent (aref data 3))
         (percent-number (string-to-number percent)))

    (when (cl-position id stock-index-list :test 'string=)
      (aset data 0 (propertize (aref data 0) 'face 'stock-face-index-name))
      (aset data 1 (propertize (aref data 1) 'face 'stock-face-index-name)))

    (aset data 3 (propertize percent 'face (cond
                                            ((> percent-number 0)
                                             'stock-face-up)
                                            ((< percent-number 0)
                                             'stock-face-down)
                                            (t 'stock-face-constant))))
    entry))


(defun stock-remove-entry-face (entry)
  "Remove ENTRY properties."
  (let* ((id (car entry))
         (data (cadr entry)))
    (when (cl-position id stock-index-list :test 'string=)
      (stock-remove-face (aref data 0))
      (stock-remove-face (aref data 1)))

    (stock-remove-face (aref data 3))
    entry))

;;;;; interactive

;;;###autoload
(defun stock ()
  "Launch stock and switch to visual buffer."
  (interactive)
  (stock-init)

  (let ((timer-alive (stock-timer-alive-p)))

    (stock-switch-visual stock-buffer-name)
    (stock-render-request stock-buffer-name
                           (append stock-index-list stock-stocks)
                           (lambda (_resp)
                             (if (and stock-auto-refresh (not timer-alive))
                                 (stock-handle-auto-refresh))))))


;;;###autoload
;; (defun stock-exit ()
;;   "Exit stock."
;;   (interactive)
;;   (quit-window t)
;;   (message "Stock has been killed."))


;;;###autoload
(defun stock-search (codes)
  "Search stock by codes.
CODES: string of stocks list."
  (interactive "sPlease input code to search: ")
  (setq stock-search-codes (split-string codes))
  (stock-switch-visual stock-search-buffer-name)
  (stock-render-request stock-search-buffer-name stock-search-codes))


;;;###autoload
(defun stock-add (codes)
  "Add stocks by codes.
CODES: string of stocks list."
  (interactive "sPlease input code to add: ")
  (setq codes (split-string codes))
  (stock-validate-request
   codes
   (lambda (resp)
     (setq codes (mapcar #'car resp))
     
     (when codes
       (setq stock-stocks (append stock-stocks codes))
       (stock-writecache stock-cache-path stock-stocks)
       (stock-render-request stock-buffer-name
                              (append stock-index-list stock-stocks)
                              (lambda (_resp)
                                (message "[%s] have been added."
                                         (mapconcat 'identity codes ", "))))))))


;;;###autoload
(defun stock-remove ()
  "Remove stocks."
  (interactive)
  (let* ((code (completing-read "Please select the stock code to remove: "
                                stock-stocks
                                nil
                                t
                                nil
                                nil
                                nil))
         (index (cl-position code stock-stocks :test 'string=)))
    (when index
      (setq stock-stocks (stock-remove-nth-element stock-stocks index))
      (stock-writecache stock-cache-path stock-stocks)
      (stock-render-request stock-buffer-name (append stock-index-list stock-stocks)
                             (lambda (_resp)
                               (message "<%s> have been removed." code))))))


;;;###autoload
(defun stock-switch-colouring ()
  "Manual switch colouring. It's handy for emergencies."
  (interactive)
  (setq stock-colouring (not stock-colouring))
  (stock-render (buffer-name) t))

;;;;; mode

(defvar stock-visual-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" 'stock-add)
    (define-key map "_" 'stock-remove)
    (define-key map "c" 'stock-switch-colouring)
    map)
  "Keymap for `stock-visual-mode'.")


(define-derived-mode stock-visual-mode tabulated-list-mode "Stock"
  "Major mode for avhice real-time board."
  (setq tabulated-list-format stock-visual-columns)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'stock-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))


;;;;; mode-line and header-line common functions

(defun stock-format-stock-string (format-str name percent)
  "Format stock string by replacing %n with NAME and %p with PERCENT in FORMAT-STR."
  (let ((result format-str))
    (setq result (replace-regexp-in-string "%n" name result t t))
    (setq result (replace-regexp-in-string "%p" percent result t t))
    result))


(defun stock--format-stock-entry (entry format-str)
  "Format a single stock ENTRY using FORMAT-STR for display."
  (let* ((data (cadr entry))
         (name (aref data 1))
         (percent (aref data 3))
         (percent-number (string-to-number percent))
         (face (cond
                ((> percent-number 0) 'stock-face-up)
                ((< percent-number 0) 'stock-face-down)
                (t 'stock-face-constant))))
    (propertize (stock-format-stock-string format-str name percent)
                'face face)))


;;;;; mode-line functions

(defun stock-modeline-format-stock (entry)
  "Format a single stock ENTRY for mode-line display."
  (stock--format-stock-entry entry stock-modeline-format))


(defun stock-modeline-update-string (entries)
  "Update `stock-modeline-string' with stock ENTRIES data."
  (setq stock-modeline-data entries)
  (setq stock-modeline-string
        (if entries
            (mapconcat #'stock-modeline-format-stock entries " ")
          ""))
  (force-mode-line-update t))


(defun stock-modeline-fetch ()
  "Fetch stock data for mode-line display."
  (when stock-modeline-stocks
    (stock-request
     (stock-make-request-url stock-api stock-modeline-stocks)
     (lambda ()
       (let ((entries (seq-filter
                       #'stock-valid-entry-p
                       (stock-format-content stock-modeline-stocks
                                             (stock-parse-response)))))
         (stock-modeline-update-string entries))))))


(defun stock-modeline-loop-refresh (_timer)
  "Loop to refresh mode-line stock data."
  (when stock-modeline-timer
    (if (and (stock-weekday-p)
             (stock-trading-time-p))
        (stock-modeline-fetch))
    (stock-modeline-handle-refresh)))


(defun stock-modeline-handle-refresh ()
  "Schedule next mode-line refresh."
  (when stock-modeline-timer
    (stock-set-timeout #'stock-modeline-loop-refresh
                        stock-modeline-refresh-seconds)))


;;;###autoload
(defun stock-modeline-mode (&optional arg)
  "Toggle stock display in mode-line.
With prefix ARG, enable if ARG is positive, disable otherwise."
  (interactive "P")
  (let ((enable (if arg
                    (> (prefix-numeric-value arg) 0)
                  (not stock-modeline-timer))))
    (if enable
        (progn
          (unless stock-modeline-stocks
            (setq stock-modeline-stocks stock-code-list))
          (unless (member '(:eval stock-modeline-string) global-mode-string)
            (if global-mode-string
                (push '(:eval stock-modeline-string) global-mode-string)
              (setq global-mode-string '("" (:eval stock-modeline-string)))))
          (setq stock-modeline-timer t)
          (stock-modeline-fetch)
          (stock-modeline-handle-refresh)
          (message "Stock mode-line enabled."))
      (setq stock-modeline-timer nil)
      (setq stock-modeline-string "")
      (setq global-mode-string
            (delete '(:eval stock-modeline-string) global-mode-string))
      (force-mode-line-update t)
      (message "Stock mode-line disabled."))))


;;;###autoload
(defun stock-modeline-add (codes)
  "Add stocks to mode-line display by CODES."
  (interactive "sPlease input stock codes to add to mode-line: ")
  (let ((code-list (split-string codes)))
    (stock-validate-request
     code-list
     (lambda (resp)
       (let ((valid-codes (mapcar #'car resp)))
         (when valid-codes
           (setq stock-modeline-stocks
                 (delete-dups (append stock-modeline-stocks valid-codes)))
           (stock-modeline-fetch)
           (message "[%s] added to mode-line."
                    (mapconcat 'identity valid-codes ", "))))))))


;;;###autoload
(defun stock-modeline-remove ()
  "Remove a stock from mode-line display."
  (interactive)
  (if (null stock-modeline-stocks)
      (message "No stocks in mode-line.")
    (let* ((code (completing-read "Select stock to remove from mode-line: "
                                  stock-modeline-stocks nil t))
           (index (cl-position code stock-modeline-stocks :test 'string=)))
      (when index
        (setq stock-modeline-stocks
              (stock-remove-nth-element stock-modeline-stocks index))
        (stock-modeline-fetch)
        (message "<%s> removed from mode-line." code)))))


;;;;; header-line functions

(defun stock-headerline-format-stock (entry)
  "Format a single stock ENTRY for header-line display."
  (stock--format-stock-entry entry stock-headerline-format))


(defun stock-headerline-update-string (entries)
  "Update `stock-headerline-string' with stock ENTRIES data."
  (setq stock-headerline-data entries)
  (setq stock-headerline-string
        (if entries
            (mapconcat #'stock-headerline-format-stock entries " ")
          ""))
  (force-mode-line-update t))


(defun stock-headerline-fetch ()
  "Fetch stock data for header-line display."
  (when stock-headerline-stocks
    (stock-request
     (stock-make-request-url stock-api stock-headerline-stocks)
     (lambda ()
       (let ((entries (seq-filter
                       #'stock-valid-entry-p
                       (stock-format-content stock-headerline-stocks
                                             (stock-parse-response)))))
         (stock-headerline-update-string entries))))))


(defun stock-headerline-loop-refresh (_timer)
  "Loop to refresh header-line stock data."
  (when stock-headerline-timer
    (if (and (stock-weekday-p)
             (stock-trading-time-p))
        (stock-headerline-fetch))
    (stock-headerline-handle-refresh)))


(defun stock-headerline-handle-refresh ()
  "Schedule next header-line refresh."
  (when stock-headerline-timer
    (stock-set-timeout #'stock-headerline-loop-refresh
                        stock-headerline-refresh-seconds)))


;;;###autoload
(defun stock-headerline-mode (&optional arg)
  "Toggle stock display in header-line.
With prefix ARG, enable if ARG is positive, disable otherwise."
  (interactive "P")
  (let ((enable (if arg
                    (> (prefix-numeric-value arg) 0)
                  (not stock-headerline-timer))))
    (if enable
        (progn
          (unless stock-headerline-stocks
            (setq stock-headerline-stocks stock-code-list))
          (setq-default header-line-format
                        '(:eval stock-headerline-string))
          (setq stock-headerline-timer t)
          (stock-headerline-fetch)
          (stock-headerline-handle-refresh)
          (message "Stock header-line enabled."))
      (setq stock-headerline-timer nil)
      (setq stock-headerline-string "")
      (setq-default header-line-format nil)
      (force-mode-line-update t)
      (message "Stock header-line disabled."))))


;;;###autoload
(defun stock-headerline-add (codes)
  "Add stocks to header-line display by CODES."
  (interactive "sPlease input stock codes to add to header-line: ")
  (let ((code-list (split-string codes)))
    (stock-validate-request
     code-list
     (lambda (resp)
       (let ((valid-codes (mapcar #'car resp)))
         (when valid-codes
           (setq stock-headerline-stocks
                 (delete-dups (append stock-headerline-stocks valid-codes)))
           (stock-headerline-fetch)
           (message "[%s] added to header-line."
                    (mapconcat 'identity valid-codes ", "))))))))


;;;###autoload
(defun stock-headerline-remove ()
  "Remove a stock from header-line display."
  (interactive)
  (if (null stock-headerline-stocks)
      (message "No stocks in header-line.")
    (let* ((code (completing-read "Select stock to remove from header-line: "
                                  stock-headerline-stocks nil t))
           (index (cl-position code stock-headerline-stocks :test 'string=)))
      (when index
        (setq stock-headerline-stocks
              (stock-remove-nth-element stock-headerline-stocks index))
        (stock-headerline-fetch)
        (message "<%s> removed from header-line." code)))))

(provide 'stock)

;;; stock.el ends here

;; 0：”大秦铁路”，股票名字；
;; 1：”27.55″，今日开盘价；
;; 2：”27.25″，昨日收盘价；
;; 3：”26.91″，当前价格；
;; 4：”27.55″，今日最高价；
;; 5：”26.20″，今日最低价；
;; 6：”26.91″，竞买价，即“买一”报价；
;; 7：”26.92″，竞卖价，即“卖一”报价；
;; 8：”22114263″，成交的股票数，由于股票交易以一百股为基本单位，所以在使用时，通常把该值除以一百；
;; 9：”589824680″，成交金额，单位为“元”，为了一目了然，通常以“万元”为成交金额的单位，所以通常把该值除以一万；
;; 10：”4695″，“买一”申请4695股，即47手；
;; 11：”26.91″，“买一”报价；
;; 12：”57590″，“买二”
;; 13：”26.90″，“买二”
;; 14：”14700″，“买三”
;; 15：”26.89″，“买三”
;; 16：”14300″，“买四”
;; 17：”26.88″，“买四”
;; 18：”15100″，“买五”
;; 19：”26.87″，“买五”
;; 20：”3100″，“卖一”申报3100股，即31手；
;; 21：”26.92″，“卖一”报价
;; (22, 23), (24, 25), (26,27), (28, 29)分别为“卖二”至“卖四的情况”
;; 30：”2008-01-11″，日期；
;; 31：”15:05:32″，时间；

;; var hq_str_sh000001=\"上证指数,3261.9219,3268.6955,3245.3123,3262.0025,3216.9927,0,0,319906033,409976276121,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2023-03-14,15:30:39,00,\"

;; ("sh000001" "上证指数" "3245.3123" "-0.72%" "3262.0025" "3216.9927" 3199060 "40997627W" "3261.9219" "3268.6955")

;; var hq_str_sh603866=\"桃李面包,0.000,15.250,15.250,0.000,0.000,0.000,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,0,0.000,2023-03-27,09:11:30,00,\"

;; var hq_str_sh603866=\"桃李面包,0.000,15.250,15.250,0.000,0.000,15.110,15.110,0,0.000,5100,15.110,2000,0.000,0,0.000,0,0.000,0,0.000,5100,15.110,0,0.000,0,0.000,0,0.000,0,0.000,2023-03-27,09:16:00,00,\"

;; ("sh603866" ["sh603866" "桃李面包" "15.250" "0.00%" "0.000" "0.000" "0" "0W" "0.000" "15.250"])

;; http://image.sinajs.cn/newchart/daily/n/sh601006.gif
