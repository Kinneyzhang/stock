;;; stock.el --- A-share real-time stock data dashboard  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kinneyzhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/stock
;; Original Author: zakudriver <zy.hua1122@gmail.com>
;; Original URL: https://github.com/zakudriver/achive
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (tp "0.1.0"))
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

;; Stock is an Emacs package for displaying A-share (Chinese stock market)
;; real-time data.  It provides:
;;
;; - A dashboard buffer showing stock indices and individual stocks
;; - Mode-line and header-line display for quick price monitoring
;; - Auto-refresh during trading hours (9:00-11:30, 13:00-15:00)
;; - Stock search and favorites management
;;
;; Main entry points:
;; - `stock' - Open the main stock dashboard
;; - `stock-modeline-mode' - Toggle mode-line stock display
;; - `stock-headerline-mode' - Toggle header-line stock display
;;
;; Data is fetched from Sina Finance API.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'url)
(require 'stock-utils)
(require 'tp)

;; Declare external variable from url.el to avoid byte-compiler warning.
;; This variable holds the HTTP response status code after url-retrieve.
(defvar url-http-response-status)

;;;; Customization

(defgroup stock nil
  "A-share real-time stock data dashboard."
  :prefix "stock-"
  :group 'tools
  :link '(url-link :tag "GitHub" "https://github.com/Kinneyzhang/stock"))

(defcustom stock-index-list '("sh000001" "sz399001" "sz399006")
  "List of stock index codes to display.
Default includes Shanghai Composite, Shenzhen Component, and ChiNext.
Format: sh/sz + 6-digit code."
  :group 'stock
  :type '(repeat string))

(defcustom stock-code-list '("sh600036" "sz000625")
  "Default list of stock codes for new users.
These are used when no cache file exists."
  :group 'stock
  :type '(repeat string))

(defcustom stock-buffer-name "*A Chive*"
  "Buffer name for the main stock dashboard."
  :group 'stock
  :type 'string)

(defcustom stock-search-buffer-name "*A Chive - results -*"
  "Buffer name for stock search results."
  :group 'stock
  :type 'string)

(defcustom stock-auto-refresh t
  "Non-nil means auto-refresh stock data during trading hours."
  :group 'stock
  :type 'boolean)

(defcustom stock-refresh-seconds 5
  "Interval in seconds between auto-refresh cycles."
  :group 'stock
  :type 'integer)

(defcustom stock-cache-path (concat user-emacs-directory ".stock")
  "Path to the cache file storing favorite stock codes."
  :group 'stock
  :type 'file)

(defcustom stock-colouring t
  "Non-nil means apply color faces to price changes.
When nil, display is monochrome (useful for discretion at work)."
  :group 'stock
  :type 'boolean)

(defcustom stock-modeline-stocks nil
  "List of stock codes to display in mode-line.
Example: \\='(\"sh600036\" \"sz000625\")."
  :group 'stock
  :type '(repeat string))

(defcustom stock-modeline-refresh-seconds 1
  "Interval in seconds between mode-line refresh."
  :group 'stock
  :type 'integer)

(defcustom stock-modeline-format "[%n:%p]"
  "Format string for mode-line stock display.
%n is replaced with stock name, %p with change percent."
  :group 'stock
  :type 'string)

(defcustom stock-headerline-stocks nil
  "List of stock codes to display in header-line.
Example: \\='(\"sh600036\" \"sz000625\")."
  :group 'stock
  :type '(repeat string))

(defcustom stock-headerline-refresh-seconds 1
  "Interval in seconds between header-line refresh."
  :group 'stock
  :type 'integer)

(defcustom stock-headerline-format "[%n:%p]"
  "Format string for header-line stock display.
%n is replaced with stock name, %p with change percent."
  :group 'stock
  :type 'string)

;;;; Faces

(defface stock-face-up
  '((t (:inherit error)))
  "Face for rising stock prices (red in Chinese markets)."
  :group 'stock)

(defface stock-face-down
  '((t (:inherit success)))
  "Face for falling stock prices (green in Chinese markets)."
  :group 'stock)

(defface stock-face-constant
  '((t (:inherit shadow)))
  "Face for unchanged stock prices."
  :group 'stock)

(defface stock-face-index-name
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face for stock index names (e.g., Shanghai Composite)."
  :group 'stock)

;;;; Constants

(defconst stock-api-url "https://hq.sinajs.cn"
  "Sina Finance API base URL.")

;; Raw API response field indices (0-indexed from comma-separated values):
;; 0: name, 1: open, 2: yestclose, 3: price, 4: high, 5: low,
;; 6: bid, 7: ask, 8: volume, 9: turnover, ...
(defconst stock--field-specs
  '((:code       . 0)      ; Stock code (from request, not API)
    (:name       . nil)    ; Stock name (needs decoding)
    (:price      . 4)      ; Current price
    (:change-percent . nil) ; Calculated field
    (:high       . 5)      ; Today's high
    (:low        . 6)      ; Today's low
    (:volume     . nil)    ; Trading volume (calculated)
    (:turnover   . nil)    ; Turnover amount (calculated)
    (:open       . 2)      ; Opening price
    (:yestclose  . 3))     ; Yesterday's close
  "Mapping of plist keys to API response field indices.
nil values indicate calculated fields.")

;;;; Internal Variables

(defvar stock--search-codes nil
  "Current search query stock codes.")

(defvar stock--favorite-codes nil
  "User's favorite stock codes (persisted to cache).")

(defvar stock--entry-cache nil
  "Cached stock entries for display.
Each entry is (CODE . PLIST) where PLIST contains stock data.")

(defvar stock--pop-to-buffer-action nil
  "Custom action for `pop-to-buffer' when switching to stock buffer.")

;;;; tp.el Incremental Update Variables

(defvar stock--tp-layers nil
  "Alist mapping stock codes to their tp layer names.
Each element is (CODE . LAYER-SYMBOL).")

(defvar stock--tp-cell-data (make-hash-table :test 'equal)
  "Hash table storing cell data for tp.el incremental updates.
Keys are \"CODE:FIELD\" strings, values are the display values.")

;;;; Mode-line Variables

(defvar stock-modeline-string ""
  "Current mode-line display string.")

(defvar stock--modeline-timer nil
  "Timer object for mode-line refresh.")

(defvar stock--modeline-data nil
  "Cached stock data for mode-line.")

;;;; Header-line Variables

(defvar stock-headerline-string ""
  "Current header-line display string.")

(defvar stock--headerline-timer nil
  "Timer object for header-line refresh.")

(defvar stock--headerline-data nil
  "Cached stock data for header-line.")

;;;; Table Column Definition

(defun stock--make-plist-sorter (key)
  "Create a sort function for tabulated-list by plist KEY."
  (lambda (a b)
    (let ((val-a (string-to-number (plist-get (cadr a) key)))
          (val-b (string-to-number (plist-get (cadr b) key))))
      (> val-a val-b))))

(defconst stock--visual-columns
  (vector
   '("股票代码" 8 nil)
   '("名称" 10 nil)
   (list "当前价" 10 (stock--make-plist-sorter :price))
   (list "涨跌幅" 7 (stock--make-plist-sorter :change-percent))
   (list "最高价" 10 (stock--make-plist-sorter :high))
   (list "最低价" 10 (stock--make-plist-sorter :low))
   (list "成交量" 10 (stock--make-plist-sorter :volume))
   (list "成交额" 10 (stock--make-plist-sorter :turnover))
   (list "开盘价" 10 (stock--make-plist-sorter :open))
   (list "昨日收盘价" 10 (stock--make-plist-sorter :yestclose)))
  "Column definitions for the stock dashboard.
Each column is (NAME WIDTH SORT-FN).")

;;;; API Request Functions

(defun stock--make-request-url (codes)
  "Build API URL for stock CODES."
  (format "%s/list=%s" stock-api-url (string-join codes ",")))

(defun stock--request (url callback)
  "Make async HTTP request to URL, call CALLBACK on success."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/javascript;charset=UTF-8")
           ("Referer" . "https://finance.sina.com.cn"))))
    (url-retrieve url
                  (lambda (_status)
                    (let ((inhibit-message t))
                      (message "stock: request successful at %s"
                               (format-time-string "%T")))
                    (funcall callback))
                  nil 'silent)))

(defun stock--parse-response ()
  "Parse HTTP response body, return decoded string."
  (unless (= 200 url-http-response-status)
    (error "Stock API request failed with status %d" url-http-response-status))
  (let ((body (buffer-substring-no-properties
               (progn (goto-char (point-min))
                      (search-forward "\n\n")
                      (point))
               (point-max))))
    (decode-coding-string body 'gb18030)))

;;;; Data Parsing Functions

(defun stock--parse-raw-values (code resp-str)
  "Extract values for CODE from API response RESP-STR.
Returns list of comma-separated values or nil if not found."
  (when (string-match (format "%s=\"\\([^\"]+\\)\"" code) resp-str)
    (split-string (match-string 1 resp-str) ",")))

(defun stock--raw-to-plist (code values)
  "Convert raw VALUES list to plist for stock CODE.
VALUES is the comma-separated data from API."
  (if (or (null values) (< (length values) 10))
      ;; Invalid data - return placeholder
      (list :code code
            :name "-"
            :price "-"
            :change-percent "-"
            :high "-"
            :low "-"
            :volume "-"
            :turnover "-"
            :open "-"
            :yestclose "-")
    ;; Parse actual data
    (let* ((name (decode-coding-string (nth 0 values) 'gb18030))
           (open (nth 1 values))
           (yestclose (nth 2 values))
           (price (nth 3 values))
           (high (nth 4 values))
           (low (nth 5 values))
           (volume-raw (nth 8 values))
           (turnover-raw (nth 9 values))
           (change-percent (stock-make-percent price yestclose open))
           (volume (stock-format-volume volume-raw))
           (turnover (stock-format-turnover turnover-raw)))
      (list :code code
            :name name
            :price price
            :change-percent change-percent
            :high high
            :low low
            :volume volume
            :turnover turnover
            :open open
            :yestclose yestclose))))

(defun stock--parse-response-to-entries (codes resp-str)
  "Parse response RESP-STR for CODES into entry list.
Returns list of (CODE . PLIST) entries."
  (mapcar (lambda (code)
            (let* ((values (stock--parse-raw-values code resp-str))
                   (plist (stock--raw-to-plist code values)))
              (cons code plist)))
          codes))

(defun stock--entry-valid-p (entry)
  "Check if ENTRY contains valid stock data."
  (not (string= (plist-get (cdr entry) :name) "-")))

;;;; Display Conversion Functions

(defun stock--plist-to-vector (plist)
  "Convert stock PLIST to vector for tabulated-list display."
  (vector (plist-get plist :code)
          (plist-get plist :name)
          (plist-get plist :price)
          (plist-get plist :change-percent)
          (plist-get plist :high)
          (plist-get plist :low)
          (plist-get plist :volume)
          (plist-get plist :turnover)
          (plist-get plist :open)
          (plist-get plist :yestclose)))

(defun stock--entry-to-tabulated (entry)
  "Convert ENTRY (CODE . PLIST) to tabulated-list format.
Returns (ID [COLUMN-VALUES...] PLIST) for sorting access."
  (let ((code (car entry))
        (plist (cdr entry)))
    (list code (stock--plist-to-vector plist) plist)))

;;;; Face Application Functions

(defun stock--get-change-face (percent-str)
  "Return face for change PERCENT-STR."
  (let ((num (string-to-number percent-str)))
    (cond ((> num 0) 'stock-face-up)
          ((< num 0) 'stock-face-down)
          (t 'stock-face-constant))))

(defun stock--propertize-entry (entry)
  "Apply faces to tabulated ENTRY based on price change.
ENTRY is (ID VECTOR PLIST) format."
  (let* ((id (car entry))
         (vec (cadr entry))
         (plist (caddr entry))
         (percent (plist-get plist :change-percent))
         (is-index (member id stock-index-list)))
    ;; Apply index face to code and name
    (when is-index
      (aset vec 0 (propertize (aref vec 0) 'face 'stock-face-index-name))
      (aset vec 1 (propertize (aref vec 1) 'face 'stock-face-index-name)))
    ;; Apply change face to percent
    (aset vec 3 (propertize (aref vec 3) 'face (stock--get-change-face percent)))
    entry))

(defun stock--remove-entry-faces (entry)
  "Remove all faces from tabulated ENTRY."
  (let* ((id (car entry))
         (vec (cadr entry))
         (is-index (member id stock-index-list)))
    (when is-index
      (stock-remove-text-properties (aref vec 0))
      (stock-remove-text-properties (aref vec 1)))
    (stock-remove-text-properties (aref vec 3))
    entry))

;;;; tp.el Incremental Update Functions

(defun stock--tp-cell-key (code field)
  "Generate a unique key for CODE and FIELD combination."
  (format "%s:%s" code field))

(defun stock--tp-cell-var (code field)
  "Generate a variable symbol for CODE and FIELD combination."
  (intern (format "stock--cell-%s-%s" code field)))

(defun stock--tp-layer-name (code field)
  "Generate a layer name for CODE and FIELD combination."
  (intern (format "stock-layer-%s-%s" code field)))

(defun stock--tp-define-cell-layer (code field)
  "Define a tp layer for stock CODE and FIELD with reactive variable.
The layer uses `display' property to show the cell value."
  (let* ((var-sym (stock--tp-cell-var code field))
         (layer-name (stock--tp-layer-name code field)))
    ;; Ensure variable exists
    (unless (boundp var-sym)
      (set var-sym ""))
    ;; Define the layer with reactive display property
    (tp-define-layer layer-name
      :props `(display ,(intern (concat "$" (symbol-name var-sym)))))))

(defun stock--tp-update-cell (code field value &optional face)
  "Update cell VALUE for stock CODE and FIELD.
Optionally apply FACE to the display.
Uses tp.el to incrementally update the display property."
  (let ((var-sym (stock--tp-cell-var code field))
        (display-value (if face
                           (propertize value 'face face)
                         value)))
    ;; Set the reactive variable, triggering tp.el auto-update
    (set var-sym display-value)))

(defun stock--tp-update-entry (entry)
  "Update all cells for a stock ENTRY using tp.el.
ENTRY is (CODE . PLIST)."
  (let* ((code (car entry))
         (plist (cdr entry))
         (is-index (member code stock-index-list))
         (percent (plist-get plist :change-percent))
         (percent-face (stock--get-change-face percent))
         (index-face 'stock-face-index-name)
         (fields '(:code :name :price :change-percent :high :low
                   :volume :turnover :open :yestclose)))
    (dolist (field fields)
      (let* ((value (plist-get plist field))
             (face (cond
                    ;; Apply index face to code and name
                    ((and is-index (memq field '(:code :name))) index-face)
                    ;; Apply change face to percent
                    ((eq field :change-percent) percent-face)
                    (t nil))))
        (when value
          (stock--tp-update-cell code field value face))))))

(defun stock--tp-init-buffer (buffer-name entries)
  "Initialize BUFFER-NAME with tp.el layers for ENTRIES.
Creates placeholder cells and applies tp layers for incremental updates."
  (with-current-buffer buffer-name
    (let ((inhibit-read-only t)
          (fields '(:code :name :price :change-percent :high :low
                    :volume :turnover :open :yestclose)))
      ;; Define layers and initialize variables for each cell
      (dolist (entry entries)
        (let ((code (car entry)))
          (dolist (field fields)
            (stock--tp-define-cell-layer code field)
            (stock--tp-update-cell code field "-")))))))

(defun stock--tp-render-incremental (entries)
  "Incrementally update stock data using tp.el.
ENTRIES is a list of (CODE . PLIST)."
  (dolist (entry entries)
    (stock--tp-update-entry entry)))

(defvar stock--first-render t
  "Flag indicating if this is the first render.")

;;;; Rendering Functions

(defun stock--render-buffer (buffer-name &optional manual)
  "Render stock data in BUFFER-NAME.
If MANUAL is t and `stock-colouring' is nil, remove faces.
Uses tp.el for incremental updates after first render."
  (let* ((tabulated-entries (mapcar #'stock--entry-to-tabulated
                                    stock--entry-cache))
         (entries (if stock-colouring
                      (mapcar #'stock--propertize-entry tabulated-entries)
                    tabulated-entries)))
    (when (and manual (not stock-colouring))
      (setq entries (mapcar #'stock--remove-entry-faces tabulated-entries)))
    (with-current-buffer buffer-name
      ;; Convert to tabulated-list format: (ID VECTOR)
      ;; The plist (third element) was used for sorting but is not needed
      ;; by tabulated-list-mode, so we extract only ID and VECTOR.
      (setq tabulated-list-entries
            (mapcar (lambda (e) (list (car e) (cadr e))) entries))
      ;; Use tabulated-list-print with update-only mode for incremental updates
      (tabulated-list-print t t)
      ;; Apply tp.el incremental updates to the rendered cells
      (stock--tp-render-incremental stock--entry-cache))))

(defun stock--fetch-and-render (buffer-name codes &optional callback)
  "Fetch data for CODES and render in BUFFER-NAME.
Call CALLBACK with entries after rendering."
  (stock--request
   (stock--make-request-url codes)
   (lambda ()
     (let ((resp (stock--parse-response)))
       (setq stock--entry-cache (stock--parse-response-to-entries codes resp))
       (stock--render-buffer buffer-name)
       (when callback
         (funcall callback stock--entry-cache))))))

(defun stock--validate-and-callback (codes callback)
  "Validate CODES are real stocks, call CALLBACK with valid entries."
  (stock--request
   (stock--make-request-url codes)
   (lambda ()
     (let* ((resp (stock--parse-response))
            (entries (stock--parse-response-to-entries codes resp))
            (valid (seq-filter #'stock--entry-valid-p entries)))
       (funcall callback valid)))))

;;;; Timer and Refresh Functions

(defun stock--timer-alive-p ()
  "Return t if stock buffer exists."
  (get-buffer stock-buffer-name))

(defun stock--schedule-refresh ()
  "Schedule next auto-refresh cycle."
  (stock-set-timeout #'stock--loop-refresh stock-refresh-seconds))

(defun stock--loop-refresh (_timer)
  "Main refresh loop, called by timer."
  (when (and (stock--timer-alive-p) (stock-weekday-p))
    (if (stock-working-time-p stock-buffer-name)
        (stock--fetch-and-render
         stock-buffer-name
         (append stock-index-list stock--favorite-codes)
         (lambda (_) (stock--schedule-refresh)))
      (stock--schedule-refresh))))

;;;; Initialization

(defun stock--init ()
  "Initialize stock, loading favorites from cache."
  (let ((cached (stock-read-cache stock-cache-path)))
    (if cached
        (setq stock--favorite-codes cached)
      (setq stock--favorite-codes stock-code-list)
      (stock-write-cache stock-cache-path stock--favorite-codes))))

(defun stock--switch-to-buffer (buffer-name)
  "Switch to BUFFER-NAME and enable stock-visual-mode."
  (pop-to-buffer buffer-name stock--pop-to-buffer-action)
  (stock-visual-mode))

;;;; Interactive Commands

;;;###autoload
(defun stock ()
  "Open the stock dashboard showing indices and favorite stocks."
  (interactive)
  (stock--init)
  (let ((was-alive (stock--timer-alive-p)))
    (stock--switch-to-buffer stock-buffer-name)
    (stock--fetch-and-render
     stock-buffer-name
     (append stock-index-list stock--favorite-codes)
     (lambda (_)
       (when (and stock-auto-refresh (not was-alive))
         (stock--schedule-refresh))))))

;;;###autoload
(defun stock-refresh ()
  "Manually refresh stock data."
  (interactive)
  (when (get-buffer-window stock-buffer-name)
    (stock--fetch-and-render stock-buffer-name
                              (append stock-index-list stock--favorite-codes)))
  (when (get-buffer-window stock-search-buffer-name)
    (stock--fetch-and-render stock-search-buffer-name stock--search-codes)))

;;;###autoload
(defun stock-search (codes)
  "Search for stocks by CODES (space-separated)."
  (interactive "sEnter stock codes to search: ")
  (setq stock--search-codes (split-string codes))
  (stock--switch-to-buffer stock-search-buffer-name)
  (stock--fetch-and-render stock-search-buffer-name stock--search-codes))

;;;###autoload
(defun stock-add (codes)
  "Add stocks by CODES (space-separated) to favorites."
  (interactive "sEnter stock codes to add: ")
  (let ((code-list (split-string codes)))
    (stock--validate-and-callback
     code-list
     (lambda (valid-entries)
       (let ((valid-codes (mapcar #'car valid-entries)))
         (when valid-codes
           (setq stock--favorite-codes
                 (delete-dups (append stock--favorite-codes valid-codes)))
           (stock-write-cache stock-cache-path stock--favorite-codes)
           (stock--fetch-and-render
            stock-buffer-name
            (append stock-index-list stock--favorite-codes)
            (lambda (_)
              (message "Added: [%s]" (string-join valid-codes ", "))))))))))

;;;###autoload
(defun stock-remove ()
  "Remove a stock from favorites."
  (interactive)
  (if (null stock--favorite-codes)
      (message "No favorite stocks to remove.")
    (let* ((code (completing-read "Select stock to remove: "
                                  stock--favorite-codes nil t))
           (idx (cl-position code stock--favorite-codes :test #'string=)))
      (when idx
        (setq stock--favorite-codes
              (stock-remove-nth-element stock--favorite-codes idx))
        (stock-write-cache stock-cache-path stock--favorite-codes)
        (stock--fetch-and-render
         stock-buffer-name
         (append stock-index-list stock--favorite-codes)
         (lambda (_)
           (message "Removed: <%s>" code)))))))

;;;###autoload
(defun stock-switch-colouring ()
  "Toggle color display on/off (for discrete viewing)."
  (interactive)
  (setq stock-colouring (not stock-colouring))
  (stock--render-buffer (buffer-name) t)
  (message "Colouring %s" (if stock-colouring "enabled" "disabled")))

;;;; Major Mode

(defvar stock-visual-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "+" #'stock-add)
    (define-key map "_" #'stock-remove)
    (define-key map "c" #'stock-switch-colouring)
    (define-key map "g" #'stock-refresh)
    (define-key map "s" #'stock-search)
    map)
  "Keymap for `stock-visual-mode'.")

(define-derived-mode stock-visual-mode tabulated-list-mode "Stock"
  "Major mode for A-share stock real-time dashboard.

Key bindings:
\\{stock-visual-mode-map}"
  (setq tabulated-list-format stock--visual-columns)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook #'stock-refresh nil t)
  (tabulated-list-init-header)
  ;; tablist-minor-mode is optional, only enable if available
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

;;;; Mode-line and Header-line Common Functions

(defun stock--format-line-entry (entry format-str)
  "Format ENTRY for line display using FORMAT-STR.
ENTRY is (CODE . PLIST)."
  (let* ((plist (cdr entry))
         (name (plist-get plist :name))
         (percent (plist-get plist :change-percent))
         (face (stock--get-change-face percent)))
    (propertize (format-spec format-str
                             `((?n . ,name)
                               (?p . ,percent)))
                'face face)))


;;;; Mode-line Functions

(defun stock--modeline-update (entries)
  "Update mode-line with ENTRIES data."
  (setq stock--modeline-data entries)
  (setq stock-modeline-string
        (if entries
            (mapconcat (lambda (e)
                         (stock--format-line-entry e stock-modeline-format))
                       entries " ")
          ""))
  (force-mode-line-update t))

(defun stock--modeline-fetch ()
  "Fetch stock data for mode-line display."
  (when stock-modeline-stocks
    (stock--request
     (stock--make-request-url stock-modeline-stocks)
     (lambda ()
       (let* ((resp (stock--parse-response))
              (entries (stock--parse-response-to-entries
                        stock-modeline-stocks resp))
              (valid (seq-filter #'stock--entry-valid-p entries)))
         (stock--modeline-update valid))))))

(defun stock--modeline-schedule ()
  "Schedule next mode-line refresh."
  (when stock--modeline-timer
    (stock-set-timeout #'stock--modeline-loop
                        stock-modeline-refresh-seconds)))

(defun stock--modeline-loop (_timer)
  "Mode-line refresh loop."
  (when stock--modeline-timer
    (when (and (stock-weekday-p) (stock-trading-time-p))
      (stock--modeline-fetch))
    (stock--modeline-schedule)))

;;;###autoload
(defun stock-modeline-mode (&optional arg)
  "Toggle stock display in mode-line.
With prefix ARG, enable if positive, disable otherwise."
  (interactive "P")
  (let ((enable (if arg
                    (> (prefix-numeric-value arg) 0)
                  (not stock--modeline-timer))))
    (if enable
        (progn
          (unless stock-modeline-stocks
            (setq stock-modeline-stocks stock-code-list))
          (unless (member '(:eval stock-modeline-string) global-mode-string)
            (if global-mode-string
                (push '(:eval stock-modeline-string) global-mode-string)
              (setq global-mode-string '("" (:eval stock-modeline-string)))))
          (setq stock--modeline-timer t)
          (stock--modeline-fetch)
          (stock--modeline-schedule)
          (message "Stock mode-line enabled."))
      (setq stock--modeline-timer nil)
      (setq stock-modeline-string "")
      (setq global-mode-string
            (delete '(:eval stock-modeline-string) global-mode-string))
      (force-mode-line-update t)
      (message "Stock mode-line disabled."))))

;;;###autoload
(defun stock-modeline-add (codes)
  "Add CODES (space-separated) to mode-line display."
  (interactive "sEnter stock codes for mode-line: ")
  (let ((code-list (split-string codes)))
    (stock--validate-and-callback
     code-list
     (lambda (valid-entries)
       (let ((valid-codes (mapcar #'car valid-entries)))
         (when valid-codes
           (setq stock-modeline-stocks
                 (delete-dups (append stock-modeline-stocks valid-codes)))
           (stock--modeline-fetch)
           (message "Mode-line: added [%s]" (string-join valid-codes ", "))))))))

;;;###autoload
(defun stock-modeline-remove ()
  "Remove a stock from mode-line display."
  (interactive)
  (if (null stock-modeline-stocks)
      (message "No stocks in mode-line.")
    (let* ((code (completing-read "Remove from mode-line: "
                                  stock-modeline-stocks nil t))
           (idx (cl-position code stock-modeline-stocks :test #'string=)))
      (when idx
        (setq stock-modeline-stocks
              (stock-remove-nth-element stock-modeline-stocks idx))
        (stock--modeline-fetch)
        (message "Mode-line: removed <%s>" code)))))


;;;;; header-line functions

(defun stock--headerline-update (entries)
  "Update header-line with ENTRIES data."
  (setq stock--headerline-data entries)
  (setq stock-headerline-string
        (if entries
            (mapconcat (lambda (e)
                         (stock--format-line-entry e stock-headerline-format))
                       entries " ")
          ""))
  (force-mode-line-update t))

(defun stock--headerline-fetch ()
  "Fetch stock data for header-line display."
  (when stock-headerline-stocks
    (stock--request
     (stock--make-request-url stock-headerline-stocks)
     (lambda ()
       (let* ((resp (stock--parse-response))
              (entries (stock--parse-response-to-entries
                        stock-headerline-stocks resp))
              (valid (seq-filter #'stock--entry-valid-p entries)))
         (stock--headerline-update valid))))))

(defun stock--headerline-schedule ()
  "Schedule next header-line refresh."
  (when stock--headerline-timer
    (stock-set-timeout #'stock--headerline-loop
                        stock-headerline-refresh-seconds)))

(defun stock--headerline-loop (_timer)
  "Header-line refresh loop."
  (when stock--headerline-timer
    (when (and (stock-weekday-p) (stock-trading-time-p))
      (stock--headerline-fetch))
    (stock--headerline-schedule)))

;;;###autoload
(defun stock-headerline-mode (&optional arg)
  "Toggle stock display in header-line.
With prefix ARG, enable if positive, disable otherwise."
  (interactive "P")
  (let ((enable (if arg
                    (> (prefix-numeric-value arg) 0)
                  (not stock--headerline-timer))))
    (if enable
        (progn
          (unless stock-headerline-stocks
            (setq stock-headerline-stocks stock-code-list))
          (setq-default header-line-format
                        '(:eval stock-headerline-string))
          (setq stock--headerline-timer t)
          (stock--headerline-fetch)
          (stock--headerline-schedule)
          (message "Stock header-line enabled."))
      (setq stock--headerline-timer nil)
      (setq stock-headerline-string "")
      (setq-default header-line-format nil)
      (force-mode-line-update t)
      (message "Stock header-line disabled."))))

;;;###autoload
(defun stock-headerline-add (codes)
  "Add CODES (space-separated) to header-line display."
  (interactive "sEnter stock codes for header-line: ")
  (let ((code-list (split-string codes)))
    (stock--validate-and-callback
     code-list
     (lambda (valid-entries)
       (let ((valid-codes (mapcar #'car valid-entries)))
         (when valid-codes
           (setq stock-headerline-stocks
                 (delete-dups (append stock-headerline-stocks valid-codes)))
           (stock--headerline-fetch)
           (message "Header-line: added [%s]" (string-join valid-codes ", "))))))))

;;;###autoload
(defun stock-headerline-remove ()
  "Remove a stock from header-line display."
  (interactive)
  (if (null stock-headerline-stocks)
      (message "No stocks in header-line.")
    (let* ((code (completing-read "Remove from header-line: "
                                  stock-headerline-stocks nil t))
           (idx (cl-position code stock-headerline-stocks :test #'string=)))
      (when idx
        (setq stock-headerline-stocks
              (stock-remove-nth-element stock-headerline-stocks idx))
        (stock--headerline-fetch)
        (message "Header-line: removed <%s>" code)))))

;;;; Variable Watchers

(defun stock--on-code-list-change (_symbol newval _operation _where)
  "Handle changes to `stock-code-list'.
NEWVAL is the new value being set.
Automatically refreshes the dashboard when stock-code-list changes."
  (when (and (stock--timer-alive-p)
             (listp newval))
    ;; Update favorites to match the new code list
    (setq stock--favorite-codes newval)
    (stock-write-cache stock-cache-path stock--favorite-codes)
    ;; Refresh the dashboard
    (stock--fetch-and-render
     stock-buffer-name
     (append stock-index-list stock--favorite-codes)
     (lambda (_)
       (message "Stock list updated: %d stocks"
                (length stock--favorite-codes))))))

(defun stock--on-index-list-change (_symbol newval _operation _where)
  "Handle changes to `stock-index-list'.
NEWVAL is the new value being set.
Automatically refreshes the dashboard when stock-index-list changes."
  (when (and (stock--timer-alive-p)
             (listp newval))
    ;; Refresh the dashboard with new index list
    (stock--fetch-and-render
     stock-buffer-name
     (append newval stock--favorite-codes)
     (lambda (_)
       (message "Index list updated: %d indices" (length newval))))))

(defun stock--on-modeline-stocks-change (_symbol newval _operation _where)
  "Handle changes to `stock-modeline-stocks'.
NEWVAL is the new value being set.
Automatically refreshes the mode-line when stock-modeline-stocks changes."
  (when (and stock--modeline-timer
             (listp newval))
    (stock--modeline-fetch)))

(defun stock--on-headerline-stocks-change (_symbol newval _operation _where)
  "Handle changes to `stock-headerline-stocks'.
NEWVAL is the new value being set.
Automatically refreshes the header-line when stock-headerline-stocks changes."
  (when (and stock--headerline-timer
             (listp newval))
    (stock--headerline-fetch)))

;; Register variable watchers
;; Note: add-variable-watcher requires Emacs 26.1+
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher 'stock-code-list #'stock--on-code-list-change)
  (add-variable-watcher 'stock-index-list #'stock--on-index-list-change)
  (add-variable-watcher 'stock-modeline-stocks #'stock--on-modeline-stocks-change)
  (add-variable-watcher 'stock-headerline-stocks #'stock--on-headerline-stocks-change))

(provide 'stock)

;;; stock.el ends here
