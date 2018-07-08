#!/usr/bin/env sh
":"; exec emacs -Q --script "$0" -- "run-tests" "$@" # -*-emacs-lisp-*-

;; some high-level tests of the sarit-exist-webapp

;; run this as: bash ./tests.el

;; or 

;; run from command line as `emacs --no-init-file --no-site-file -batch -l tests.el -f ert-run-tests-batch-and-exit'

(require 'ert)
(require 'url)
(require 'rx)
(eval-when-compile (require 'cl-lib))


(defvar show-debug? nil)
(setq show-debug?
      (or (member "--verbose" (member "--" command-line-args))
	  (member "-v" (member "--" command-line-args))
	  nil))

(defvar exist-apps-base-url
  (url-generic-parse-url "http://localhost:8088/")
  "The base url (including path) to exist-db applications.

Typical values will be: http://127.0.1.1:8088/, for exist started
with bin/server.sh, or http://127.0.1.1:8088/exist/apps/ for
exist started with bin/startup.sh.")

(setq exist-apps-base-url
      (let ((url-arg (or (member "--base-url" (member "--" command-line-args))
			 (member "-b" (member "--" command-line-args)))))
	(url-generic-parse-url
	 (if url-arg
	     (cadr url-arg)
	   "http://127.0.0.1:8088/"))))

(defun debug-message (format-string &rest args)
  ;; (message "debug args: %s" (list format-string args))
  (when show-debug?
    ;; (message (apply 'format format-string args))
    (message
     (format
      "test-debug %s: %s"
      (format-time-string "%FT%T%z" (current-time))
      (if args
	  (apply 'format format-string args)
	format-string)))))

(defun sarit-tests-get-docs-in-corpus (&optional full-path?)
  (let ((default-directory (if load-file-name
			       (file-name-directory load-file-name)
			     default-directory)))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../sarit-pm/SARIT-corpus/saritcorpus.xml"))
      (mapcar (lambda (entry)
		(when (equal (car entry) 'include)
		  (if full-path?
		      (expand-file-name (cdr (caar (cdr entry))))
		    (cdr (caar (cdr entry))))))
	      (cdr (cdr (cdr (libxml-parse-xml-region (point-min) (point-max)))))))))

;; (sarit-tests-get-docs-in-corpus)

;; (sarit-tests-get-docs-in-corpus t)

(when show-debug?
  (debug-message "Showing debug messages")
  (debug-message "command-line-args: %s" command-line-args))

;;; url retrieval functions give us the following buffer local
;;; variables:

;; '((buffer-display-time 23029 54969 23547 478000)
;;  (buffer-display-count . 2)
;;  (buffer-invisibility-spec . t)
;;  (buffer-file-truename)
;;  (point-before-scroll)
;;  (buffer-auto-save-file-format . t)
;;  (buffer-file-format)
;;  (enable-multibyte-characters)
;;  (mark-active)
;;  (mode-line-format ""
;; 		   (eldoc-mode-line-string
;; 		    (" " eldoc-mode-line-string " "))
;; 		   "%b [%s]")
;;  (mode-name . "Fundamental")
;;  (major-mode . fundamental-mode)
;;  (buffer-read-only)
;;  (buffer-auto-save-file-name)
;;  (buffer-saved-size . 0)
;;  (buffer-backed-up)
;;  (default-directory . "/home/beta/webstuff/eXist-stuff/sarit-existdb/tests/")
;;  (buffer-file-name)
;;  (url-current-object .
;; 		     [cl-struct-url "http" nil nil "127.0.0.1" 8080 "/exist/apps/sarit-pm/works/carakasamhita.tex?token=a452857b-8938-495b-ad92-afa9783dd84e&cache=no&source=yes" nil nil t nil t])
;;  (url-http-end-of-headers . #<marker at 389 in  *http 127.0.0.1:8080*>)
;;  (url-http-content-type . "media-type=application/pdf")
;;  (url-http-content-length)
;;  (url-http-transfer-encoding . "chunked")
;;  (url-http-after-change-function . url-http-chunked-encoding-after-change-function)
;;  (url-http-response-version . "1.1")
;;  (url-http-response-status . 200)
;;  (url-http-chunked-length . 0)
;;  (url-http-chunked-counter . 80)
;;  (url-http-chunked-start . #<marker at 390 in  *http 127.0.0.1:8080*>)
;;  (url-callback-function .
;; 			#[128 "\302\303\304p#\210\300\305\240\210\301p\240\207"
;; 			      [(t)
;; 			       (#<buffer  *http 127.0.0.1:8080*>)
;; 			       url-debug retrieval "Synchronous fetching done (%S)" t]
;; 			      5 "\n\n(fn &rest IGNORED)"])
;;  (url-callback-arguments nil)
;;  (url-show-status)
;;  (url-http-process . #<process 127.0.0.1>)
;;  (url-http-method . "GET")
;;  (url-http-extra-headers)
;;  (url-http-noninteractive)
;;  (url-http-data)
;;  (url-http-target-url .
;; 		      [cl-struct-url "http" nil nil "127.0.0.1" 8080 "/exist/apps/sarit-pm/works/carakasamhita.tex?token=a452857b-8938-495b-ad92-afa9783dd84e&cache=no&source=yes" nil nil t nil t])
;;  (url-http-no-retry)
;;  (url-http-connection-opened . t)
;;  (url-mime-accept-string)
;;  (url-http-proxy)
;;  (deactivate-mark)
;;  (minor-mode-overriding-map-alist))


(ert-deftest test-general-urls ()
  "Test a list of urls.

Helps to avoid regressions."
  (let ((cases `(,(concat (url-recreate-url exist-apps-base-url)
			  "sarit-pm/docs/search-help-brief.html")
		 ,(concat (url-recreate-url exist-apps-base-url)
			  "sarit-pm/docs/search-help-interface.html")
		 ,(concat (url-recreate-url exist-apps-base-url)
			  "sarit-pm/docs/welcome.html"))))
    (mapc
     (lambda (c)
       (with-current-buffer (url-retrieve-synchronously c (not show-debug?))
	 ;; (pop-to-buffer (current-buffer))
	 (debug-message
	  "url-http-response-status for %s: %s"
	  (url-unhex-string (url-recreate-url url-http-target-url))
	  url-http-response-status)
	 (should
	  (equal
	   url-http-response-status
	   200))))
     cases)))

(ert-deftest test-search-urls ()
  "Test search function for specific things.

These urls had problems at one time or another, avoid
regressions."
  (let ((cases
	 `(
	   ,(concat (url-recreate-url exist-apps-base-url)
		   "sarit-pm/works/search.html?query=tatra&field=text&tei-target=tei-text&work-authors=all")
	   ,(concat (url-recreate-url exist-apps-base-url)
		   "sarit-pm/works/search.html?query=tatra+AND+atra+AND+नरयागश्चत्&field=text&tei-target=tei-text&work-authors=all")
	   ,(concat (url-recreate-url exist-apps-base-url)
		    "sarit-pm/works/search.html?query=tatra+AND+atra&field=text&tei-target=tei-text&work-authors=all"))))
    (let ((url-cache-expire-time 1))
      (url-cache-prune-cache))
    (mapc
     (lambda (c)
       (with-current-buffer (url-retrieve-synchronously c (not show-debug?))
	 ;; (pop-to-buffer (current-buffer))
	 (debug-message
	  "url-http-response-status for %s: %s"
	  (url-unhex-string (url-recreate-url url-http-target-url))
	  url-http-response-status)
	 (should
	  (equal
	   url-http-response-status
	   200))
	 (goto-char (point-min))
	 (re-search-forward "id=\"hit-count\"[^>]*>\\([0-9]+\\)</span>" nil)
	 (let ((result-count (string-to-number (match-string 1))))
	   (debug-message "Results: %s" result-count)
	   (should
	    (equal
	     t
	     (< 0 result-count)))
	   (when (< 30 result-count)
	     (with-current-buffer (url-retrieve-synchronously (format "%s%s" c "&start=21") (not show-debug?))
	       (debug-message (format "url-http-response-status for %s is: %s"
				      (url-recreate-url url-http-target-url)
				      url-http-response-status))
	       (should
		(equal
		 url-http-response-status
		 200)))))))
     cases)))

;; (ert "test-fetching-urls")

(ert-deftest test-check-access-to-texts ()
  "See if the individual XML files can be browsed/displayed."
  (let ((base-url exist-apps-base-url)
	(texts (sarit-tests-get-docs-in-corpus)))
    (should (equal t (< 0 (length texts))))
    (mapc
     (lambda (x)
       (let ((text-url (url-generic-parse-url
			(concat (url-recreate-url base-url)
				(format "sarit-pm/works/%s?view=div" x)))))
	 (debug-message "Retrieving %s at %s" x (url-recreate-url text-url))
	 (with-current-buffer (url-retrieve-synchronously text-url (not show-debug?))
	   ;; search links to contents: href="siddhiviniscayatika.xml?root=1.4.5.21&amp;view=div"
	   (let
	       ((links
		 (while (re-search-forward (rx-to-string `(and "href=\""
							       ,x
							       (group-n 1
									"?root="
									(1+ (not (any "\"")) ))
							       "\""))
					   nil 'noerr)
		   (match-string 1))))
	     (when (< 0 (length links))
	       (let ((query (elt (random (length links)) links))
		     (contents-url text-url))
		 (setf (url-filename contents-url)
		       (concat (car (url-path-and-query contents-url))
			       query))
		 (debug-message "Retrieving contents of %s at %s" x (url-recreate-url contents-url))
		 (with-current-buffer (url-retrieve-synchronously contents-url (not show-debug?))
		   (should
		    (equal
		     url-http-response-status
		     200)))))))))
     texts)
    ;; currently not working for all texts (needs <pb/> elements?)
    (mapc
     (lambda (x)
       (when
	   ;; cheap lookup to see if we have at least 5 "<pb/>" elements
	   (with-temp-buffer
	     (insert-file-contents-literally
	      (expand-file-name
	       x
	       (file-name-as-directory
		(expand-file-name
		 "../sarit-pm/SARIT-corpus"
		 (if load-file-name
		     (file-name-directory load-file-name)
		   default-directory))))
	      nil)
	     (re-search-forward "<pb\\b" nil 'no-error 5))
	 (let ((page-url (url-generic-parse-url
			  (concat (url-recreate-url base-url)
				  (format "sarit-pm/works/%s?view=page" x)) ) ))
	   (debug-message "Retrieving pagewise view for %s at %s" x (url-recreate-url page-url))
	   (with-current-buffer (url-retrieve-synchronously page-url (not show-debug?))
	     (should
	      (equal
	       url-http-response-status
	       200))))))
     texts)))

;; (url-generic-parse-url "http://localhost:8088/sarit-pm/works/siddhiviniscayatika.xml?view=div")

;; (ert "test-check-access-to-texts")

(ert-deftest test-display-principals ()
  :expected-result :failed
  (mapcar
   (lambda (x)
     (let ((text-url (url-generic-parse-url
		      (concat (url-recreate-url base-url)
			      (format "sarit-pm/works/%s?view=div" x)))))
       (with-current-buffer (url-retrieve-synchronously text-url (not show-debug?))
	 (should
	  (equal
	   url-http-response-status
	   200))
	 (goto-char (point-min))
	 ;; ascertain principals, but avoid bad formatting
	 (save-excursion
	   (when (re-search-forward "<li\\s-+title=\"tei:principal\">" nil 'no-error)
	     (debug-message "Checking display of principals for %s" x)
	     (should
	      (equal
	       (re-search-forward "\\s-+,"
				  (save-excursion (re-search-forward "</li>") (point))
				  'no-error)
	       nil)))))))
   (sarit-tests-get-docs-in-corpus)))

(ert-deftest test-search-display-kwic-or-context ()
  "Check if the display parameter is working."
  (let ((cases
	 `(
	   ,(concat (url-recreate-url exist-apps-base-url)
		    "sarit-pm/works/search.html?query=*citra*&field=text&tei-target=tei-text&work-authors=all&display=%s"))))
    (let ((url-cache-expire-time 1))
      (url-cache-prune-cache))
    (mapc
     (lambda (c)
       ;; check kwic
       (with-current-buffer (url-retrieve-synchronously (format c "kwic") (not show-debug?))
	 (pop-to-buffer (current-buffer))
	 (debug-message
	  "url-http-response-status for %s: %s"
	  (url-unhex-string (url-recreate-url url-http-target-url))
	  url-http-response-status)
	 (should
	  (equal
	   url-http-response-status
	   200))
	 (goto-char (point-min))
	 ;; check we have at least one result
	 (save-excursion
	   (should
	    (re-search-forward "div\\s-+class=\"document\"" nil)))
	 ;;; probably useless:
	 ;; (while (re-search-forward "div\\s-+class=\"document\"" nil 'noerr)
	 ;;   (let ((end (save-excursion (save-match-data (re-search-forward "</div>")))))
	 ;;     (save-excursion
	 ;;       ;; search for mark item or a button that’s hiding stuff
	 ;;       (should (re-search-forward "<mark>\\|<button[^>]+title=\"Note\"" end 'noerr)))))
	 )
       ;; check structural
       (with-current-buffer (url-retrieve-synchronously (format c "structural") (not show-debug?))
	 (pop-to-buffer (current-buffer))
	 (debug-message
	  "url-http-response-status for %s: %s"
	  (url-unhex-string (url-recreate-url url-http-target-url))
	  url-http-response-status)
	 (should
	  (equal
	   url-http-response-status
	   200))
	 (goto-char (point-min))
	 ;; check we have at least one result
	 (save-excursion
	   (should
	    (re-search-forward "div\\s-+class=\"document\"" nil)))
	 (while (re-search-forward "div\\s-+class=\"document\"" nil 'noerr)
	   (let ((end (or
		       (save-excursion
			 (save-match-data
			   (when (re-search-forward "div\\s-+class=\"document\"" nil 'noerr)
			     (- (point) 2))))
		       (point-max))))
	     (save-excursion
	       (should (re-search-forward "<mark>" end 'noerr)))
	     (save-excursion
	       (should (re-search-forward "class=\"search-tag\"" end 'noerr)))))))
     cases)))

;; (ert "test-search-display-kwic-or-context")

(ert-deftest test-fetch-tex ()
  "Check if TeX generation is working."
  :expected-result :failed
  (let ((doc-urls (mapcar
		   (lambda (doc)
		     (format
		      "%ssarit-pm/works/%s?source=yes"
		      (url-recreate-url exist-apps-base-url)
		      (concat (file-name-sans-extension doc) ".tex")))
		   (sarit-tests-get-docs-in-corpus))))
    (dolist (doc-url doc-urls)
      (debug-message  "Retrieving TeX source %s" doc-url)
      (with-current-buffer (url-retrieve-synchronously doc-url)
	(should
	 (equal
	  url-http-response-status
	  200))
	(should
	 (string-match "application/tex" url-http-content-type))))))

;; (ert "test-fetch-tex")

(ert-deftest test-fetch-pdf ()
  "Check if PDF download is working."
  (let ((doc-urls (mapcar
		   (lambda (doc)
		     (format
		      "%ssarit-pm/works/%s"
		      (url-recreate-url exist-apps-base-url)
		      (concat (file-name-sans-extension doc) ".tex")))
		   (sarit-tests-get-docs-in-corpus))))
    (dolist (doc-url doc-urls)
      (debug-message  "Retrieving TeX -> PDF for %s" doc-url)
      (with-current-buffer (url-retrieve-synchronously doc-url)
	(should
	 (equal
	  url-http-response-status
	  200))
	(should
	 (string-match "application/pdf" url-http-content-type))))))

(ert-deftest test-fetch-epub ()
  "Check if epub generation is working."
  (let ((doc-urls (mapcar
		   (lambda (doc)
		     (format
		      "%ssarit-pm/works/%s"
		      (url-recreate-url exist-apps-base-url)
		      (concat (file-name-sans-extension doc) ".epub")))
		   (sarit-tests-get-docs-in-corpus))))
    (dolist (doc-url doc-urls)
      (debug-message  "Retrieving epub %s" doc-url)
      (with-current-buffer (url-retrieve-synchronously doc-url)
	(should
	 (equal
	  url-http-response-status
	  200))))))

;; (let ((exist-apps-base-url (url-generic-parse-url "http://127.0.1.1:8080/exist/apps/")))
;;   (ert "test-fetch-tex"))

(when noninteractive
  (cond
   ((or (member "-h" command-line-args)
	(member "--help" command-line-args))
    (message
     "
Run high-level tests against SARIT’s eXistdb app.  

Options:

-h, --help             Show help
-b, --base-url         URL where sarit-pm is running (default: %s)
-v, --verbose          Say what’s going on

"
     (url-recreate-url exist-apps-base-url)))
   ((member "run-tests" command-line-args)
    (message "Running tests against %s"
	     (url-recreate-url exist-apps-base-url))
    (ert-run-tests-batch-and-exit))))


