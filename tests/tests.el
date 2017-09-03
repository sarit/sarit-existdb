#! /bin/sh
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

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
       (expand-file-name "../SARIT-corpus/saritcorpus.xml"))
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


(ert-deftest test-fetching-urls ()
  "Test search function for specific things.

These urls had problems at one time or another, avoid
regressions."
  (let ((cases
	 '(
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra&field=text&tei-target=tei-text&work-authors=all"
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra+AND+atra+AND+नरयागश्चत्&field=text&tei-target=tei-text&work-authors=all"
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra+AND+atra&field=text&tei-target=tei-text&work-authors=all"
	   )))
    (let ((url-cache-expire-time 1))
      (url-cache-prune-cache))
    (mapc
     (lambda (c)
       (with-current-buffer (url-retrieve-synchronously c (not show-debug?))
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
	 (re-search-forward "id=\"hit-count\">\\([0-9]+\\)</span>" nil 'noerr)
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
  (let ((base-url (url-generic-parse-url "http://localhost:8080"))
	(texts (sarit-tests-get-docs-in-corpus)))
    (should (equal t (< 0 (length texts))))
    (mapc
     (lambda (x)
       (let ((text-url (url-generic-parse-url
			(concat (url-recreate-url base-url)
				(format "/exist/apps/sarit-pm/works/%s?view=div" x)))))
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
		 (setf (url-filename contents-url) (concat (car (url-path-and-query contents-url))
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
		 "../SARIT-corpus"
		 (if load-file-name
		     (file-name-directory load-file-name)
		   default-directory))))
	      nil)
	     (re-search-forward "<pb\\b" nil 'no-error 5))
	 (setf (url-filename base-url) (format "/exist/apps/sarit-pm/works/%s?view=page" x))
	 (debug-message "Retrieving page wise view for %s at %s" x (url-recreate-url base-url))
	 (with-current-buffer (url-retrieve-synchronously base-url (not show-debug?))
	   (should
	    (equal
	     url-http-response-status
	     200)))))
     texts)))

;; (url-generic-parse-url "http://localhost:8080/exist/apps/sarit-pm/works/siddhiviniscayatika.xml?view=div")

;; (ert "test-check-access-to-texts")

(ert-deftest test-display-principals ()
  :expected-result :failed 
  (mapcar
   (lambda (x)
     (let ((text-url (url-generic-parse-url
		      (concat (url-recreate-url base-url)
			      (format "/exist/apps/sarit-pm/works/%s?view=div" x)))))
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



(ert-run-tests-batch-and-exit)


