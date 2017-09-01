#! /bin/sh
":"; exec emacs --no-site-file --script "$0" -- "$@" # -*-emacs-lisp-*-

;; some high-level tests of the sarit-exist-webapp

;; run this as: bash ./tests.el

;; or 

;; run from command line as `emacs --no-init-file --no-site-file -batch -l tests.el -f ert-run-tests-batch-and-exit'


(require 'ert)
(require 'url)

(ert-deftest test-fetching-urls ()
  (let ((cases
	 '(
	   ;; fairly random urls, just noticed some problems with them
	   ;; while browsing
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra&field=text&tei-target=tei-text&work-authors=all"
	   "http://10.8.0.34:8080/exist/apps/sarit-pm/works/search.html?query=tatra+AND+atra+AND+%E0%A4%A8%E0%A4%B0%E0%A4%AF%E0%A4%BE%E0%A4%97%E0%A4%B6%E0%A5%8D%E0%A4%9A%E0%A4%A4%E0%A5%8D&field=text&tei-target=tei-text&work-authors=all"
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra+AND+atra&field=text&tei-target=tei-text&work-authors=all"
	   )))
    (let ((url-cache-expire-time 1))
      (url-cache-prune-cache))
    (mapc
     (lambda (c)
       (with-current-buffer (url-retrieve-synchronously c)
	 (pop-to-buffer (current-buffer))
	 (message "url-http-response-status for %s is: %s"
		  (url-recreate-url url-http-target-url)
		  url-http-response-status)
	 (should
	  (equal
	   url-http-response-status
	   200))
	 (goto-char (point-min))
	 (re-search-forward "id=\"hit-count\">\\([0-9]+\\)</span>" nil 'noerr)
	 (let ((result-count (string-to-number (match-string 1))))
	   (message "Results: %s" result-count)
	   (should
	    (equal
	     t
	     (< 0 result-count)))
	   (when (< 30 result-count)
	     (with-current-buffer (url-retrieve-synchronously (format "%s%s" c "&start=21"))
	       (message "url-http-response-status for %s is: %s"
			(url-recreate-url url-http-target-url)
			url-http-response-status)
	       (should
		(equal
		 url-http-response-status
		 200)))))))
     cases)))

(ert "test-fetching-urls")



;; FAQ: Why test in emacs-lisp? --> Please show me how to do this in Xquery.
