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

(ert-deftest test-fetching-urls ()
  "Test search function for specific things.

These urls had problems at one time or another, avoid
regressions."
  (let ((cases
	 '(
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra&field=text&tei-target=tei-text&work-authors=all"
	   "http://localhost:8080/exist/apps/sarit-pm/works/search.html?query=tatra+AND+atra+AND+%E0%A4%A8%E0%A4%B0%E0%A4%AF%E0%A4%BE%E0%A4%97%E0%A4%B6%E0%A5%8D%E0%A4%9A%E0%A4%A4%E0%A5%8D&field=text&tei-target=tei-text&work-authors=all"
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

;; (ert "test-fetching-urls")

(ert-deftest test-check-access-to-texts ()
  "See if the individual XML files can be browsed/displayed."
  (let ((base-url (url-generic-parse-url "http://localhost:8080"))
	(texts (with-temp-buffer
		 (insert-file (expand-file-name "../SARIT-corpus/saritcorpus.xml"
						(if load-file-name
						    (file-name-directory load-file-name)
						  default-directory)))
		 (mapcar (lambda (entry)
			   (when (equal (car entry) 'include)
			     (cdr (caar (cdr entry)))))
			 (cdr (cdr (cdr (libxml-parse-xml-region (point-min) (point-max)))))))))
    (should (equal t (< 0 (length texts))))
    (mapc
     (lambda (x)
       (let ((text-url (url-generic-parse-url
			(concat (url-recreate-url base-url)
				(format "/exist/apps/sarit-pm/works/%s?view=div" x)))))
	 (message "Retrieving %s at %s" x (url-recreate-url text-url))
	 (with-current-buffer (url-retrieve-synchronously text-url)
	   (should
	    (equal
	     url-http-response-status
	     200))
	   ;; search links to contents: href="siddhiviniscayatika.xml?root=1.4.5.21&amp;view=div"
	   (goto-char (point-min))
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
		 (message "Retrieving contens of %s at %s" x (url-recreate-url contents-url))
		 (with-current-buffer (url-retrieve-synchronously contents-url)
		   (should
		    (equal
		     url-http-response-status
		     200)))))))))
     texts)
;;; currently not working for all texts (needs <pb/> elements?)
    ;; (mapc
    ;;  (lambda (x)
    ;;    (setf (url-filename base-url) (format "/exist/apps/sarit-pm/works/%s?view=page" x))
    ;;    (message "Retrieving %s at %s" x (url-recreate-url base-url))
    ;;    (with-current-buffer (url-retrieve-synchronously base-url)
    ;; 	 (should
    ;; 	  (equal
    ;; 	   url-http-response-status
    ;; 	   200))))
    ;;  texts)
    ))




;; (url-generic-parse-url "http://localhost:8080/exist/apps/sarit-pm/works/siddhiviniscayatika.xml?view=div")

;; (ert "test-check-access-to-texts")


(ert-run-tests-batch-and-exit)


;; FAQ: Why test in emacs-lisp? --> Please show me how to do this in Xquery.
