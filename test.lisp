;;;; Test suite for XML parser
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :xml))

(defparameter *test-path* #p"d:/ccl-projects/xml/test/xmltest/"
  "Location on disk where the test suite resides.")

(defun run-test-suite ()
  "Open xmltest.xml and run all tests inside of it."
  (flet ((test-pathname (test)
           (merge-pathnames test *test-path*)))
    (loop
       with suite = (xml-load (test-pathname "xmltest.xml"))

       ;; loop over all the tests
       for test in (xml-query suite "//TEST")

       ;; get the id and uri
       for type = (xml-node-value (first (xml-query test "@TYPE")))
       for id = (xml-node-value (first (xml-query test "@ID")))
       for uri = (xml-node-value (first (xml-query test "@URI")))

       ;; load the test document - malformed documents *should* fail
       for doc = (handler-case
                     (xml-load (test-pathname uri))
                   (condition ()
                     (string-equal type "not-wf")))

       ;; output the result
       do (format t "~20,,,'.a...~:[FAIL~;PASS~]~%" id doc))))
