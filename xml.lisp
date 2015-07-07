;;;; XML parser for LispWorks
;;;;
;;;; Copyright (c) 2015 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :xml
  (:use :cl :lw :hcl :external-format)
  (:export

   ;; parsing strings
   #:xml-parse
   #:xml-parse-doc

   ;; load from file
   #:xml-load
   #:xml-load-doc))

(in-package :xml)

;;; ----------------------------------------------------

(defun xml-parse (context string &optional source)
  "Parse a string as XML."
  (let ((p (make-instance 'xml-parser :string string :source source)))
    (handler-case
        (prog1 context
          (sax:start-document context)

          ;; document ::= decl misc* doctype? misc* element misc*
          (xml-decl p context)
          (xml-misc p context)
          (xml-doctype p context)
          (xml-misc p context)
          (xml-element p context)
          (xml-misc p context)

          ;; done
          (sax:end-document context))
      (error (c)
        (error "~a on line ~d~@[ of ~s~]" c (xml-parser-line p) (xml-parser-source p))))))

;;; ----------------------------------------------------

(defun xml-parse-doc (string &optional source)
  "Parse a string as XML into a document context."
  (xml-parse (make-instance 'xml-doc:xml-doc) string source))

;;; ----------------------------------------------------

(defun xml-load (context pathname &key (element-type 'simple-char))
  "Read the contents of a file and then parse it as XML."
  (flet ((slurp ()
           (with-open-file (s pathname :direction :input :element-type element-type)
             (let ((string (make-array (file-length s) :element-type element-type :fill-pointer t)))
               (prog1 string
                 (setf (fill-pointer string) (read-sequence string s)))))))
    (xml-parse context (slurp) pathname)))

;;; ----------------------------------------------------

(defun xml-load-doc (pathname &key (element-type 'simple-char))
  "Read the contents of a file and then parse it as XML into a document context."
  (xml-load (make-instance 'xml-doc:xml-doc) pathname :element-type element-type))
