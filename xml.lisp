;;;; XML parser for LispWorks
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :xml
  (:use :cl :lw :parsergen :lexer)
  (:export
   #:parse-xml
   #:parse-xml-file

   ;; element traversal
   #:query-xml

   ;; attribute traversal
   #:get-attribute

   ;; doc accessors
   #:doc-decl
   #:doc-stylesheets
   #:doc-source
   #:doc-type
   #:doc-entities
   #:doc-root

   ;; element accessors
   #:element-name
   #:element-ns
   #:element-doc

   ;; tag accessors
   #:tag-parent
   #:tag-attributes
   #:tag-text
   #:tag-elements

   ;; attribute accessors
   #:attrib-name
   #:attrib-ns
   #:attrib-value))

(in-package :xml)

(defvar *xml-doc* nil
  "The document being parsed currently.")
(defvar *xml-root* nil
  "The root tag of the current document.")
(defvar *xml-tag* nil
  "The tag being parsed.")
(defvar *xml-stack* nil
  "The stack of inner tags.")
(defvar *xml-tokens* nil
  "The parsed closures used to build the document.")

(defconstant +xml-keywords+
  '(("xml"            . :xml)
    ("xml-stylesheet" . :xml-stylesheet)
    ("doctype"        . :doctype)
    ("entity"         . :entity)))

(defconstant +xml-entities+
  '(("quot" "\"")
    ("apos" "'")
    ("lt"   "<")
    ("gt"   ">")
    ("amp"  "&")))

(defclass doc ()
  ((decl        :initarg :decl        :accessor doc-decl        :initform nil)
   (stylesheets :initarg :stylesheets :accessor doc-stylesheets :initform nil)
   (source      :initarg :source      :accessor doc-source      :initform nil)
   (doctype     :initarg :types       :accessor doc-type        :initform nil)
   (entities    :initarg :entities    :accessor doc-entities    :initform +xml-entities+)
   (root        :initarg :root        :accessor doc-root        :initform nil))
  (:documentation "XML prolog, entity macros, and root tag."))

(defclass element ()
  ((name :initarg :name :accessor element-name)
   (ns   :initarg :ns   :accessor element-ns)
   (doc  :initarg :doc  :accessor element-doc))
  (:documentation "All tags and attributes are elements."))

(defclass tag (element)
  ((parent     :initarg :parent     :accessor tag-parent)
   (attributes :initarg :attributes :accessor tag-attributes)
   (inner-text :initarg :inner-text :accessor tag-text)
   (elements   :initarg :elements   :accessor tag-elements))
  (:documentation "Attributes, name, inner text, and child tags."))

(defclass attribute (element)
  ((value      :initarg :value :accessor attrib-value :initform nil))
  (:documentation "A single attribute of a tag."))

(defmethod print-object ((doc doc) s)
  "Output a document to a stream."
  (print-unreadable-object (doc s :type t)
    (with-slots (ns name)
        (doc-root doc)
      (format s "~@[~a:~]~a" ns name))))

(defmethod print-object ((e element) s)
  "Output an element to a stream."
  (print-unreadable-object (e s :type t)
    (with-slots (ns name)
        e
      (format s "~@[~a:~]~a" ns name))))

(deflexer xml-lexer (:case-fold t :multi-line t)
  ("<!%-%-"             (prog1
                            :next-token
                          (comment-lexer)))

  ;; tag
  ("<"                  (prog1
                            :lt
                          (setf *lexer* #'tag-lexer)))

  ;; inner text
  ("[^<%s%n]+"          (values :text $$))

  ;; whitespace is coalesced
  ("[%s%n]+"            (values :text " ")))

(deflexer comment-lexer ()
  ("%-%->[%s%n]*")

  ;; skip characters
  ("."                  :next-token)

  ;; end of document
  ("$"                  (error "Unterminated comment")))

(deflexer cdata-lexer ()
  ("%]%]>[%s%n]*"       (prog1
                            :end-cdata
                          (setf *lexer* #'xml-lexer)))

  ;; any other character sequence
  ("%]?[^%]]*"          (values :text $$))

  ;; end of document
  ("$"                  (error "Unterminated CDATA")))

(deflexer tag-lexer (:case-fold t :multi-line t)
  ("[%s%n]+"            :next-token)
  ("/"                  :end)
  ("%?"                 :decl)
  ("!"                  :bang)
  ("%-"                 :dash)
  ("="                  :eq)
  (":"                  :ns)

  ;; character data
  ("%[CDATA%["          (prog1
                            :cdata
                          (setf *lexer* #'cdata-lexer)))

  ;; tag terminal
  (">[%s%n]*"           (prog1
                            :gt
                          (setf *lexer* #'xml-lexer)))

  ;; attribute quoting
  ("\"([^\"]*)\""       (values :quot $1))
  ("'([^']*)'"          (values :quot $1))

  ;; tag and attribute names
  ("%a[%w%-_]*"         (let ((kw (assoc $$ +xml-keywords+ :test #'string-equal)))
                          (if kw (cdr kw) (values :id $$)))))

(defparser xml-parser
  ((start xml) $1)

  ;; document
  ((xml :text xml)
   (if (string= $1 " ")
       $2
     (error "Text outside root tag")))

  ;; prolog, entities, and root tag
  ((xml decl xml)
   `(,$1 ,@$2))
  ((xml stylesheet xml)
   `(,$1 ,@$2))
  ((xml doctype xml)
   `(,$1 ,@$2))
  ((xml entity xml)
   `(,$1 ,@$2))
  ((xml tag)
   `(,@$1))

  ;; <?xml ...?>
  ((decl :lt :decl :xml attrs :decl :gt)
   (lambda () (push-decl $4)))

  ;; <?xml-stylesheet ...?>
  ((stylesheet :lt :decl :xml-stylesheet attrs :decl :gt)
   (lambda () (push-stylesheet $4)))

  ;; <!doctype ...>
  ((doctype :lt :bang :doctype :id :id :quot :quot :gt)
   (lambda () (set-doctype $4 $5 $6 $7)))
  ((doctype :lt :bang :doctype :id :id :quot :gt)
   (lambda () (set-doctype $4 $5 $6)))
  ((doctype :lt :bang :doctype :id)
   (lambda () (set-doctype $4)))

  ;; <!entity ...>
  ((entity :lt :bang :entity :id :quot :gt)
   (lambda () (push-entity $4 $5)))

  ;; <tag attributes .../>
  ((tag :lt :id :ns :id attrs :end :gt)
   `(,(lambda () (append-tag $4 $5 $2))))
  ((tag :lt :id attrs :end :gt)
   `(,(lambda () (append-tag $2 $3))))

  ;; <tag attributes ...>
  ((tag :lt :id :ns :id attrs :gt inner-xml)
   `(,(lambda () (push-tag $4 $5 $2)) ,@$7))
  ((tag :lt :id attrs :gt inner-xml)
   `(,(lambda () (push-tag $2 $3)) ,@$5))

  ;; tag error
  ((tag :error)
   (error "Illegal XML"))

  ;; inner text
  ((inner-xml :text inner-xml)
   `(,(lambda () (push-text $1)) ,@$2))

  ;; child tags
  ((inner-xml :lt :end :id :ns :id :gt)
   `(,(lambda () (pop-tag $5 $3))))
  ((inner-xml :lt :end :id :gt)
   `(,(lambda () (pop-tag $3))))
  ((inner-xml tag inner-xml)
   `(,@$1 ,@$2))

  ;; character data
  ((inner-xml :lt :bang :cdata cdata inner-xml)
   `(,@$4 ,@$5))

  ;; block of binary character data
  ((cdata :text cdata)
   `(,(lambda () (push-cdata $1)) ,@$2))
  ((cdata :end-cdata)
   `())

  ;; inner-tag attributes
  ((attrs attr attrs)
   `(,$1 ,@$2))
  ((attrs)
   `())

  ;; single attribute
  ((attr :id :ns :id :eq :quot)
   `(,$3 ,$5 ,$1))
  ((attr :id :eq :quot)
   `(,$1 ,$3)))

(defun replace-refs (string)
  "Replace all &ref; references with their counterparts."
  (flet ((deref (m)
           (let ((ref (first (match-captures m))))
             (if (char= (char ref 0) #\#)
                 (let ((n (if (char-equal (char ref 1) #\x)
                              (parse-integer (subseq ref 2) :radix 16)
                            (parse-integer (subseq ref 1)))))
                   (string (code-char n)))
               (let ((e (assoc ref (doc-entities *xml-doc*) :test #'string-equal)))
                 (if (null e)
                     (prog1
                         ref
                       (warn "Unrecognized entity ~s" ref))
                   (second e)))))))
    (replace-re #.(compile-re "&([^;]+);") #'deref string :all t)))

(defun make-attribute (name value &optional ns)
  "Create a new attribute element to add to a tag or document."
  (make-instance 'attribute
                 :name name
                 :ns ns
                 :doc *xml-doc*
                 :value (replace-refs value)))

(defun make-tag (name attrs &optional ns)
  "Create a new tag to push onto the stack."
  (make-instance 'tag
                 :name name
                 :ns ns
                 :attributes (loop :for a :in attrs :collect (apply #'make-attribute a))
                 :doc *xml-doc*
                 :parent *xml-tag*
                 :elements ()
                 :inner-text (make-string-output-stream :element-type 'character)))

(defun push-decl (attrs)
  "Add attribute to the document declaration."
  (dolist (attr attrs)
    (push (apply #'make-attribute attr) (doc-decl *xml-doc*))))

(defun push-stylesheet (attrs)
  "Add attributes to the document stylesheet."
  (dolist (attr attrs)
    (push (apply #'make-attribute attr) (doc-stylesheets *xml-doc*))))

(defun set-doctype (root &rest dtds)
  "Set the xml document type for the current document."
  (setf (doc-type *xml-doc*) (cons root dtds)))

(defun push-entity (key value)
  "Add another entity reference to the document."
  (push (list key value) (doc-entities *xml-doc*)))

(defun push-tag (name attrs &optional ns)
  "Set the current tag being parsed."
  (let ((tag (make-tag name attrs ns)))
    (when *xml-tag*
      (push tag (tag-elements *xml-tag*)))
    (push *xml-tag* *xml-stack*)

    ;; create a new tag to house child and text elements
    (setf *xml-tag* tag)))

(defun pop-tag (name &optional ns)
  "Shift from the parse stack to the current tag and validate."
  (unless (and (string-equal name (element-name *xml-tag*))
               (or (string-equal ns (element-ns *xml-tag*))
                   (null ns)))
    (error "Close tag ~@[~a:~]~a does not match ~a" ns name *xml-tag*))

  ;; fix up the current tag (inner text and elements)
  (with-slots (inner-text elements)
      *xml-tag*
    (setf inner-text (get-output-stream-string inner-text)
          elements (reverse elements)))

  ;; set the root element and pop the top stack item
  (setf *xml-root* *xml-tag* *xml-tag* (pop *xml-stack*)))

(defun append-tag (name attrs &optional ns)
  "Push and pop the same tag at once."
  (push-tag name attrs ns)
  (pop-tag name))

(defun push-text (text)
  "Write text from the document to the inner text of the current tag."
  (princ (replace-refs text) (tag-text *xml-tag*)))

(defun push-cdata (text)
  "Write CDATA text to the inner text of the current tag."
  (princ text (tag-text *xml-tag*)))

(defun parse-xml (string &optional source)
  "Convert an XML string into a Lisp object."
  (let ((*xml-doc* (make-instance 'doc :source source))
        (*xml-tokens* (with-lexbuf (string source)
                        (parse #'xml-parser #'xml-lexer))))

    ;; build the document and tags
    (dolist (f *xml-tokens*)
      (funcall f))

    ;; complete the document and return the root element
    (prog1
        *xml-doc*
      (setf (doc-root *xml-doc*) *xml-root*))))

(defun parse-xml-file (pathname)
  "Read the contents of a file and parse it as XML."
  (with-open-file (f pathname)
    (let ((s (make-array (file-length f) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer s) (read-sequence s f))
      (parse-xml s pathname))))

(defmethod query-xml ((tag tag) xpath)
  "Recursively descend into a tag finding child tags with a given path."
  (labels ((query (tag xpath)
             (destructuring-bind (name &rest rest)
                 xpath
               (let ((qs (loop :for e :in (tag-elements tag)
                               :when (string-equal (element-name e) name)
                               :collect e)))
                 (if (null rest)
                     qs
                   (loop :for q :in qs :nconc (query q rest)))))))
    (query tag (split-sequence "/" xpath :coalesce-separators t))))

(defmethod query-xml ((doc doc) xpath)
  "Recursively descend into a document finding all child tags at a given path."
  (query-xml (make-instance 'tag :elements (list (doc-root doc))) xpath))

(defmethod get-attribute ((tag tag) name)
  "Search for an attribute in a tag."
  (find name (tag-attributes tag) :key #'element-name :test #'string-equal))

(defmethod get-attribute ((doc doc) name)
  "Search for an attribute in the XML declaration."
  (find name (doc-decl doc) :key #'element-name :test #'string-equal))