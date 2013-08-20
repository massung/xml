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
  (:use :cl :lw :parsergen :re :lexer)
  (:export
   #:parse-xml
   #:parse-xml-file

   ;; element traversal
   #:query-xml
   #:query-attribute

   ;; doc accessors
   #:doc-decl
   #:doc-stylesheets
   #:doc-source
   #:doc-type
   #:doc-entities
   #:doc-root

   ;; xml node accessors
   #:node-name
   #:node-ns
   #:node-doc
   #:node-parent
   #:node-attributes
   #:node-value
   #:node-elements))

(in-package :xml)

(defvar *xml-doc* nil
  "The document being parsed currently.")
(defvar *xml-root* nil
  "The root tag of the current document.")
(defvar *xml-stack* nil
  "The stack of inner tags.")
(defvar *xml-tag* nil
  "The tag being parsed.")

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

(defclass node ()
  ((name :initarg :name :accessor node-name)
   (ns   :initarg :ns   :accessor node-ns)
   (doc  :initarg :doc  :accessor node-doc))
  (:documentation "All tags and attributes are node elements."))

(defclass tag (node)
  ((parent     :initarg :parent     :accessor node-parent)
   (attributes :initarg :attributes :accessor node-attributes)
   (inner-text :initarg :inner-text :accessor node-value)
   (elements   :initarg :elements   :accessor node-elements))
  (:documentation "Attributes, name, inner text, and child tags."))

(defclass attribute (node)
  ((value :initarg :value :accessor node-value :initform nil))
  (:documentation "A single attribute of a tag."))

(defmethod print-object ((doc doc) s)
  "Output a document to a stream."
  (print-unreadable-object (doc s :type t)
    (with-slots (ns name)
        (doc-root doc)
      (format s "~@[~a:~]~a" ns name))))

(defmethod print-object ((node node) s)
  "Output an element to a stream."
  (print-unreadable-object (node s :type t)
    (with-slots (ns name)
        node
      (format s "~@[~a:~]~a" ns name))))

(deflexer prolog-lexer
  ("[%s%n]+"               (values :next-token))

  ;; comments - just tokenize and skip
  ("<!%-%-"                (prog1
                               :next-token
                             (comment-lexer)))

  ;; prolog tags
  ("<%?xml%s+"             (values :xml))
  ("<%?xml%-stylesheet%s+" (values :xml-stylesheet))
  ("<!DOCTYPE%s+"          (values :doctype))
  ("<!ENTITY%s+"           (values :entity))

  ;; root tag
  ("<"                     (push-lexer #'tag-lexer :tag))

  ;; tag terminators
  ("%?>"                   (values :decl-gt))
  (">"                     (values :gt))

  ;; identifiers
  ("%a[%w%-_]*"            (values :id $$))

  ;; attributes
  ("="                     (values :eq))
  (":"                     (values :ns))

  ;; value quoting
  ("\"([^\"]*)\""          (values :quot $1))
  ("'([^']*)'"             (values :quot $1)))

(deflexer comment-lexer
  ("%-%->[%s%n]*")

  ;; skip characters
  ("."                     (values :next-token))

  ;; end of file
  ("$"                     (error "Unterminated comment")))

(deflexer tag-lexer
  ("/>[%s%n]*"             (pop-lexer :end-tag))

  ;; enter tag body
  (">[%s%n]*"              (swap-lexer #'inner-xml-lexer :inner-xml))

  ;; tokens
  ("[%s%n]+"               (values :next-token))
  ("="                     (values :eq))
  (":"                     (values :ns))

  ;; identifiers
  ("%a[%w%-_]*"            (values :id $$))

  ;; value quoting
  ("\"([^\"]*)\""          (values :quot $1))
  ("'([^']*)'"             (values :quot $1)))

(deflexer inner-xml-lexer
  ("<!%-%-"                (prog1
                               :next-token
                             (comment-lexer)))

  ;; tag terminal
  ("</"                    (swap-lexer #'close-tag-lexer :close-tag))

  ;; cdata tag
  ("<!%[CDATA%["           (push-lexer #'cdata-lexer :cdata))

  ;; child tag
  ("<"                     (push-lexer #'tag-lexer :tag))
 
  ;; inner text and coalesced whitespace
  ("[^<%s%n]+"             (values :text $$))
  ("[%s%n]+"               (values :text " ")))

(deflexer close-tag-lexer
  ("[%s%n]+"               (values :next-token))
  ("%a[%w%-_]*"            (values :id $$))
  (":"                     (values :ns))

  ;; finish the tag
  (">[%s%n]*"              (pop-lexer :end-tag)))

(deflexer cdata-lexer
  ("%]%]>"                 (pop-lexer :end-cdata))

  ;; body
  ("%]?[^%]]*"             (values :text $$))

  ;; end of file
  ("$"                     (error "Unterminated CDATA")))

(defparser xml-parser
  ((start xml) $1)

  ;; declaration and prolog
  ((xml decl prolog) `(,$1 ,@$2))
  ((xml prolog) $1)

  ;; <?xml ... ?>
  ((decl :xml attrs :decl-gt)
   (lambda () (push-decl $2)))

  ;; stylesheets, doctypes, and entities tags
  ((prolog stylesheet prolog)
   `(,$1 ,@$2))
  ((prolog doctype prolog)
   `(,$1 ,@$2))
  ((prolog entity prolog)
   `(,$1 ,@$2))

  ;; root element
  ((prolog :tag tag) $2)

  ;; illegal xml
  ((prolog :error)
   (error "No root tag"))

  ;; <?xml-stylesheet ... ?>
  ((stylesheet :xml-stylesheet attrs :decl-gt)
   (lambda () (push-stylesheet $2)))

  ;; <!doctype ...>
  ((doctype :doctype :id :id :quot :quot :gt)
   (lambda () (set-doctype $2 $3 $4 $5)))
  ((doctype :doctype :id :id :quot :gt)
   (lambda () (set-doctype $2 $3 $4)))
  ((doctype :doctype :id :gt)
   (lambda () (set-doctype $2)))

  ;; <!entity id "value">
  ((entity :entity :id :quot :gt)
   (lambda () (push-entity $2 $3)))

  ;; tags with child elements
  ((tag :id :ns :id attrs :inner-xml inner-xml)
   `(,(lambda () (push-tag $3 $4 $1)) ,@$6))
  ((tag :id attrs :inner-xml inner-xml)
   `(,(lambda () (push-tag $1 $2)) ,@$4))

  ;; tags without child elements
  ((tag :id :ns :id attrs :end-tag)
   `(,(lambda () (append-tag $3 $4 $1))))
  ((tag :id attrs :end-tag)
   `(,(lambda () (append-tag $1 $2))))

  ;; illegal xml
  ((tag :error)
   (error "Malformed tag"))

  ;; attribute list
  ((attrs attr attrs)
   `(,$1 ,@$2))
  ((attrs)
   `())

  ;; attribute
  ((attr :id :ns :id :eq :quot)
   `(,$3 ,$5))
  ((attr :id :eq :quot)
   `(,$1 ,$3))

  ;; inner xml
  ((inner-xml element inner-xml)
   `(,@$1 ,@$2))

  ;; close tags
  ((inner-xml :close-tag :id :end-tag)
   `(,(lambda () (pop-tag $2))))
  ((inner-xml :close-tag :id :ns :id :end-tag)
   `(,(lambda () (pop-tag $4 $2))))

  ;; child tags
  ((element :tag tag) $2)
  ((element :cdata cdata) $2)

  ;; inner text
  ((element :text)
   `(,(lambda () (push-text $1))))

  ;; character data
  ((cdata :text cdata)
   `(,(lambda () (push-text $1 :cdata t)) ,@$2))
  ((cdata :end-cdata)
   `())

  ;; unknown
  ((element :error)
   (error "Invalid XML")))

(defun replace-refs (string)
  "Replace all &ref; references with their counterparts."
  (flet ((deref (m)
           (let ((ref (first (match-groups m))))
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

(defun push-entity (key value)
  "Add another entity reference to the document."
  (push (list key value) (doc-entities *xml-doc*)))

(defun set-doctype (root &rest dtds)
  "Set the xml document type for the current document."
  (setf (doc-type *xml-doc*) (cons root dtds)))

(defun push-tag (name attrs &optional ns)
  "Set the current tag being parsed."
  (let ((tag (make-tag name attrs ns)))
    (when *xml-tag*
      (push tag (node-elements *xml-tag*)))
    (push *xml-tag* *xml-stack*)

    ;; create a new tag to house child and text elements
    (setf *xml-tag* tag)))

(defun pop-tag (name &optional ns)
  "Shift from the parse stack to the current tag and validate."
  (unless (and (string-equal name (node-name *xml-tag*))
               (or (null ns)
                   (string-equal ns (node-ns *xml-tag*))))
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

(defun push-text (text &key cdata)
  "Write text from the document to the inner text of the current tag."
  (princ (if cdata text (replace-refs text)) (node-value *xml-tag*)))

(defun parse-xml (string &optional source)
  "Convert an XML string into a Lisp object."
  (let ((*xml-doc* (make-instance 'doc :source source))
        (*xml-tag*))

    ;; build the document and tags
    (let ((doc (parse #'xml-parser (tokenize #'prolog-lexer string source))))
      (dolist (f doc)
        (funcall f)))

    ;; complete the document and return the root element
    (prog1
        *xml-doc*
      (setf (doc-root *xml-doc*) *xml-root*))))

(defun parse-xml-file (pathname)
  "Read the contents of a file and parse it as XML."
  (parse-xml (slurp pathname) pathname))

(defmethod query-xml ((tag tag) xpath)
  "Recursively descend into a tag finding child tags with a given path."
  (labels ((query (tag xpath)
             (destructuring-bind (name &rest rest)
                 xpath
               (let ((qs (loop :for e :in (node-elements tag)
                               :when (string-equal (node-name e) name)
                               :collect e)))
                 (if (null rest)
                     qs
                   (loop :for q :in qs :nconc (query q rest)))))))
    (query tag (split-sequence "/" xpath :coalesce-separators t))))

(defmethod query-xml ((doc doc) xpath)
  "Recursively descend into a document finding all child tags at a given path."
  (query-xml (make-instance 'tag :elements (list (doc-root doc))) xpath))

(defmethod query-attribute ((tag tag) name)
  "Search for an attribute in a tag."
  (find name (node-attributes tag) :key #'node-name :test #'string-equal))

(defmethod query-attribute ((doc doc) name)
  "Search for an attribute in the XML declaration."
  (find name (doc-decl doc) :key #'node-name :test #'string-equal))