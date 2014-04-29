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

   ;; single-element searches
   #:find-xml
   #:find-attribute

   ;; multi-element searches
   #:query-xml

   ;; doc accessors
   #:doc-source
   #:doc-decl
   #:doc-type
   #:doc-prolog
   #:doc-root

   ;; prolog accessors
   #:prolog-stylesheets
   #:prolog-entities

   ;; xml node accessors
   #:node-name
   #:node-ns
   #:node-doc
   #:node-parent
   #:node-attributes
   #:node-value
   #:node-elements))

(in-package :xml)

(defconstant +xml-entities+
  `(("quot" "\"")
    ("apos" "'")
    ("lt"   "<")
    ("gt"   ">")
    ("amp"  "&")

    ;; not in the spec; very common
    ("copy"  ,(string (code-char 169)))
    ("reg"   ,(string (code-char 174)))
    ("cent"  ,(string (code-char 162)))
    ("pound" ,(string (code-char 163)))
    ("yen"   ,(string (code-char 165)))
    ("euro"  ,(string (code-char 8364)))
    ("trade" ,(string (code-char 8424)))
    ("ndash" ,(string (code-char 8211)))
    ("mdash" ,(string (code-char 8212)))
    ("lsquo" ,(string (code-char 8216)))
    ("rsquo" ,(string (code-char 8217)))
    ("sbquo" ,(string (code-char 8218)))
    ("ldquo" ,(string (code-char 8220)))
    ("rdquo" ,(string (code-char 8221)))
    ("bdquo" ,(string (code-char 8222)))))

(defclass doc ()
  ((source   :initarg :source   :accessor doc-source)
   (decl     :initarg :decl     :accessor doc-decl)
   (doctype  :initarg :doctype  :accessor doc-type)
   (prolog   :initarg :prolog   :accessor doc-prolog)
   (encoding :initarg :encoding :accessor doc-encoding)
   (root     :initarg :root     :accessor doc-root))
  (:documentation "XML prolog, entity macros, and root tag."))

(defclass prolog ()
  ((stylesheets :initarg :stylesheets :accessor prolog-stylesheets)
   (entities    :initarg :entities    :accessor prolog-entities))
  (:documentation "XML prolog declarations."))

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
  ((xml doc root)
   `(,@$1 ,$2))

  ;; invalid xml
  ((xml :error)
   (error "No root tag"))

  ;; document
  ((doc decl prolog)
   `(,$1 ,@$2))
  ((doc prolog)
   `(nil ,@$1))

  ;; <?xml ... ?>
  ((decl :xml attrs :decl-gt) $2)

  ;; doctype, stylesheets, and entities tags
  ((prolog misc doctype misc)
   `(,$2 (,@$1 ,@$3)))
  ((prolog misc)
   `(nil ,$1))

  ;; stylesheets and entities
  ((misc stylesheet misc)
   `(,$1 ,@$2))
  ((misc entity misc)
   `(,$1 ,@$2))
  ((misc)
   `())

  ;; <!doctype ...>
  ((doctype :doctype :id :id :quot :quot :gt)
   `(:doctype (,$2 ,$3 ,$4 ,$5)))
  ((doctype :doctype :id :id :quot :gt)
   `(:doctype (,$2 ,$3 ,$4)))
  ((doctype :doctype :id :gt)
   `(:doctype (,$2)))

  ;; <?xml-stylesheet ... ?>
  ((stylesheet :xml-stylesheet attrs :decl-gt)
   `(:stylesheet ,$2))

  ;; <!entity id "value">
  ((entity :entity :id :quot :gt)
   `(:entity (,$2 ,$3)))

  ;; root tag
  ((root :tag tag) $2)

  ;; tags with child elements
  ((tag :id :ns :id attrs :inner-xml inner-xml)
   `((,$3 ,$1) ,$4 ,$6))
  ((tag :id attrs :inner-xml inner-xml)
   `((,$1) ,$2 ,$4))

  ;; tags without child elements
  ((tag :id :ns :id attrs :end-tag)
   `((,$3 ,$1) ,$4 ((:close-tag (,$3 ,$1)))))
  ((tag :id attrs :end-tag)
   `((,$1) ,$2 ((:close-tag (,$1)))))

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
   `((,$3 ,$1) ,$5))
  ((attr :id :eq :quot)
   `((,$1) ,$3))

  ;; inner xml
  ((inner-xml element inner-xml)
   `(,$1 ,@$2))
  ((inner-xml :cdata cdata inner-xml)
   `(,@$2 ,@$3))

  ;; close tags
  ((inner-xml :close-tag :id :end-tag)
   `((:close-tag (,$2))))
  ((inner-xml :close-tag :id :ns :id :end-tag)
   `((:close-tag (,$4 ,$2))))

  ;; inner xml elements
  ((element :tag tag)
   `(:tag ,$2))
  ((element :text)
   `(:text ,$1))

  ;; unknown child element
  ((element :error)
   (error "Invalid XML"))

  ;; character data
  ((cdata :text cdata)
   `((:cdata ,$1) ,@$2))
  ((cdata :end-cdata)
   `()))

(defconstant +ref-re+ (compile-re "&((#?x?)([^;]+));")
  "Compiled pattern used for replacing inner-text entity references.")

(defun replace-refs (prolog string)
  "Replace all &ref; references with their counterparts."
  (flet ((expand (m)
           (with-re-match (m m)
             (cond
              ((string= "#"  $2) (code-char (parse-integer $3)))
              ((string= "#x" $2) (code-char (parse-integer $3 :radix 16)))
              (t                 (let ((e (second (assoc $1 (prolog-entities prolog) :test #'string=))))
                                   (if e
                                       e
                                     (prog1 $1 (warn "Unrecognized entity ~s" $1)))))))))
    (replace-re +ref-re+ #'expand string :all t)))

(defun normalize (string)
  "Remove whitespace from the beginning and ends of a string."
  (string-trim '(#\space #\tab #\return #\linefeed #\newline) string))

(defun write-inner-text (doc tag text &optional cdata)
  "Write CDATA or inner text to a tag's inner-text stream."
  (let ((s (if (eq (doc-encoding doc) :latin-1)
               text
             (let ((bytes (map 'vector #'char-code text)))
               (external-format:decode-external-string bytes (doc-encoding doc))))))
    (princ (if cdata s (replace-refs (doc-prolog doc) s)) (node-value tag))))

(defun close-tag (tag name &optional (ns (node-ns tag)))
  "Validate that the close tag matches the open tag."
  (unless (and (equal (node-name tag) name)
               (equal (node-ns tag) ns))
    (error "Close tag \"~@[~a:~]~a\" does not match ~a" ns name tag))

  ;; reverse the elements and flush the inner-text stream
  (setf (node-value tag) (normalize (get-output-stream-string (node-value tag)))
        (node-elements tag) (reverse (node-elements tag))))

(defun make-attribute (doc key value)
  "Create a new attribute element to add to a tag or document."
  (destructuring-bind (name &optional ns)
      key
    (make-instance 'attribute
                   :name name
                   :ns ns
                   :doc doc
                   :value (normalize (replace-refs (doc-prolog doc) value)))))

(defun make-tag (doc parent tag-form)
  "Evaluate a tag form."
  (destructuring-bind ((name &optional ns) attrs inner-forms)
      tag-form
    (let ((tag (make-instance 'tag
                              :name name
                              :ns ns
                              :attributes (loop :for a :in attrs :collect (apply #'make-attribute doc a))
                              :doc doc
                              :parent parent
                              :elements nil
                              :inner-text (make-string-output-stream :element-type 'character))))

      ;; evaluate the inner forms
      (loop :for (kind value) :in inner-forms
            :finally (return tag)
            :do (case kind
                  (:tag             (push (funcall #'make-tag doc tag value) (node-elements tag)))

                  ;; inner text data
                  (:cdata           (write-inner-text doc tag value t))
                  (:text            (write-inner-text doc tag value))

                  ;; create a child tag...
                  (:close-tag       (apply #'close-tag tag value)))))))

(defun make-prolog (prolog)
  "Evaluate the document prolog forms."
  (let ((entities +xml-entities+)
        (stylesheets))
    (loop :for (kind value) :in prolog
          :do (case kind
                (:entity     (push value entities))
                (:stylesheet (push value stylesheets))))

    ;; create the prolog from the built up lists
    (make-instance 'prolog :stylesheets stylesheets :entities entities)))

(defun encoding-of-xml-decl (decl)
  "Return the external format for the xml declaration."
  (when-let (encoding (assoc "encoding" decl :test #'string-equal :key #'first))
    (cond
     ((string-equal (second encoding) "utf-8") :utf-8)
     ((string-equal (second encoding) "iso-8859-1") :latin-1)
     ((string-equal (second encoding) "shift_jis") :sjis)
     ((string-equal (second encoding) "x-mac-roman") :macos-roman)
     ((string-equal (second encoding) "euc-jp" :euc-jp))
     ((string-equal (second encoding) "jis") :jis))))

(defun parse-xml (string &optional source)
  "Convert an XML string into a Lisp object."
  (destructuring-bind (decl doctype prolog root)
      (parse #'xml-parser (tokenize #'prolog-lexer string source))
    (let ((doc (make-instance 'doc
                              :source source
                              :decl decl
                              :doctype doctype
                              :encoding (or (encoding-of-xml-decl decl) :latin-1)
                              :prolog (make-prolog prolog))))
      (prog1
          doc
        (setf (doc-root doc) (make-tag doc nil root))))))

(defmethod find-xml ((tag tag) xpath)
  "Returns the first child to match xpath."
  (labels ((query (tag xpath)
             (destructuring-bind (name &rest rest)
                 xpath
               (prog ((child (member name (node-elements tag) :test #'string-equal :key #'node-name)))
                 next-child

                 ;; no child found
                 (unless child
                   (return-from find-xml))

                 ;; at the end of the xpath
                 (unless rest
                   (return-from find-xml (first child)))

                 ;; scan the rest of the path
                 (query (first child) rest)

                 ;; find the next child tree
                 (setf child (member name (rest child) :test #'string-equal :key #'node-name))

                 ;; repeat
                 (go next-child)))))
    (query tag (split-sequence "/" xpath :coalesce-separators t))))

(defmethod find-xml ((doc doc) xpath)
  "Returns the first child to match xpath."
  (find-xml (make-instance 'tag :elements (list (doc-root doc))) xpath))

(defmethod query-xml ((tag tag) xpath)
  "Recursively descend into a tag finding child tags with a given path."
  (labels ((query (tag xpath)
             (destructuring-bind (name &rest rest)
                 xpath
               (let ((qs (remove-if-not #'(lambda (e) (string-equal (node-name e) name)) (node-elements tag))))
                 (if (null rest)
                     qs
                   (loop :for q :in qs :append (query q rest)))))))
    (query tag (split-sequence "/" xpath :coalesce-separators t))))

(defmethod query-xml ((doc doc) xpath)
  "Recursively descend into a document finding all child tags at a given path."
  (query-xml (make-instance 'tag :elements (list (doc-root doc))) xpath))

(defmethod find-attribute ((tag tag) name)
  "Search for an attribute in a tag."
  (find name (node-attributes tag) :key #'node-name :test #'string-equal))

(defmethod find-attribute ((doc doc) name)
  "Search for an attribute in the XML declaration."
  (find name (doc-decl doc) :key #'node-name :test #'string-equal))