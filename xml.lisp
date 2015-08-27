;;;; Non-validating, Lightweight XML parser for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
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
  (:use :cl :parse :re :lexer :markup)
  (:export
   #:xml-parse
   #:xml-load

   ;; document searching
   #:xml-query
   #:xml-query-attribute

   ;; document accessors
   #:xml-doc-source
   #:xml-doc-version
   #:xml-doc-encoding
   #:xml-doc-doctype
   #:xml-doc-root

   ;; external reference accessors
   #:xml-ref-system-uri
   #:xml-ref-public-id

   ;; doctype accessors
   #:xml-doctype-root

   ;; parsed node accessors
   #:xml-node-doc
   #:xml-node-name
   #:xml-node-value

   ;; parsed tag accessors
   #:xml-tag-elements
   #:xml-tag-parent
   #:xml-tag-attributes

   ;; utility functions
   #:xml-name-char-p
   #:xml-token-char-p))

(in-package :xml)

;;; ----------------------------------------------------

(defvar *xml-doc* nil
  "The current document being parsed.")

;;; ----------------------------------------------------

(defclass xml-doc ()
  ((source     :initarg :source     :accessor xml-doc-source)
   (version    :initarg :version    :accessor xml-doc-version)
   (encoding   :initarg :encoding   :accessor xml-doc-encoding)
   (doctype    :initarg :doctype    :accessor xml-doc-doctype)
   (root       :initarg :root       :accessor xml-doc-root))
  (:documentation "Basic XML document parsing context."))

;;; ----------------------------------------------------

(defclass xml-ref ()
  ((public-id  :initarg :public-id  :accessor xml-ref-public-id)
   (system-uri :initarg :system-uri :accessor xml-ref-system-uri))
  (:documentation "A SYSTEM or PUBLIC external reference."))

;;; ----------------------------------------------------

(defclass xml-doctype (xml-ref)
  ((root       :initarg :root       :accessor xml-doctype-root)
   (entities   :initarg :entities   :accessor xml-doctype-entities))
  (:documentation "DOCTYPE declaration."))

;;; ----------------------------------------------------

(defclass xml-node ()
  ((doc        :initarg :doc        :accessor xml-node-doc)
   (name       :initarg :name       :accessor xml-node-name)
   (value      :initarg :value      :accessor xml-node-value))
  (:documentation "A generic element."))

;;; ----------------------------------------------------

(defclass xml-tag (xml-node)
  ((elts       :initarg :elements   :accessor xml-tag-elements)
   (parent     :initarg :parent     :accessor xml-tag-parent)
   (atts       :initarg :attributes :accessor xml-tag-attributes))
  (:documentation "An XML tag with attributes and inner-text value."))

;;; ----------------------------------------------------

(defclass xml-attribute (xml-node)
  ()
  (:documentation "An attribute key/value pair."))

;;; ----------------------------------------------------

(defmethod print-object ((doc xml-doc) stream)
  "Output a document to a stream."
  (print-unreadable-object (doc stream :type t)
    (prin1 (xml-node-name (xml-doc-root doc)) stream)))

;;; ----------------------------------------------------

(defmethod print-object ((node xml-node) stream)
  "Output a node to a stream."
  (print-unreadable-object (node stream :type t)
    (prin1 (xml-node-name node) stream)))

;;; ----------------------------------------------------

(defun xml-name-char-p (c)
  "T if the c is a valid XML name character."
  (or (char= c #\:)
      (char= c #\_)

      ;; any ascii letter
      (char<= #\A c #\Z)
      (char<= #\a c #\z)

      ;; acceptable unicode character ranges
      (char<= #\u+00C0 c #\u+00D6)
      (char<= #\u+00D8 c #\u+00F6)
      (char<= #\u+00F8 c #\u+02FF)
      (char<= #\u+0370 c #\u+037D)
      (char<= #\u+037F c #\u+1FFF)
      (char<= #\u+200C c #\u+200D)
      (char<= #\u+2070 c #\u+218F)
      (char<= #\u+2C00 c #\u+2FEF)
      (char<= #\u+3001 c #\u+D7FF)
      (char<= #\u+F900 c #\u+FDCF)
      (char<= #\u+FDF0 c #\u+FFFD)

      ;; extended, UTF characters
      (char<= #\u+10000 c #\u+EFFFF)))

;;; ----------------------------------------------------

(defun xml-token-char-p (c)
  "T if the c is a valid XML token character."
  (or (xml-name-char-p c)

      ;; one-off characters
      (char= c #\-)
      (char= c #\.)
      (char= c #\u+00b7)

      ;; digits
      (char<= #\0 c #\9)

      ;; unicode character ranges
      (char<= #\u+0300 c #\u+036F)
      (char<= #\u+203F c #\u+2040)))

;;; ----------------------------------------------------

(defun xml-external-format (encoding)
  "Return a keyword for the external format for an encoding string."
  (cond ((string-equal encoding "utf-8") :utf-8)
        ((string-equal encoding "utf-16") :utf-16)
        ((string-equal encoding "utf-32") :utf-32)
        ((string-equal encoding "x-mac-roman") :macos-roman)
        ((string-equal encoding "euc-jp") :euc-jp)

        ;; iso-8859-x external formats
        ((string-equal encoding "iso-8859-1") :iso-8859-1)
        ((string-equal encoding "iso-8859-2") :iso-8859-2)
        ((string-equal encoding "iso-8859-3") :iso-8859-3)
        ((string-equal encoding "iso-8859-4") :iso-8859-4)
        ((string-equal encoding "iso-8859-5") :iso-8859-5)
        ((string-equal encoding "iso-8859-6") :iso-8859-6)
        ((string-equal encoding "iso-8859-7") :iso-8859-7)
        ((string-equal encoding "iso-8859-8") :iso-8859-8)
        ((string-equal encoding "iso-8859-9") :iso-8859-9)
        ((string-equal encoding "iso-8859-10") :iso-8859-10)
        ((string-equal encoding "iso-8859-11") :iso-8859-11)
        ((string-equal encoding "iso-8859-12") :iso-8859-12)
        ((string-equal encoding "iso-8859-13") :iso-8859-13)
        ((string-equal encoding "iso-8859-14") :iso-8859-14)
        ((string-equal encoding "iso-8859-15") :iso-8859-15)
        ((string-equal encoding "iso-8859-16") :iso-8859-16)

        ;; default to utf-8
        (t :utf-8)))

;;; ----------------------------------------------------

(define-lexer xml-lexer (s)

  ;; xml declaration
  ("^<%?xml[%s%n]+"
   (push-lexer s 'xml-decl-lexer :xml))

  ;; skip whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; document type declaration
  ("<!DOCTYPE[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-doctype-lexer :doctype))

  ;; processing instruction
  ("<%?(%:xml-name-char-p:%:xml-token-char-p:*)[%s%n]+(.-)%?>"
   (values :processing-instruction (list $1 $2)))

  ;; root tag
  ("<(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-attribute-lexer :tag $1)))

;;; ----------------------------------------------------

(define-lexer xml-decl-lexer (s)

  ;; skip whitespace
  ("[%s%n]+" :next-token)

  ;; declaration attributes
  ("version[%s%n]*=[%s%n]*(?'(.-)'|\"(.-)\")" (values :version $1))
  ("encoding[%s%n]*=[%s%n]*(?'(.-)'|\"(.-)\")" (values :encoding $1))
  ("standalone[%s%n]*=[%s%n]*(?'(.-)'|\"(.-)\")" (values :standalone $1))

  ;; end of xml declaration
  ("%?>" (pop-lexer s :end-xml)))

;;; ----------------------------------------------------

(define-lexer xml-doctype-lexer (s)

  ;; external references
  ("[%s%n]+SYSTEM[%s%n]+(?'(.-)'|\"(.-)\")"
   (values :system (list $1)))
  ("[%s%n]+PUBLIC[%s%n]+(?'(.-)'|\"(.-)\")[%s%n]+(?'(.-)'|\"(.-)\")"
   (values s :public (list $2 $1)))

  ;; internal document type declaration
  ("[%s%n]*%[" (push-lexer s 'xml-dtd-lexer :dtd))

  ;; end of doctype
  ("[%s%n]*>" (pop-lexer s (print :end-doctype))))

;;; ----------------------------------------------------

(define-lexer xml-dtd-lexer (s)

  ;; skip whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; processing instruction
  ("<%?(%:xml-name-char-p:%:xml-token-char-p:*)[%s%n]+(.-)%?>"
   (values :processing-instruction (list $1 $2)))

  ;; skip parameter entities, and warn
  ("%%(%:xml-name-char-p:%:xml-token-char-p:*);"
   (prog1 :next-token
     (warn "Skipping PE reference in DTD ~s..." $1)))

  ;; entity tags
  ("<!ENTITY[%s%n]+%%[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-dtd-element-lexer :dtd-element))
  ("<!ENTITY[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-entity-lexer :entity-decl))

  ;; non-entity declarations (non-validating, so skip)
  ("<!(%u+)[%s%n]+" (push-lexer s 'xml-dtd-element-lexer :dtd-element $1))

  ;; end of internal document type declaration
  ("%]" (pop-lexer s :end-dtd)))

;;; ----------------------------------------------------

(define-lexer xml-entity-lexer (s)

  ;; external references
  ("[%s%n]+SYSTEM[%s%n]+(?'(.-)'|\"(.-)\")"
   (values :system (list $1)))
  ("[%s%n]+PUBLIC[%s%n]+(?'(.-)'|\"(.-)\")[%s%n]+(?'(.-)'|\"(.-)\")"
   (values :public (list $2 $1)))

  ;; literal values
  ("[%s%n]+(?'(.-)'|\"(.-)\")" (values :value $1))

  ;; unparsed-entities
  ("[%s%n]+NDATA[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (values :ndata $1))

  ;; end of entity
  ("[%s%n]*>" (pop-lexer s :end-dtd-element)))

;;; ----------------------------------------------------

(define-lexer xml-dtd-element-lexer (s)

  ;; end of element declaration
  (">" (pop-lexer s :end-dtd-element))

  ;; skip quoted strings
  ("'.-'|\".-\"" :next-token)

  ;; skip everything else up to the end of the element declaration
  (".[^'\"%[]*" :next-token))

;;; ----------------------------------------------------

(define-lexer xml-attribute-lexer (s)

  ;; end of tag attributes
  ("/>" (pop-lexer s :singleton-tag))
  (">" (swap-lexer s 'xml-inner-lexer :end-tag))

  ;; skip whitespace
  ("[%s%n]+" :next-token)

  ;; attribute and value
  ("(%:xml-name-char-p:%:xml-token-char-p:*)"
   (values :attr $1))
  ("=[%s%n]*(?'(.-)'|\"(.-)\")"
   (values :value $1)))

;;; ----------------------------------------------------

(define-lexer xml-inner-lexer (s)

  ;; skip comments
  ("<!%-%-.-%-%->" :next-token)

  ;; CDATA block
  ("<!%[CDATA%[(.-)%]%]>"
   (values :cdata $1))

  ;; processing instruction
  ("<%?(%:xml-name-char-p:%:xml-token-char-p:*)[%s%n]+(.-)%?>"
   (values :processing-instruction (list $1 $2)))

  ;; end tag
  ("</(%:xml-name-char-p:%:xml-token-char-p:*)[%s%n]*>"
   (pop-lexer s :close-tag $1))

  ;; open child tag
  ("<(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-attribute-lexer :tag $1))

  ;; all other inner text (entities to expand later)
  (".[^<]*" (values :inner-text $$)))

;;; ----------------------------------------------------

(define-parser xml-parser
  "An XML SAX parser."
  (.let* ((decl (.opt nil 'xml-decl-parser))
          (doctype (.opt nil 'xml-doctype-parser))
          (root (.opt nil 'xml-tag-parser)))
    (.ret (list decl doctype root))))

;;; ----------------------------------------------------

(define-parser xml-decl-parser
  "An <?xml..?> declaration."
  (.do (.is :xml)

       ;; read the version
       (.let* ((version (.opt "1.0" (.is :version)))
               (encoding (.opt "utf-8" (.is :encoding)))
               (standalone (.opt "no" (.is :standalone))))

         ;; close the declaration and return
         (.do (.or (.is :end-xml)
                   (.fail "Missing expected '?>' in declaration"))

              ;; return the declaration attributes
              (.ret (list version (xml-external-format encoding)))))))

;;; ----------------------------------------------------

(define-parser xml-doctype-parser
  "The document type declaration."
  (.let* ((root (.is :doctype))

          ;; document can have an optional external reference
          (external-ref (.opt nil (.is :external-ref))))

    ;; check for an internal subset DTD
    (.do (.put (make-instance 'xml-doctype
                              :root root
                              :system-uri (first external-ref)
                              :public-id (second external-ref)))

         ;; check for internal subset
         (.maybe 'xml-dtd-parser)

         ;; finish the doctype or fail
         (.either (.is :end-doctype)
                  (.fail "Missing '>' in DOCTYPE"))

         ;; return the doctype
         (.get))))

;;; ----------------------------------------------------

(define-parser xml-dtd-parser
  "Internal subset document type declaration."
  (.do (.is :dtd)

       ;; read all the elements
       (.skip-many 'xml-dtd-elements-parser)

       ;; end the dtd
       (.either (.is :end-foo)
                (.fail "Missing ']' in DTD."))))

;;; ----------------------------------------------------

(define-parser xml-dtd-elements-parser
  "All the element delcarations of the DTD."
  (.or (.is :processing-instruction)

       ;; entity declarations
       'xml-entity-parser

       ;; other declarations are skipped
       (.do (.is :dtd-element)
            (.is :end-dtd-element))))

;;; ----------------------------------------------------

(define-parser xml-entity-parser
  "Entity declarations in the DTD."
  (.let (name (.is :entity-decl))

    ;; the value can be parsed, unparsed, or an external reference
    (.do (.or (.let* ((value (.is :value))

                      ;; optionally an unparsed NDATA type
                      (ndata (.opt nil (.is :ndata))))

                ;; if there's no NDATA, it is a parsed entity
                (if ndata
                    (.let (doctype (.get))
                      (.ret (push (cons name value)
                                  (xml-doctype-entities doctype))))
                  (.ret nil)))

              ;; external reference?
              (.is :external-ref))

         ;; end the entity
         (.is :end-dtd-element))))

;;; ----------------------------------------------------

(define-parser xml-misc-parser
  "Processing instructions."
  (.skip-many (.is :processing-instruction)))

;;; ----------------------------------------------------

(define-parser xml-tag-parser
  "Tag name, attributes, inner-text, and close tag."
  (.let* ((tag (.is :tag))
          (atts 'xml-attribute-parser))

    ;; check for singleton tag
    (.or (.do (.is :singleton-tag) (.ret (list :tag tag atts)))
         (.do (.is :end-tag)

              ;; parse all the inner xml
              (.let (inner (.many (.or 'xml-tag-parser
                                       'xml-cdata-parser
                                       'xml-inner-text-parser)))

                ;; match the end tag
                (.or (.let (end (.is :close-tag))
                       (if (string= tag end)
                           (.ret (append (list :tag tag atts) inner))
                         (.fail "Tag mismatch; expected ~s got ~s" tag end)))

                     ;; there is no close tag
                     (.fail "Missing close tag ~s" tag)))))))

;;; ----------------------------------------------------

(define-parser xml-attribute-parser
  "Parse all the attributes in a tag."
  (.many (.let* ((att (.is :attr))
                 (value (.is :value)))
           (.ret (list att value)))))

;;; ----------------------------------------------------

(define-parser xml-cdata-parser
  "Parse a CDATA section."
  (.let (data (.is :cdata))
    (.ret (list :cdata data))))

;;; ----------------------------------------------------

(define-parser xml-inner-text-parser
  "Parse inner text."
  (.let (text (.is :inner-text))
    (.ret (list :inner-text text))))

;;; ----------------------------------------------------

(defun expand-entity-refs (string)
  "Use the document entities to expand a string."
  (let ((dtd (xml-doc-doctype *xml-doc*)))
    (markup-decode string :entities (and dtd (xml-doctype-entities dtd)))))

;;; ----------------------------------------------------

(defun build-attribute (att)
  "Construct an xml-attribute, decoding the value."
  (destructuring-bind (k v)
      att
    (make-instance 'xml-attribute
                   :doc *xml-doc*
                   :name k
                   :value (expand-entity-refs v))))

;;; ----------------------------------------------------

(defun build-tag (parent name atts &rest inner-xml)
  "Construct an xml-tag from a parsed form."
  (let ((tag (make-instance 'xml-tag
                            :doc *xml-doc*
                            :name name
                            :value nil
                            :attributes nil
                            :elements nil
                            :parent parent)))
    (prog1 tag

      ;; construct the attributes
      (setf (xml-tag-attributes tag) (mapcar #'build-attribute atts))

      ;; construct the inner-xml
      (loop

         ;; all inner text will be written to a stream
         with inner-text = (make-string-output-stream)

         ;; process inner-xml
         for e in inner-xml
         for etype = (first e)

         ;; collect child tags
         when (eq etype :tag)
         collect (apply 'build-tag tag (rest e))
         into child-tags

         ;; cdata is written as-is
         when (eq etype :cdata)
         do (write-string (second e) inner-text)

         ;; inner text needs to be decoded and written
         when (eq etype :inner-text)
         do (write-string (expand-entity-refs (second e)) inner-text)

         ;; set the child elemnets
         finally (setf (xml-tag-elements tag) child-tags

                       ;; write the output to the tag's value
                       (xml-node-value tag)
                       (get-output-stream-string inner-text))))))

;;; ----------------------------------------------------

(defun build-doc (decl doctype root)
  "Construct the xml-doc from the parsed forms."
  (destructuring-bind (&optional (version "1.0") (encoding :utf-8))
      decl
    (setf (xml-doc-version *xml-doc*) version
          (xml-doc-encoding *xml-doc*) encoding))

  ;; set the doctype if present
  (setf (xml-doc-doctype *xml-doc*) doctype)

  ;; construct the root tag
  (setf (xml-doc-root *xml-doc*)
        (apply 'build-tag nil (rest root))))

;;; ----------------------------------------------------

(defun xml-parse (string &optional source)
  "Parse a string as XML."
  (let ((*xml-doc* (make-instance 'xml-doc :source source)))
;    (prog1 *xml-doc*
      (with-lexer (lexer 'xml-lexer string :source source)
        (with-token-reader (next-token lexer)
          (let ((*markup-entity-start-char-p* 'xml-name-char-p)
                (*markup-entity-char-p* 'xml-token-char-p))
            (parse 'xml-parser next-token))))))

;;; ----------------------------------------------------

(defun xml-load (pathname &rest slurp-args)
  "Read the contents of a file and then parse it as XML."
  (xml-parse (apply #'lex:slurp pathname slurp-args) pathname))

;;; ----------------------------------------------------

(defmethod xml-query ((tag xml-tag) path)
  "Recursively descend into a tag finding child tags with a given path."
  (let* ((path-elts (split-re #r"/" path))

         ;; determine which tags to search (breadth-first)
         (tags (cond ((null path-elts))

                     ;; the first path is empty, start at the root
                     ((string= (first path-elts) "")
                      (prog1 (list (xml-doc-root (xml-node-doc tag)))
                        (pop path-elts)))

                     ;; search this tag
                     (t (list tag)))))

    ;; search each path element
    (do ((path (pop path-elts)
               (pop path-elts)))
        ((null path) tags)

      ;; find all the tags that match this path element
      (setf tags (if (or (string= path "")
                         (string= path "*"))
                     tags
                   (remove path tags :test 'string/= :key 'xml-node-name)))

      ;; if there are more path elements to search, get child tags
      (when path-elts
        (setf tags (loop for tag in tags append (xml-tag-elements tag)))))))

;;; ----------------------------------------------------

(defmethod xml-query ((doc xml-doc) path)
  "Recursively descend into the document to find a given path."
  (xml-query (xml-doc-root doc) path))

;;; ----------------------------------------------------

(defmethod xml-query-attribute ((tag xml-tag) name)
  "Return an attribute with the given name from a tag if found."
  (find name (xml-tag-attributes tag) :test #'string= :key #'xml-node-name))

;;; ----------------------------------------------------

(defmethod xml-query-attribute ((doc xml-doc) name)
  "Return an attribute with the given name from a tag if found."
  (xml-query-attribute (xml-doc-root doc) name))
