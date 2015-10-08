;;;; Non-validating, Lightweight XML parser for ClozureCL
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
  (:use :cl :ccl :parse :re :rfc-date :lexer :markup)
  (:export

   ;; parsing, loading, and reading
   #:xml-parse
   #:xml-load
   #:xml-read

   ;; queries
   #:xml-query-compile
   #:xml-query

   ;; query variables
   #:%node
   #:%position
   #:%parent
   #:%name
   #:%text

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
   #:xml-doctype-entities

   ;; generic node accessors
   #:xml-node-namespace
   #:xml-node-doc
   #:xml-node-parent
   #:xml-node-name
   #:xml-node-value

   ;; entity accessors
   #:xml-entity-ndata

   ;; parsed tag accessors
   #:xml-tag-elements
   #:xml-tag-attributes

   ;; utility functions
   #:xml-name-char-p
   #:xml-token-char-p))

(in-package :xml)

;;; ----------------------------------------------------

(defclass xml-doc ()
  ((source     :initarg :source     :accessor xml-doc-source)
   (version    :initarg :version    :accessor xml-doc-version)
   (encoding   :initarg :encoding   :accessor xml-doc-encoding)
   (standalone :initarg :standalone :accessor xml-doc-standalone)
   (doctype    :initarg :doctype    :accessor xml-doc-doctype)
   (root       :initarg :root       :accessor xml-doc-root))
  (:documentation "Basic XML document parsing context."))

;;; ----------------------------------------------------

(defclass xml-node ()
  ((namespace  :initarg :namespace  :accessor xml-node-namespace)
   (doc        :initarg :document   :accessor xml-node-doc)
   (name       :initarg :name       :accessor xml-node-name)
   (value      :initarg :value      :accessor xml-node-value)
   (parent     :initarg :parent     :accessor xml-node-parent))
  (:documentation "A generic node name/value pair."))

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

(defclass xml-entity (xml-ref xml-node)
  ((ndata      :initarg :ndata      :accessor xml-entity-ndata))
  (:documentation "A parsed or unparsed entity."))

;;; ----------------------------------------------------

(defclass xml-tag (xml-node)
  ((namespaces :initarg :namespaces :accessor xml-tag-namespaces)
   (elements   :initarg :elements   :accessor xml-tag-elements)
   (attributes :initarg :attributes :accessor xml-tag-attributes))
  (:documentation "A generic element."))

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
      (char<= #\u+FDF0 c #\u+FFFD)))

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
  (cond ((string-equal encoding "us-ascii") :us-ascii)

        ;; utf formats
        ((string-equal encoding "utf-8") :utf-8)
        ((string-equal encoding "utf-16") :utf-16)
        ((string-equal encoding "utf-32") :utf-32)

        ;; macos and japanese extended unicode
        ((string-equal encoding "x-mac-roman") :macos-roman)
        ((string-equal encoding "euc-jp") :euc-jp)

        ;; iso-8859-x encodings
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

(defun parsed-entity-ref (doctype ref)
  "Return the expanded, parsed entity reference value."
  (when doctype
    (let* ((entities (xml-doctype-entities doctype))
           (entity (find ref
                         entities
                         :test #'string=
                         :key #'xml-node-name)))
      (when entity
        (if (xml-entity-ndata entity)
            (warn "Cannot parse unparsed entity ~s" ref)
          (xml-node-value entity))))))

;;; ----------------------------------------------------

(define-lexer xml-lexer (s)

  ;; xml declaration
  ("^<%?xml[%s%n]+"
   (push-lexer s 'xml-decl-lexer :xml))

  ;; skip whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; document type declaration
  ("<!DOCTYPE[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-doctype-lexer :doctype $1))

  ;; processing instruction
  ("<%?(%:xml-name-char-p:%:xml-token-char-p:*)[%s%n]+(.-)%?>"
   (prog1 :next-token
     (warn "Skipping processing instruction ~s..." $1)))

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
   (values :external-ref (list $1)))
  ("[%s%n]+PUBLIC[%s%n]+(?'(.-)'|\"(.-)\")[%s%n]+(?'(.-)'|\"(.-)\")"
   (values :external-ref (list $2 $1)))

  ;; internal document type declaration
  ("[%s%n]*%[" (push-lexer s 'xml-dtd-lexer :dtd))

  ;; end of doctype
  ("[%s%n]*>" (pop-lexer s :end-doctype)))

;;; ----------------------------------------------------

(define-lexer xml-dtd-lexer (s)

  ;; skip whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; processing instruction
  ("<%?(%:xml-name-char-p:%:xml-token-char-p:*)[%s%n]+(.-)%?>"
   (prog1 :next-token
     (warn "Skipping processing instruction ~s..." $1)))

  ;; skip parameter entities, and warn
  ("%%(%:xml-name-char-p:%:xml-token-char-p:*);"
   (prog1 :next-token
     (warn "Skipping PE reference in DTD ~s..." $1)))

  ;; entity tags
  ("<!ENTITY[%s%n]+%%[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-dtd-element-lexer :dtd-element))
  ("<!ENTITY[%s%n]+(%:xml-name-char-p:%:xml-token-char-p:*)"
   (push-lexer s 'xml-entity-lexer :entity-decl $1))

  ;; non-entity declarations (non-validating, so skip)
  ("<!(%u+)[%s%n]+" (push-lexer s 'xml-dtd-element-lexer :dtd-element $1))

  ;; end of internal document type declaration
  ("%]" (pop-lexer s :end-dtd)))

;;; ----------------------------------------------------

(define-lexer xml-entity-lexer (s)

  ;; external references
  ("[%s%n]+SYSTEM[%s%n]+(?'(.-)'|\"(.-)\")"
   (values :external-ref (list $1)))
  ("[%s%n]+PUBLIC[%s%n]+(?'(.-)'|\"(.-)\")[%s%n]+(?'(.-)'|\"(.-)\")"
   (values :external-ref (list $2 $1)))

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
  (".[^>'\"%[]*" :next-token))

;;; ----------------------------------------------------

(define-lexer xml-attribute-lexer (s)

  ;; end of tag attributes
  ("/>" (pop-lexer s :singleton-tag))
  (">" (swap-lexer s 'xml-inner-lexer :end-tag))

  ;; skip whitespace
  ("[%s%n]+" :next-token)

  ;; namespaces
  ("xmlns:(%:xml-name-char-p:%:xml-token-char-p:*)"
   (values :namespace $1))
  ("xmlns[%s%n]*=[%s%n]*(?'(.-)'|\"(.-)\")"
   (values :default-namespace $1))

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
   (prog1 :next-token
     (warn "Skipping processing instruction ~s..." $1)))

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

          ;; optional document type declaration
          (doctype (.opt nil 'xml-doctype-parser))

          ;; the root tag is the only required element
          (root 'xml-tag-parser))

    ;; return the components of the xml file
    (.ret (list decl doctype root))))

;;; ----------------------------------------------------

(define-parser xml-decl-parser
  "An <?xml..?> declaration."
  (.do (.is :xml)

       ;; read the version, which is not optional
       (.let* ((version (.is :version))

               ;; the encoding and standalone flag are both optional
               (encoding (.opt "utf-8" (.is :encoding)))
               (standalone (.opt "no" (.is :standalone))))

         ;; close the declaration and return
         (.do (.is :end-xml)

              ;; return the declaration attributes
              (.ret (list version encoding standalone))))))

;;; ----------------------------------------------------

(define-parser xml-doctype-parser
  "The document type declaration."
  (.let* ((root (.is :doctype))

          ;; document can have an optional external reference
          (external-ref (.opt nil (.is :external-ref)))

          ;; optionally parse any internal subset DTD
          (dtd (.opt nil 'xml-dtd-parser)))

      ;; finish the doctype
    (.do (.is :end-doctype)

         ;; return the DTD parsed (if any)
         (.ret (list root external-ref dtd)))))

;;; ----------------------------------------------------

(define-parser xml-dtd-parser
  "Internal subset document type declaration."
  (.between (.is :dtd) (.is :end-dtd) (.many 'xml-elements-parser)))

;;; ----------------------------------------------------

(define-parser xml-elements-parser
  "All the element delcarations of the DTD."
  (.or 'xml-entity-parser

       ;; other declarations are skipped
       (.ignore (.do (.is :dtd-element)
                     (.is :end-dtd-element)))))

;;; ----------------------------------------------------

(define-parser xml-entity-parser
  "Entity declarations in the DTD."
  (.let* ((name (.is :entity-decl))

          ;; the value can be parsed, unparsed, or an external reference
          (entity (.or (.let* ((value (.is :value))

                               ;; optionally an unparsed NDATA type
                               (ndata (.opt nil (.is :ndata))))
                         (.ret (list name nil value ndata)))

                       ;; external reference?
                       (.let* ((ref (.is :external-ref))

                               ;; optionall an unparsed NDATA type
                               (ndata (.opt nil (.is :ndata))))
                         (.ret (list name ref nil ndata))))))

    ;; end the entity
    (.do (.is :end-dtd-element)

         ;; return it
         (.ret entity))))

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
  (.many (.or (.let (default-ns (.is :default-namespace))
                (.ret (list :namespace t default-ns)))

              ;; named namespace
              (.let* ((ns (.is :namespace))
                      (value (.is :value)))
                (.ret (list :namespace ns value)))

              ;; attributes
              (.let* ((att (.is :attr))
                      (value (.is :value)))
                (.ret (list :attribute att value))))))

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

(defun expand-entity-refs (doc string)
  "Use the document entities to expand a string."
  (let ((dtd (xml-doc-doctype doc)))
    (markup-decode string #'(lambda (ref)
                              (parsed-entity-ref dtd ref)))))

;;; ----------------------------------------------------

(defun find-namespace (name tag)
  "Lookup a namespace in an tag tree given the name."
  (loop
     with default-ns = nil

     ;; stop at the root
     while tag

     ;; lookup the namespace in this tag
     for nss =  (xml-tag-namespaces tag)
     for ns = (find name nss :test 'equal :key 'xml-node-name)

     ;; if the namespace was found, return it
     when ns return ns

     ;; if there's no default namespace yet, find one
     unless default-ns do (setf default-ns (find t nss :key 'xml-node-name))

     ;; otherwise, go up the tree, stop at root
     do (unless (setf tag (xml-node-parent tag))
          (loop-finish))

     ;; if none found, find the first default namespace and use that
     finally (return default-ns)))

;;; ----------------------------------------------------

(defun name-namespace (name tag)
  "Split a name into namespace and name."
  (let ((i (position #\: name)))
    (if i
        (let ((ns (find-namespace (subseq name 0 i) tag)))
          (values ns (if ns (subseq name (1+ i)) name)))
      (values (find-namespace t tag) name))))

;;; ----------------------------------------------------

(defun build-namespace (doc parent name value)
  "Construct an xml-namespace."
  (make-instance 'xml-node
                 :document doc
                 :parent parent
                 :namespace nil
                 :name name
                 :value value))

;;; ----------------------------------------------------

(defun build-attribute (doc parent k v)
  "Construct an xml-attribute, decoding the value."
  (multiple-value-bind (ns name)
      (name-namespace k (xml-node-parent parent))
    (make-instance 'xml-node
                   :document doc
                   :namespace ns
                   :parent parent
                   :name name
                   :value (expand-entity-refs doc v))))

;;; ----------------------------------------------------

(defun build-tag (doc parent name atts &rest inner-xml)
  "Construct an xml-tag from a parsed form."
  (let ((tag (make-instance 'xml-tag
                            :document doc
                            :namespace nil
                            :parent parent
                            :name name
                            :value nil
                            :elements nil)))
    (prog1 tag

      ;; collect all the namespaces first...
      (setf (xml-tag-namespaces tag)
            (loop
               for (type k v) in atts
               when (eq type :namespace)
               collect (build-namespace doc tag k v))

            ;; ...then the attributes, which use the namespaces
            (xml-tag-attributes tag)
            (loop
               for (type k v) in atts
               when (eq type :attribute)
               collect (build-attribute doc tag k v)))

      ;; now determine the namespace and the name of the tag
      (multiple-value-bind (ns tag-name)
          (name-namespace name tag)
        (setf (xml-node-name tag) tag-name (xml-node-namespace tag) ns))

      ;; construct the inner-xml
      (loop
         with inner-text = (make-string-output-stream)

         ;; process inner-xml
         for e in inner-xml
         for etype = (first e)

         ;; collect child tags
         when (eq etype :tag)
         collect (apply 'build-tag doc tag (rest e))
         into child-tags

         ;; cdata is written as-is
         when (eq etype :cdata)
         do (write-string (second e) inner-text)

         ;; inner text needs to be decoded and written
         when (eq etype :inner-text)
         do (let ((text (expand-entity-refs doc (second e))))
              (write-string text inner-text))

         ;; set the child elements
         finally (setf (xml-tag-elements tag) child-tags

                       ;; write the output to the tag's value
                       (xml-node-value tag)
                       (get-output-stream-string inner-text))))))

;;; ----------------------------------------------------

(defun build-dtd (doc root &optional ref elements)
  "Create the doctype from all the elements of the DTD."
  (flet ((make-entity (name ref value ndata)
           (make-instance 'xml-entity
                          :document doc
                          :parent nil
                          :name name
                          :system-uri (first ref)
                          :public-id (second ref)
                          :value value
                          :ndata ndata)))

    ;; construct the document type declaration
    (make-instance 'xml-doctype
                   :root root
                   :system-uri (first ref)
                   :public-id (second ref)
                   :entities (loop
                                for e in elements
                                when e
                                collect (apply #'make-entity e)))))

;;; ----------------------------------------------------

(defun build-doc (source decl doctype root-tag)
  "Construct the xml-doc from the parsed forms."
  (let ((doc (make-instance 'xml-doc
                            :source source
                            :version "1.0"
                            :encoding :utf-8
                            :standalone t
                            :doctype nil
                            :root nil)))
    (prog1 doc

      ;; was there an xml declaration?
      (when decl
        (destructuring-bind (version &optional encoding standalone)
            decl
          (setf (xml-doc-version doc) version)

          ;; optionally set the encoding
          (when encoding
            (setf (xml-doc-encoding doc)
                  (xml-external-format encoding)))

          ;; optionally set the standalone flag
          (when standalone
            (setf (xml-doc-standalone doc)
                  (string-equal standalone "yes")))))

      ;; build the document type declaration if present
      (when doctype
        (setf (xml-doc-doctype doc)
              (apply 'build-dtd doc doctype)))

      ;; construct the root tag
      (setf (xml-doc-root doc)
            (apply 'build-tag doc nil (rest root-tag))))))

;;; ----------------------------------------------------

(defun xml-parse (string &optional source)
  "Parse a string as XML."
  (let ((spec (with-lexer (lexer 'xml-lexer string :source source)
                (with-token-reader (next-token lexer)
                  (parse 'xml-parser next-token))))

        ;; setup the markup entity name predicates for xml
        (*markup-entity-start-char-p* 'xml-name-char-p)
        (*markup-entity-char-p* 'xml-token-char-p))

    ;; construct the document from the spec
    (apply 'build-doc source spec)))

;;; ----------------------------------------------------

(defun xml-load (pathname)
  "Read the contents of a file and then parse it as XML."
  (let ((bytes (slurp pathname :element-type '(unsigned-byte 8))))
    (xml-parse (xml-decode bytes) pathname)))

;;; ----------------------------------------------------

(defun xml-read (node)
  "Read the value of an XML node and coerce it to a given type."
  (read-from-string (xml-node-value node)))

;;; ----------------------------------------------------

(defun xml-decode (bytes)
  "Determine the encoding type of a vector and decode it."
  (when (>= (length bytes) 4)
    (let* ((mark (logior (dpb (elt bytes 0) (byte 8 24) 0)
                         (dpb (elt bytes 1) (byte 8 16) 0)
                         (dpb (elt bytes 2) (byte 8  8) 0)
                         (dpb (elt bytes 3) (byte 8  0) 0)))

           ;; check for a byte order mark first
           (format (cond ((= #x0000FEFF mark) :utf-32)
                         ((= #xFFFE0000 mark) :utf-32)

                         ;; utf-16 big and little endian
                         ((= #xFEFF (ldb (byte 16 16) mark)) :utf-16)
                         ((= #xFFFE (ldb (byte 16 16) mark)) :utf-16)

                         ;; utf-8 with byte mark
                         ((= #xFEBBBF (ldb (byte 24 8) mark)) :utf-8)

                         ;; without a byte order mark
                         ((= #x0000003C mark) :utf-32)
                         ((= #x3C000000 mark) :utf-32)
                         ((= #x003C003F mark) :utf-16)
                         ((= #x3C003F00 mark) :utf-16)
                         ((= #x3C3F786D mark) :utf-8)

                        ;; anything else, just assume utf-8
                        (t :utf-8))))
      (decode-string-from-octets bytes :external-format format))))
