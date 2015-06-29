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
  (:use :cl :lw :hcl :external-format :parsergen :re :lexer)
  (:export
   #:xml-parse
   #:xml-parse-attributes
   #:xml-parse-text

   ;; load from file
   #:xml-load

   ;; element searches
   #:xml-query
   #:xml-query-attribute

   ;; parsed document accessors
   #:xml-doc-root
   #:xml-doc-doctype
   #:xml-doc-entities
   #:xml-doc-instructions
   #:xml-doc-source
   #:xml-doc-source-encoding
   #:xml-doc-encoding
   #:xml-doc-standalone
   #:xml-doc-version

   ;; parsed external reference accessors
   #:xml-external-ref-public-id
   #:xml-external-ref-system-uri

   ;; parsed doctype accessors
   #:xml-doctype-root

   ;; parsed node accessors
   #:xml-node-doc
   #:xml-node-name
   #:xml-node-value

   ;; parsed entity accessors
   #:xml-entity-ndata
   
   ;; parsed tag accessors
   #:xml-tag-attributes
   #:xml-tag-elements
   #:xml-tag-instructions))

(in-package :xml)

(defvar *xml-tokens* nil
  "List of tokens being parsed.")
(defvar *xml-doc* nil
  "Document being parsed.")
(defvar *xml-parameter-entities* nil
  "Parsed parameter entities.")

;;; ----------------------------------------------------

(defclass xml-doc ()
  ((root            :initarg :root            :accessor xml-doc-root                :initform nil)
   (doctype         :initarg :doctype         :accessor xml-doc-doctype             :initform nil)
   (entities        :initarg :entities        :accessor xml-doc-entities            :initform nil)
   (instructions    :initarg :instructions    :accessor xml-doc-instructions        :initform nil)
   (source          :initarg :source          :accessor xml-doc-source              :initform nil)
   (source-encoding :initarg :source-encoding :accessor xml-doc-source-encoding     :initform :utf-8)
   (encoding        :initarg :encoding        :accessor xml-doc-encoding            :initform :utf-8)
   (standalone      :initarg :standalone      :accessor xml-doc-standalone          :initform t)
   (version         :initarg :version         :accessor xml-doc-version             :initform 1.0))
  (:documentation "Basic XML document parsing context."))

(defclass xml-external-ref ()
  ((public          :initarg :public          :accessor xml-external-ref-public-id  :initform nil)
   (system          :initarg :system          :accessor xml-external-ref-system-uri :initform nil))
  (:documentation "An external reference."))

(defclass xml-doctype (xml-external-ref)
  ((root            :initarg :root            :accessor xml-doctype-root            :initform nil))
  (:documentation "A !DOCTYPE reference."))

(defclass xml-node ()
  ((name            :initarg :name            :accessor xml-node-name)
   (value           :initarg :value           :accessor xml-node-value              :initform nil)
   (doc             :initarg :doc             :accessor xml-node-doc                :initform *xml-doc*))
  (:documentation "A generic element."))

(defclass xml-processing-instruction (xml-node)
  ()
  (:documentation "A processing instruction about how to treat this document."))

(defclass xml-entity (xml-node xml-external-ref)
  ((ndata           :initarg :ndata           :accessor xml-entity-ndata            :initform nil))
  (:documentation "An !ENTITY declaration in the DTD."))

(defclass xml-tag (xml-node)
  ((attributes      :initarg :attributes      :accessor xml-tag-attributes          :initform nil)
   (elements        :initarg :elements        :accessor xml-tag-elements            :initform nil)
   (instructions    :initarg :instructions    :accessor xml-tag-instructions        :initform nil))
  (:documentation "An XML tag with attributes and inner-text value."))

(defclass xml-attribute (xml-node)
  ()
  (:documentation "An attribute key/value pair."))

;;; ----------------------------------------------------

(defmethod print-object ((doc xml-doc) stream)
  "Output an XML document to a stream."
  (print-unreadable-object (doc stream :type t)
    (if-let (root (xml-doc-root doc))
        (prin1 (xml-node-name root) stream)
      (prin1 () stream))))

(defmethod print-object ((node xml-node) stream)
  "Output an XML node to a stream."
  (print-unreadable-object (node stream :type t)
    (prin1 (xml-node-name node) stream)))

;;; ----------------------------------------------------

(deflexer xml-lexer

  ;; whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; declaration must be at the beginning
  ("^<%?xml" (push-lexer 'xml-decl-lexer :decl))

  ;; processing instructions
  ("<%?([%a_:][%w.:-]*)[%s%n]+(.-)%?>"
   (values :processing-instruction (list $1 $2)))

  ;; doctype declaration
  ("<!DOCTYPE[%s%n]+([%a_:][%w.:-]*)"
   (push-lexer 'xml-doctype-lexer :doctype $1))

  ;; tag element
  ("<([%a_:][%w.:-]*)" (swap-lexer 'xml-tag-lexer :tag $1)))

;;; ----------------------------------------------------

(deflexer xml-decl-lexer

  ;; whitespace
  ("[%s%n]+" :next-token)

  ;; assignment
  ("=" :eq)

  ;; attribute and value
  ("[%a_:][%w.:-]*" (values :att $$))
  ("'(.-)'|\"(.-)\"" (values :att-value $1))

  ;; end of xml declaration
  ("%?>" (pop-lexer :end-decl)))

;;; ----------------------------------------------------

(deflexer xml-doctype-lexer

  ;; external references
  ("[%s%n]+SYSTEM[%s%n]+(?\"(.-)\"|'(.-)')"
   (values :external-ref (list $1)))
  ("[%s%n]+PUBLIC[%s%n]+(?\"(.-)\"|'(.-)')[%s%n]+(?\"(.-)\"|'(.-)')"
   (values :external-ref (list $2 $1)))

  ;; whitespace
  ("[%s%n]+" :next-token)

  ;; embedded doctype entities
  ("%[" (push-lexer 'xml-dtd-lexer :dtd))

  ;; end of doctype
  (">" (pop-lexer :end-doctype)))

;;; ----------------------------------------------------

(deflexer xml-dtd-lexer

  ;; whitespace and comments
  ("[%s%n]+|<!%-%-.-%-%->" :next-token)

  ;; parsed entity reference
  ("%%([%a_:][%w.:-]*);"
   (values :parsed-entity $1))

  ;; entities references
  ("<!ENTITY[%s%n]+([%a_:][%w.:-]*)"
   (push-lexer 'xml-entity-lexer :entity $1))
  ("<!ENTITY[%s%n]+%%[%s%n]+([%a_:][%w.:-]*)"
   (push-lexer 'xml-entity-lexer :parameter-entity $1))

  ;; skip other dtd elements
  ("<!%u+(?\".-\"|'.-'|[^>'\"]+)*>" :next-token)

  ;; processing instructions
  ("<%?([%a_:][%w.:-]*)[%s%n]+(.-)%?>"
   (values :processing-instruction (list $1 $2)))

  ;; end of dtd
  ("%]" (pop-lexer :end-dtd)))

;;; ----------------------------------------------------

(deflexer xml-entity-lexer
  
  ;; internal value
  ("[%s%n]+(?\"(.-)\"|'(.-)')"
   (values :internal-ref $1))

  ;; external system reference
  ("[%s%n]+SYSTEM[%s%n]+(?\"(.-)\"|'(.-)')"
   (values :external-ref (list $1)))

  ;; external public reference
  ("[%s%n]+PUBLIC[%s%n]+(?\"(.-)\"|'(.-)')[%s%n]+(?\"(.-)\"|'(.-)')"
   (values :external-ref (list $2 $1)))

  ;; NDATA flag
  ("[%s%n]+NDATA[%s%n]+([%a_:][%w.:-]*)"
   (values :ndata $1))

  ;; end of entity
  ("[%s%n]*>" (pop-lexer :end-entity)))

;;; ----------------------------------------------------

(deflexer xml-tag-lexer

  ;; whitespace
  ("[%s%n]+" :next-token)

  ;; assignment
  ("=" :eq)

  ;; attribute and value
  ("[%a_:][%w.:-]*" (values :att $$))
  ("'(.-)'|\"(.-)\"" (values :att-value $1))

  ;; inner xml
  (">" (swap-lexer 'xml-inner-xml-lexer :inner-xml))

  ;; empty tag
  ("/>" (pop-lexer :empty-tag)))

;;; ----------------------------------------------------

(deflexer xml-inner-xml-lexer

  ;; comments
  ("<!%-%-.-%-%->" :next-token)

  ;; processing instructions
  ("<%?([%a_:][%w.:-]*)[%s%n]+(.-)%?>"
   (values :processing-instruction (list $1 $2)))

  ;; cdata block
  ("<!%[CDATA%[(.-)%]%]>" (values :cdata $1))

  ;; child tag
  ("<([%a_:][%w.:-]*)" (push-lexer 'xml-tag-lexer :tag $1))

  ;; close tag
  ("</([%a_:][%w.:-]*)%s*>" (pop-lexer :close-tag $1))

  ;; all other characters are parsed as text
  ("[^<]+" (values :text $$)))

;;; ----------------------------------------------------

(deflexer xml-inner-text-lexer

  ;; xml standard entities, don't include!
  ("&lt;" (values :char #\<))
  ("&gt;" (values :char #\>))
  ("&apos;" (values :char #\'))
  ("&quot;" (values :char #\"))
  ("&amp;" (values :char #\&))

  ;; unicode characters
  ("&#(%d+);" (values :char (code-char (parse-integer $1 :radix 10))))
  ("&#[xX](%x+);" (values :char (code-char (parse-integer $1 :radix 16))))

  ;; named entity references expand in-place
  ("&([%a_:][%w.:-]*);"
   (if *xml-doc*
       (if-let (ref (find $1 (xml-doc-entities *xml-doc*) :test #'string= :key #'xml-node-name))
           (if (xml-entity-ndata ref)
               (prog1 :next-token
                 (warn "Using NDATA as parsed entity ~s; ignoring..." $1))
             (include (xml-node-value ref)))
         (multiple-value-prog1 (values :text $$)
           (warn "Failed to find entity ~s in DTD" $1)))
     (multiple-value-prog1 (values :text $$)
       (warn "No document for entity ~s" $1))))

  ;; escaped characters
  ("&." (values :char (char $$ 1)))

  ;; all other text
  (".[^&]*" (values :text $$)))

;;; ----------------------------------------------------

(deflexer xml-attribute-lexer

  ;; whitespace
  ("[%s%n]+" :next-token)

  ;; assignment
  ("=" :eq)

  ;; attribute and value
  ("[%a_:][%w.:-]*" (values :att $$))
  ("'(.-)'|\"(.-)\"" (values :att-value $1)))

;;; ----------------------------------------------------

(deflexer xml-external-ref-lexer

  ;; parameter entity reference
  ("%%([%a_:][%w.:-]*);"
   (if-let (pe (find $1 *xml-parameter-entities* :test #'string= :key #'xml-node-name))
       (include (xml-node-value pe))
     (multiple-value-prog1 (values :text $$)
       (warn "Failed to find parameter entity ~s in DTD" $1))))

  ;; characters are expanded immediately
  ("&#(%d+);" (include (string (code-char (parse-integer $1 :radix 10)))))
  ("&#[xX](%x+);" (include (string (code-char (parse-integer $1 :radix 16)))))

  ;; escaped character
  ("%%." (values :text (string (char $$ 1))))
  
  ;; everything else
  (".[^%%]*" (values :text $$)))

;;; ----------------------------------------------------

(defparser xml-parser
  ((start doc))

  ;; document is xml declaration and root tag
  ((doc prolog root misc) t)
  ((doc doctype root misc) t)
  ((doc root misc) t)

  ;; xml declaration
  ((prolog decl misc doctype))
  ((prolog decl))

  ;; xml declaration
  ((decl :decl decl-atts))

  ;; xml declaration attributes
  ((decl-atts :att :eq :att-value decl-atts)
   (cond ((string= $1 "version")
          (setf (xml-doc-version *xml-doc*) $3))
         ((string= $1 "standalone")
          (setf (xml-doc-standalone *xml-doc*) (string= $3 "yes")))
         ((string= $1 "encoding")
          (setf (xml-doc-encoding *xml-doc*)
                (cond ((string-equal $3 "utf-8")       :utf-8)
                      ((string-equal $3 "utf-16")      :utf-16)
                      ((string-equal $3 "iso-8859-1")  :latin-1)
                      ((string-equal $3 "euc-jp")      :euc-jp)
                      ((string-equal $3 "iso-2022-jp") :jis)
                      ((string-equal $3 "jis")         :jis)
                      ((string-equal $3 "shift_jis")   :sjis)
                      ((string-equal $3 "x-mac-roman") :macos-roman)
                      
                      ;; unknown encoding, default to utf-8
                      (t (prog1 :utf-8
                           (warn "Unknown character encoding ~s; using UTF-8..." $3))))))))

  ;; end of declaration
  ((decl-atts :end-decl))

  ;; miscellaneous processing instructions
  ((misc processing-instruction misc)
   (push $1 (xml-doc-instructions *xml-doc*)))
  ((misc))

  ;; doctype
  ((doctype :doctype :external-ref :dtd dtd :end-doctype)
   (setf (xml-doc-doctype *xml-doc*) (apply #'make-doctype $1 $2)))
  ((doctype :doctype :external-ref :end-doctype)
   (setf (xml-doc-doctype *xml-doc*) (apply #'make-doctype $1 $2)))
  ((doctype :doctype :dtd dtd :end-doctype)
   (setf (xml-doc-doctype *xml-doc*) (make-doctype $1)))
  ((doctype :doctype :end-doctype)
   (setf (xml-doc-doctype *xml-doc*) (make-doctype $1)))

  ;; entities
  ((dtd entity dtd))
  ((dtd parameter-entity dtd))
  ((dtd parsed-entity dtd))

  ;; processing instructions
  ((dtd processing-instruction dtd)
   (push $1 (xml-doc-instructions *xml-doc*)))

  ;; end of the dtd
  ((dtd :end-dtd))

  ;; entities
  ((entity :entity :internal-ref :end-entity)
   (when-let (entity (make-internal-entity $1 $2))
     (push entity (xml-doc-entities *xml-doc*))))
  ((entity :entity :external-ref :end-entity)
   (when-let (entity (apply #'make-external-entity $1 nil $2))
     (push entity (xml-doc-entities *xml-doc*))))
  ((entity :entity :external-ref :ndata :end-entity)
   (when-let (entity (apply #'make-external-entity $1 $3 $2))
     (push entity (xml-doc-entities *xml-doc*))))

  ;; parameter entities
  ((parameter-entity :parameter-entity :internal-ref :end-entity)
   (push (make-internal-entity $1 $2) *xml-parameter-entities*))
  ((parameter-entity :parameter-entity :external-ref :end-entity)
   (let ((entity (apply #'make-external-entity $1 nil $2)))
     (push entity *xml-parameter-entities*)))

  ;; parsed entities
  ((parsed-entity :parsed-entity)
   (if-let (entity (find $1 *xml-parameter-entities* :test #'string= :key #'xml-node-name))
       (let ((tokens (tokenize 'xml-dtd-lexer (xml-node-value entity))))
         (setf *xml-tokens* (append tokens *xml-tokens*)))
     (warn "Failed to find parameter entity ~s; skipping..." $1)))

  ;; processing instructions
  ((processing-instruction :processing-instruction)
   (let ((target (first $1))
         (instruction (second $1)))
     (make-instance 'xml-processing-instruction :name target :value instruction)))
   
  ;; document root tag
  ((root tag)
   (setf (xml-doc-root *xml-doc*) $1))

  ;; tag element
  ((tag :tag atts :empty-tag)
   (make-tag $1 $2))
  ((tag :tag atts :inner-xml inner-xml)
   (make-tag $1 $2 $4))
  
  ;; attributes and values
  ((atts :att :eq :att-value atts)
   (let ((value (xml-parse-text $3)))
     `(,(make-instance 'xml-attribute :name $1 :value value) ,@$4)))
  ((atts))

  ;; inner xml
  ((inner-xml tag inner-xml)
   `((:tag ,$1) ,@$2))
  ((inner-xml processing-instruction inner-xml)
   `((:processing-instruction ,$1) ,@$2))
  ((inner-xml :cdata inner-xml)
   `((:cdata ,$1) ,@$2))
  ((inner-xml :text inner-xml)
   `((:text ,$1) ,@$2))
  ((inner-xml :close-tag)
   `((:close-tag ,$1))))

;;; ----------------------------------------------------

(defparser xml-attribute-parser
  ((start atts))

  ;; parse all attributes
  ((atts att atts)
   `(,$1 ,@$2))
  ((atts))

  ;; individual attribute
  ((att :att :eq :att-value)
   (make-instance 'xml-attribute :name $1 :value (xml-parse-text $3))))

;;; ----------------------------------------------------

(defun expand-parsed-entity-refs (text)
  "Use the current document to expand parsed entity references."
  (with-output-to-string (s nil :element-type 'simple-char)
    (flet ((parse-entity (tok)
             (princ (token-value tok) s)))
      (scan #'parse-entity 'xml-external-ref-lexer text))))

(defun read-external-ref (name system public &optional (element-type 'simple-char))
  "Load a SYSTEM reference file and return its value."
  (declare (ignorable public))
  (if-let (pathname (probe-file (let ((source (xml-doc-source *xml-doc*)))
                                  (if (null source)
                                      system
                                    (merge-pathnames system source)))))
      (let ((value (slurp pathname :element-type element-type)))
        (if (not (subtypep element-type 'character))
            value
          (expand-parsed-entity-refs value)))
    (warn "Failed to load SYSTEM file ~s; skipping entity..." system name)))

;;; ----------------------------------------------------

(defun make-doctype (root &optional system public)
  "Create a new doctype reference."
  (make-instance 'xml-doctype :root root :system system :public public))

(defun make-internal-entity (name value)
  "Create a new, internal entity."
  (make-instance 'xml-entity :name name :value (expand-parsed-entity-refs value)))

(defun make-external-entity (name ndata system &optional public)
  "Create a new, external entity."
  (when-let (value (read-external-ref name system public (if ndata '(unsigned-byte 8) 'simple-char)))
    (make-instance 'xml-entity :name name :system system :public public :ndata ndata :value value)))

(defun make-tag (name attributes &optional inner-xml)
  "Walk the attributes and inner text of a tag and expand entities, etc."
  (loop with s = (make-string-output-stream :element-type 'simple-char)

        ;; walk the inner xml
        for (elt data) in inner-xml
          
        ;; collect all child tags
        when (eq elt :tag)
        collect data into elements

        ;; collect processing instructions
        when (eq elt :processing-instruction)
        collect data into instructions
        
        ;; write cdata verbatim
        when (eq elt :cdata)
        do (princ data s)
        
        ;; text should be parsed
        when (eq elt :text)
        do (princ (xml-parse-text data) s)
        
        ;; the close tag must match!
        when (eq elt :close-tag)
        do (if (string/= data name)
               (error "Close tag mismatch; expecting ~s, got ~s..." name data)
             (loop-finish))

        ;; done
        finally (return (make-instance 'xml-tag
                                       :name name
                                       :attributes attributes
                                       :elements elements
                                       :instructions instructions
                                       :value (get-output-stream-string s)))))

;;; ----------------------------------------------------

(defun xml-parse-text (string &key (document *xml-doc*))
  "Parse a text string as XML, decode entities."
  (let ((*xml-doc* document))
    (with-output-to-string (s nil :element-type 'simple-char)
      (flet ((scan-token (tok)
               (princ (token-value tok) s)))
        (scan #'scan-token 'xml-inner-text-lexer string)))))

(defun xml-parse-attributes (string &key (document *xml-doc*))
  "Parse a text string as a series of attributes."
  (let ((*xml-doc* document))
    (parse 'xml-attribute-parser (tokenize 'xml-attribute-lexer string))))

(defun xml-parse (string &key source (source-encoding :latin-1))
  "Parse a string as XML."
  (let ((*xml-doc* (make-instance 'xml-doc :source source :source-encoding source-encoding))
        (*xml-parameter-entities* ())
        (*xml-tokens* (tokenize 'xml-lexer string source)))
    (flet ((next-token ()
             (when-let (tok (pop *xml-tokens*))
               (values (token-class tok)
                       (token-value tok)))))
      (when (xml-parser #'next-token)
        *xml-doc*))))

;;; ----------------------------------------------------

(defun xml-load (pathname &key (source-encoding :latin-1))
  "Read the contents of a file and then parse it as XML."
  (let ((string (slurp pathname :element-type 'lw:simple-char)))
    (xml-parse string :source pathname :source-encoding source-encoding)))

;;; ----------------------------------------------------

(defmethod xml-query ((tag xml-tag) xpath &key all)
  "Recursively descend into a tag finding child tags with a given path."
  (labels ((query (tag xpath)
             (destructuring-bind (name &rest rest)
                 xpath
               (let ((qs (if (string= name "*")
                             (xml-tag-elements tag)
                           (remove name (xml-tag-elements tag) :test-not #'string= :key #'xml-node-name))))
                 (if (null rest)
                     (if all
                         qs
                       (return-from xml-query (first qs)))
                   (loop :for q :in qs :append (query q rest)))))))
    (query tag (split-sequence "/" xpath :coalesce-separators t))))

(defmethod xml-query ((doc xml-doc) xpath &key all)
  "Recursively descend into the document to find a given path."
  (xml-query (make-instance 'xml-tag :doc doc :elements (list (xml-doc-root doc))) xpath :all all))

(defmethod xml-query-attribute ((tag xml-tag) name)
  "Return an attribute with the given name from a tag if found."
  (find name (xml-tag-attributes tag) :test #'string= :key #'xml-node-name))

(defmethod xml-query-attribute ((doc xml-doc) name)
  "Return an attribute with the given name from a tag if found."
  (xml-query-attribute (xml-doc-root doc) name))
