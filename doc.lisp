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

(defpackage :xml-doc
  (:use :cl :lw :xml-sax)
  (:export
   #:xml-doc

   ;; document accessors
   #:xml-doc-source
   #:xml-doc-root
   #:xml-doc-doctype

   ;; parsed external reference accessors
   #:xml-external-ref-public-id
   #:xml-external-ref-system-uri

   ;; parsed doctype accessors
   #:xml-doctype-name

   ;; parsed node accessors
   #:xml-node-doc
   #:xml-node-name
   #:xml-node-value

   ;; parsed entity accessors
   #:xml-entity-ndata
   
   ;; parsed tag accessors
   #:xml-tag-elements
   #:xml-tag-parent
   #:xml-tag-attributes))

(in-package :xml-doc)

;;; ----------------------------------------------------

(defclass xml-doc ()
  ((source          :initarg :source          :accessor xml-doc-source              :initform nil)
   (root            :initarg :root            :accessor xml-doc-root                :initform nil)
   (doctype         :initarg :doctype         :accessor xml-doc-doctype             :initform nil))
  (:documentation "Basic XML document parsing context."))

(defclass xml-external-ref ()
  ((public          :initarg :public          :accessor xml-external-ref-public-id  :initform nil)
   (system          :initarg :system          :accessor xml-external-ref-system-uri :initform nil))
  (:documentation "An external reference."))

(defclass xml-doctype (xml-external-ref)
  ((name            :initarg :name            :accessor xml-doctype-name            :initform nil)
   (entities        :initarg :entities        :accessor xml-doctype-entities        :initform nil)
   (param-entities  :initarg :param-entities  :accessor xml-doctype-param-entities  :initform nil))
  (:documentation "A !DOCTYPE reference."))

(defclass xml-node ()
  ((name            :initarg :name            :accessor xml-node-name)
   (value           :initarg :value           :accessor xml-node-value              :initform nil))
  (:documentation "A generic element."))

(defclass xml-processing-instruction (xml-node)
  ()
  (:documentation "A processing instruction about how to treat this document."))

(defclass xml-entity (xml-node xml-external-ref)
  ((ndata           :initarg :ndata           :accessor xml-entity-ndata            :initform nil))
  (:documentation "An !ENTITY declaration in the DTD."))

(defclass xml-tag (xml-node)
  ((elements        :initarg :elements        :accessor xml-tag-elements            :initform nil)
   (parent          :initarg :parent          :accessor xml-tag-parent              :initform nil)
   (attributes      :initarg :attributes      :accessor xml-tag-attributes          :initform nil))
  (:documentation "An XML tag with attributes and inner-text value."))

(defclass xml-attribute (xml-node)
  ()
  (:documentation "An attribute key/value pair."))

;;; ----------------------------------------------------

(defmethod print-object ((doc xml-doc) stream)
  "Output a document to a stream."
  (print-unreadable-object (doc stream :type t)
    (prin1 (xml-doctype-name (xml-doc-doctype doc)) stream)))

(defmethod print-object ((node xml-node) stream)
  "Output a node to a stream."
  (print-unreadable-object (node stream :type t)
    (prin1 (xml-node-name node) stream)))

;;; ----------------------------------------------------

(defmethod end-document ((doc xml-doc))
  "Test the DTD and make sure it matches the root tag, or create one."
  (if-let (root (xml-doc-root doc))
      (let ((name (xml-node-name root)))
        (if-let (doctype (xml-doc-doctype doc))
            (unless (string= (xml-doctype-name doctype) name)
              (warn "Root tag mismatch with DOCTYPE; expected ~s got ~s") (xml-doctype-name doctype) name)
          (setf (xml-doc-doctype doc) (make-instance 'xml-doctype :name name))))
    (error "Missing root tag")))

;;; ----------------------------------------------------

(defmethod start-dtd ((doc xml-doc) name system public)
  "Create the DOCTYPE and add it to the document."
  (let ((doctype (make-instance 'xml-doctype :name name :system system :public public)))
    (setf (xml-doc-doctype doc) doctype)))

;;; ----------------------------------------------------

(defmethod resolve-external-reference ((doc xml-doc) system public ndata)
  "Return the text for a resolved external reference."
  (declare (ignore public))
  (let ((ref (if-let (local (xml-doc-source doc)) (merge-pathnames system local) system)))
    (if-let (pathname (probe-file ref))
        (with-open-file (s pathname :direction :input :element-type (if ndata '(unsigned-byte 8) 'simple-char))
          (let ((string (make-array (file-length s) :fill-pointer t)))
            (prog1 string
              (setf (fill-pointer string) (read-sequence string s)))))
    
      ;; file doesn't exist, warn and return nil
      (warn "Failed to resolve external reference ~s; skipping..." ref))))

(defmethod resolve-parsed-entity ((doc xml-doc) name param-entity-p)
  "Return the text expansion for an entity. NIL if not found."
  (if-let (doctype (xml-doc-doctype doc))
      (let ((entities (if param-entity-p
                          (xml-doctype-param-entities doctype)
                        (xml-doctype-entities doctype))))
        
        ;; find the entity in the list
        (if-let (entity (find name entities :test #'string= :key #'xml-node-name))
            (if (xml-entity-ndata entity)
                (warn "Referencing unparsed entity ~a; skipping..." name)
              (xml-node-value entity))
          (warn "Unknown entity reference ~a; skipping..." name)))
    (warn "Unknown entity reference ~a; skipping..." name)))

;;; ----------------------------------------------------

(defmethod external-entity-declaration ((doc xml-doc) name param-entity-p system public ndata)
  "Create a new entity."
  (let ((entity (make-instance 'xml-entity :name name :system system :public public :ndata ndata)))
    (if param-entity-p
        (push entity (xml-doctype-param-entities (xml-doc-doctype doc)))
      (push entity (xml-doctype-entities (xml-doc-doctype doc))))))

(defmethod internal-entity-declaration ((doc xml-doc) name param-entity-p value)
  "Create a new entity."
  (let ((entity (make-instance 'xml-entity :name name :value value)))
    (if param-entity-p
        (push entity (xml-doctype-param-entities (xml-doc-doctype doc)))
      (push entity (xml-doctype-entities (xml-doc-doctype doc))))))

;;; ----------------------------------------------------

;(defmethod notation-declaration ((doc xml-doc) name system public))
;(defmethod element-declaration ((doc xml-doc) name))
;(defmethod attribute-declaration ((doc xml-doc) name))

;;; ----------------------------------------------------

;(defmethod processing-instruction ((doc xml-doc) target data)
;(defmethod comment ((doc xml-doc) comment))

;;; ----------------------------------------------------

(defmethod start-element ((doc xml-doc) name atts)
  "Add a new element to the stack."
  (let* ((atts (loop for (n v) in atts collect (make-instance 'xml-attribute :name n :value v)))
         (parent (first (xml-doc-root doc)))
         (tag (make-instance 'xml-tag
                             :name name
                             :attributes atts
                             :parent parent
                             :value (make-string-output-stream))))

    ;; append this to the root tag (if there is one)
    (when parent
      (push tag (xml-tag-elements parent)))
    
    ;; make this the root tag
    (push tag (xml-doc-root doc))))

(defmethod end-element ((doc xml-doc) name)
  "Pop the element, ensure it matches."
  (let ((tag (pop (xml-doc-root doc))))

    ;; ensure the tags match
    (unless (string= name (xml-node-name tag))
      (error "Tag mismatch; expected ~s got ~s" (xml-node-name tag) name))
    
    ;; flush the character stream
    (setf (xml-node-value tag) (get-output-stream-string (xml-node-value tag)))

    ;; reverse child elements
    (setf (xml-tag-elements tag) (nreverse (xml-tag-elements tag)))

    ;; if this is the root tag, set it
    (unless (xml-doc-root doc)
      (setf (xml-doc-root doc) tag))))

;;; ----------------------------------------------------
      
(defmethod inner-text ((doc xml-doc) data &rest write-args &key &allow-other-keys)
  "Write the data to the inner text of the root tag."
  (apply #'write-string data (xml-node-value (first (xml-doc-root doc))) write-args))

(defmethod cdata ((doc xml-doc) data &rest write-args &key &allow-other-keys)
  "Write the data to the inner text of the root tag."
  (apply #'write-string data (xml-node-value (first (xml-doc-root doc))) write-args))
