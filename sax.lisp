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

(defpackage :xml-sax
  (:use :cl)
  (:nicknames :sax)
  (:export
   #:start-document
   #:end-document
   #:version
   #:encoding
   #:standalone
   #:start-dtd
   #:end-dtd
   #:resolve-external-reference
   #:resolve-parsed-entity
   #:external-entity-declaration
   #:internal-entity-declaration
   #:notation-declaration
   #:element-declaration
   #:attribute-declaration
   #:processing-instruction
   #:comment
   #:start-element
   #:end-element
   #:inner-text
   #:cdata))

(in-package :xml-sax)

(defclass xml-context ()
  ()
  (:documentation "Not useful for anything other than testing."))

;;; ----------------------------------------------------

(defmethod start-document (context))
(defmethod end-document (context))

;;; ----------------------------------------------------

(defmethod version (context version))
(defmethod encoding (context encoding))
(defmethod standalone (context standalone-p))

;;; ----------------------------------------------------

(defmethod start-dtd (context name system public))
(defmethod end-dtd (context))

;;; ----------------------------------------------------

(defmethod resolve-external-reference (context system public ndata))
(defmethod resolve-parsed-entity (context name param-entity-p))

;;; ----------------------------------------------------

(defmethod external-entity-declaration (context name parameter-entity-p system public ndata))
(defmethod internal-entity-declaration (context name parameter-entity-p value))

;;; ----------------------------------------------------

(defmethod notation-declaration (context name system public))
(defmethod element-declaration (context name))
(defmethod attribute-declaration (context name))

;;; ----------------------------------------------------

(defmethod processing-instruction (context target data))
(defmethod comment (context comment))

;;; ----------------------------------------------------

(defmethod start-element (context name attributes))
(defmethod end-element (context name))

;;; ----------------------------------------------------

(defmethod inner-text (context text &rest write-args &key &allow-other-keys))
(defmethod cdata (context data &rest write-args &key &allow-other-keys))
