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

(in-package :xml)

;;; ----------------------------------------------------

(defvar *xml-date/time-format* :iso8601)

;;; ----------------------------------------------------

(defvar %node)
(defvar %current)
(defvar %text)

;;; ----------------------------------------------------

(defclass xml-query ()
  ((pattern      :initarg :pattern      :reader xml-query-pattern)
   (instructions :initarg :instructions :reader xml-query-instructions))
  (:documentation "A compiled XML query."))

;;; ----------------------------------------------------

(defmethod print-object ((query xml-query) stream)
  "Output an XML query to a stream."
  (print-unreadable-object (query stream :type t)
    (prin1 (xml-query-pattern query) stream)))

;;; ----------------------------------------------------

(define-lexer xml-query-lexer (s)

  ;; path tokens
  ("^/"   (values :root))
  ("/"    (values :path))
  ("%*"   (values :any))
  ("%.%." (values :parent))
  ("%."   (values :self))

  ;; an integer index
  ("%d+"  (values :index (parse-integer $$)))

  ;; tag selector
  ("%:xml-name-char-p:%:xml-token-char-p:*"
   (values :token $$))

  ;; attribute selector
  ("@%:xml-name-char-p:%:xml-token-char-p:*"
   (values :att (subseq $$ 1)))

  ;; read a symbol to use as a map function
  ("#?'([^/]+)" (values :apply (read-from-string $1)))

  ;; read a form to use as a map lambda body
  ("%(" (read-from-lexer s :apply))

  ;; lisp forms as a filter
  ("%[" (push-lexer s 'xml-query-lexer :select))
  ("%]" (pop-lexer s :end-select)))

;;; ----------------------------------------------------

(define-parser xml-query-parser
  "Parse a query."
  (.either (.do (.is :root)
                (.let (xs (.sep-by 'xml-path-parser (.is :path)))
                  (.ret (cons '(:root) xs))))
           (.sep-by1 'xml-path-parser (.is :path))))

;;; ----------------------------------------------------

(define-parser xml-path-parser
  "Single path elements."
  (.let (path (.or (>> (.is :parent) (.ret '(:parent)))
                   (>> (.is :self) (.ret '(:self)))
                   (>> (.is :any) (.ret '(:any)))

                   ;; index
                   (.let (n (.is :index))
                     (.ret (list :index n)))

                   ;; an empty path is a descendant
                   (.do (.is :path)
                        (.or (.let (tok (.is :token))
                               (.ret `(:descendant (:tag ,tok))))
                             (.let (att (.is :att))
                               (.ret `(:descendant (:attribute ,att))))))

                   ;; a child tag
                   (.let (tok (.is :token))
                     (.ret (list :tag tok)))

                   ;; an attribute
                   (.let (att (.is :att))
                     (.ret (list :attribute att)))

                   ;; a transform
                   (.let (form (.is :apply))
                     (.ret (list :apply (if (symbolp form)
                                            form
                                          (xml-query-select (list form))))))

                   ;; syntax error in parser
                   (.fail "Illegal XML query")))

    ;; check for an index or a predicate filter
    (.either (.let (select 'xml-query-select-parser)
               (let ((q (make-instance 'xml-query :instructions select)))
                 (.ret (append path (list q)))))

             ;; just a path select
             (.ret path))))

;;; ----------------------------------------------------

(define-parser xml-query-select-parser
  "An inner context query."
  (.between (.is :select) (.is :end-select) 'xml-query-parser))

;;; ----------------------------------------------------

(defun xml-query-compile (qs)
  "Parse a xpath query string into an expresion."
  (with-lexer (lexer 'xml-query-lexer qs)
    (with-token-reader (next-token lexer)
      (multiple-value-bind (inst okp)
          (parse 'xml-query-parser next-token)
        (when okp
          (make-instance 'xml-query :pattern qs :instructions inst))))))

;;; ----------------------------------------------------

(defmethod xml-query ((doc xml-doc) (query string))
  "Compile the query before running it."
  (xml-query doc (xml-query-compile query)))

;;; ----------------------------------------------------

(defmethod xml-query ((tag xml-tag) (query string))
  "Compile the query before running it."
  (xml-query tag (xml-query-compile query)))

;;; ----------------------------------------------------

(defmethod xml-query ((doc xml-doc) (query xml-query))
  "Execute a query on a document to return a subset of values."
  (let ((top (make-instance 'xml-tag
                            :document doc
                            :name nil
                            :value nil
                            :parent nil
                            :namespaces nil
                            :attributes nil
                            :elements (list (xml-doc-root doc)))))
    (xml-query top query)))

;;; ----------------------------------------------------

(defmethod xml-query ((node xml-tag) (query xml-query))
  "Execute a query on a node to return a subset of values."
  (loop
     with xs = (list node)

     ;; loop over each step in the query
     for (walk x select) in (xml-query-instructions query)

     ;; loop over the current results, apply the select
     do (setf xs (ecase walk

                   ;; use this node
                   (:self xs)

                   ;; jump to the root node of the document
                   (:root (list (xml-doc-root (xml-element-doc node))))

                   ;; get the parent node
                   (:parent (mapcar #'xml-element-parent xs))

                   ;; find matching child tags
                   (:tag (xml-query-find x xs 'elts))

                   ;; find matching attributes; (:attribute t) matches all
                   (:attribute (xml-query-find x xs 'atts))

                   ;; traverse to all descendants
                   (:descendant (xml-query-find-descendants x xs))

                   ;; apply a function to the node, use result
                   (:apply (mapcar x xs))

                   ;; pick an index
                   (:index (let ((n (nth (1- x) xs)))
                             (when n
                               (list n))))))

     ;; optionally select from the results so far
     when select do (setf xs (flet ((test (x)
                                      (xml-query x select)))
                               (remove-if-not #'test xs)))

     ;; done, return the results
     finally (return xs)))

;;; ----------------------------------------------------

(defun xml-query-select (forms)
  "Select a from a list of nodes based on index or test."
  (compile () `(lambda (%self)
                 (when (typep %self 'xml-node)
                   (setf %node %self %text (xml-node-value %self)))
                 (progn ,@forms))))

;;; ----------------------------------------------------

(defun xml-query-name (name)
  "Return a match function comparing a name to a node."
  (let ((i (position #\: name)))
    #'(lambda (node)
        (when (string= name (xml-node-name node) :start1 (and i (1+ i)))
          (let ((ns (xml-element-ns node)))
            (if (or (null ns) (null i))
                t
              (string= name (xml-node-name ns) :end1 i)))))))

;;; ----------------------------------------------------

(defun xml-query-find (name nodes slot)
  "Search a list of nodes for a matching child tags."
  (let ((pred (xml-query-name name)))
    (loop for n in nodes append (remove-if-not pred (slot-value n slot)))))

;;; ----------------------------------------------------

(defun xml-query-find-descendants (match nodes)
  "Search rescursively through nodes to find descendant tags."
  (destructuring-bind (type name)
      match
    (let ((pred (xml-query-name name)))
      (labels ((find-descendants (node)
                 (loop
                    for child in (xml-tag-elements node)
                    for attributes = (xml-tag-attributes child)

                    ;; find matching tags...
                    for match = (ecase type
                                  (:tag (and (funcall pred child) child))

                                  ;; ...or attributes
                                  (:attribute (find-if pred attributes)))

                    ;; collect the matches
                    when match collect match into xs

                    ;; descend into non-matches
                    unless match append (find-descendants child) into ys

                    ;; combine them together
                    finally (return (append xs ys)))))
        (mapcan #'find-descendants nodes)))))
