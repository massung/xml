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

(defvar %node nil)
(defvar %position nil)
(defvar %parent nil)
(defvar %name nil)
(defvar %text nil)

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

  ;; position index
  ("%d+"  (values :index (parse-integer $$)))

  ;; tag selector
  ("%:xml-name-char-p:%:xml-token-char-p:*"
   (values :token $$))

  ;; attribute selector
  ("@%:xml-name-char-p:%:xml-token-char-p:*"
   (values :att (subseq $$ 1)))

  ;; read an xml symbol
  ("%%[^/]+" (let ((symbol (intern (string-upcase $$) :xml)))
               (values :apply `(symbol-value ',symbol))))

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
  (.let (path (.or (>> (.is :parent) (.ret '(:parent nil)))
                   (>> (.is :self) (.ret '(:self nil)))
                   (>> (.is :any) (.ret '(:any nil)))

                   ;; an empty path is a descendant
                   (.do (.is :path)
                        (.or (.let (tok (.is :token))
                               (.ret `(:descendant (:tag ,tok))))
                             (.let (att (.is :att))
                               (.ret `(:descendant (:attribute ,att))))))

                   ;; position index
                   (.let (i (.is :index))
                     (.ret (list :index i)))

                   ;; a child tag
                   (.let (tok (.is :token))
                     (.ret (list :tag tok)))

                   ;; an attribute
                   (.let (att (.is :att))
                     (.ret (list :attribute att)))

                   ;; a transform
                   (.let (form (.is :apply))
                     (.ret (list :apply (xml-query-eval form))))

                   ;; syntax error in parser
                   (.fail "Illegal XML query")))

    ;; check for an index or a predicate filter
    (.either (.let (select 'xml-query-select-parser)
               (let ((q (make-instance 'xml-query
                                       :pattern nil
                                       :instructions select)))
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

(defmacro with-query-node (&body body)
  "Bind additional XML query variables and execute a body."
  `(multiple-value-bind (%parent %name %text)
       (when (typep %node 'xml-node)
         (values (xml-node-parent %node)
                 (xml-node-name %node)
                 (xml-node-value %node)))
     (progn ,@body)))

;;; ----------------------------------------------------

(defun xml-query-eval (form)
  "Compile a function that will execute form on a node."
  (compile () `(lambda ()
                 ,(if (symbolp form) `(funcall ',form %node) form))))

;;; ----------------------------------------------------

(defun xml-query-root (doc)
  "Return a root node for a query."
  (make-instance 'xml-tag
                 :document doc
                 :namespace nil
                 :name nil
                 :value nil
                 :parent nil
                 :namespaces nil
                 :attributes nil
                 :elements (list (xml-doc-root doc))))

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
  (xml-query-run (list (xml-query-root doc)) query))

;;; ----------------------------------------------------

(defmethod xml-query ((node xml-node) (query xml-query))
  "Execute a query on a node to return a subset of values."
  (xml-query-run (list node) query))

;;; ----------------------------------------------------

(defun xml-query-run (nodes query)
  "Run all the steps in a query on a set of nodes."
  (loop
     for (step x select) in (xml-query-instructions query)

     ;; update the list of nodes with the next results
     do (flet ((step-query (%node)
                 (xml-query-step step x)))
          (let ((ns (mapcan #'step-query nodes)))
            (setf nodes (if (null select)
                            ns
                          (xml-query-select ns select)))))

     ;; if there's a select in the step then filter
     finally (return nodes)))

;;; ----------------------------------------------------

(defun xml-query-select (nodes query)
  "Filter a set of nodes using a select query."
  (loop
     for %position from 1
     for %node in nodes

     ;; test the node, if there are results, keep it
     when (xml-query-run (list %node) query) collect %node))

;;; ----------------------------------------------------

(defun xml-query-step (step &optional x)
  "Execute a single instruction in a query on the current node."
  (ecase step

    ;; just keep the node
    (:self (list %node))

    ;; jump tot he root document node
    (:root (list (xml-query-root (xml-node-doc %node))))

    ;; get the parent node
    (:parent (list (xml-node-parent %node)))

    ;; all child tags
    (:any (xml-tag-elements %node))

    ;; all matching child tags
    (:tag (xml-query-find x (xml-tag-elements %node)))

    ;; all matching attributes
    (:attribute (xml-query-find x (xml-tag-attributes %node)))

    ;; all matching descendants
    (:descendant (xml-query-find-descendants x %node))

    ;; map the node, keep non-nil results
    (:apply (with-query-node
              (let ((y (funcall x)))
                (when y
                  (list y)))))

    ;; match by index
    (:index (when (eql x %position)
              (list %node)))))

;;; ----------------------------------------------------

(defun xml-query-name (name)
  "Return a match function comparing a name to a node."
  (let ((i (position #\: name)))
    #'(lambda (node)
        (when (string= name (xml-node-name node) :start1 (if i (1+ i) 0))
          (let ((ns (xml-node-namespace node)))
            (if (or (null ns) (null i))
                t
              (string= name (xml-node-name ns) :end1 i)))))))

;;; ----------------------------------------------------

(defun xml-query-find (name nodes)
  "Search a list of nodes for a matching child tags."
  (remove-if-not (xml-query-name name) nodes))

;;; ----------------------------------------------------

(defun xml-query-find-descendants (match node)
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
        (find-descendants node)))))
