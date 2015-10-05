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

(define-lexer xml-query-lexer (s)

  ;; skip whitespace
  ("%s+" :next-token)

  ;; tokens
  ("^/"   (values :root))
  ("/"    (values :path))
  ("%*"   (values :any))
  ("%.%." (values :parent))
  ("%."   (values :self))
  ("@"    (values :att))
  ("%("   (values :plist))
  ("%)"   (values :end-plist))
  ("%["   (values :select))
  ("%]"   (values :end-select))
  (","    (values :comma))

  ;; operators
  ("="    (values :op :eq))
  ("!="   (values :op :ne))
  ("<="   (values :op :le))
  (">="   (values :op :ge))
  ("<"    (values :op :lt))
  (">"    (values :op :gt))

  ;; quoted strings
  ("'(.-)'|\"(.-)\""
   (let ((time (encode-universal-rfc-time $1 :iso8601)))
     (if time
         (values :time time)
       (values :text $1))))

  ;; numeric constants
  ("[+-]?%d+(?%.%d+)?(?e[+-]?%d+)?"
   (values :number (read-from-string $$)))

  ;; token identifier
  ("%:xml-name-char-p:%:xml-token-char-p:*"
   (values :token $$)))

;;; ----------------------------------------------------

(define-parser xml-query-parser
  "Parse a query."
  (.either (.do (.is :root)
                (.let (xs (.sep-by 'xml-path-parser (.is :path)))
                  (.ret (cons :root xs))))
           (.sep-by1 'xml-path-parser (.is :path))))

;;; ----------------------------------------------------

(define-parser xml-path-parser
  "Single path elements."
  (.let (path (.or (>> (.is :parent) (.ret '(:parent)))
                   (>> (.is :self) (.ret '(:self)))
                   (>> (.is :any) (.ret '(:any)))

                   ;; an empty path is a descendant
                   (.do (.is :path)
                        (.let (tok (.is :token))
                          (.ret (list :descendant tok))))

                   ;; an attribute
                   (.do (.is :att)
                        (.let (tok (.is :token))
                          (.ret (list :att tok))))

                   ;; a token
                   (.let (tok (.is :token))
                     (.either (.let (plist 'xml-plist-parser)
                                (.ret (list :apply tok plist)))
                              (.ret (list :tag tok))))))

    ;; check for a select
    (.either (.let (select 'xml-select-parser)
               (.ret (append path select)))

             ;; just a path select
             (.ret path))))

;;; ----------------------------------------------------

(define-parser xml-plist-parser
  "Expression parameter list."
  (.between (.is :plist)
            (.is :end-plist)

            ;; zero or more expressions delimited by commas
            (.sep-by 'xml-expr-parser (.is :comma))))

;;; ----------------------------------------------------

(define-parser xml-expr-parser
  "Single expression."
  (.let (lvalue 'xml-value-parser)
    (.either (.let* ((op (.is :op))

                     ;; get the right-side of the expression
                     (rvalue 'xml-value-parser))
               (.ret (list op lvalue rvalue)))

             ;; just a single value
             (.ret lvalue))))

;;; ----------------------------------------------------

(define-parser xml-select-parser
  "Index or select filter."
  (.between (.is :select) (.is :end-select) 'xml-expr-parser))

;;; ----------------------------------------------------

(define-parser xml-value-parser
  "A value."
  (.or (>> (.is :parent) (.ret '(:parent)))
       (>> (.is :self) (.ret '(:self)))

       ;; an attribute value
       (.do (.is :att)
            (.let (tok (.is :token))
              (.ret (list :att tok))))

       ;; a date/time, text string, or number
       (.let (x (.is :time)) (.ret (list :time x)))
       (.let (x (.is :text)) (.ret (list :text x)))
       (.let (x (.is :number)) (.ret (list :number x)))))

;;; ----------------------------------------------------

(defun xml-query-parse (qs)
  "Parse a xpath query string into an expresion."
  (with-lexer (lexer 'xml-query-lexer qs)
    (with-token-reader (next-token lexer)
      (parse 'xml-query-parser next-token))))

;;; ----------------------------------------------------

(defmethod xml-query ((doc xml-doc) query)
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

(defmethod xml-query ((node xml-tag) query)
  "Execute a query on a node to return a subset of values."
  (loop
     with xs = (list node)

     ;; loop over each step in the query
     for (select x) in query

     ;; loop over the current results, apply the select
     do (setf xs (ecase select

                   ;; use this node; same as (:where t)
                   (:self xs)

                   ;; remove based on a predicate
                   (:where (cond ((eq x t) xs)
                                 ((eq x nil) nil)

                                 ;; the test is a predicate function
                                 (t (remove-if-not x xs))))

                   ;; get the parent node
                   (:parent (mapcar #'xml-element-parent xs))

                   ;; traverse to all descendants
                   (:descendant (xml-query-find-descendants x xs))

                   ;; find matching child tags; (:tag t) matches all
                   (:tag (xml-query-find x xs 'elts))

                   ;; find matching attributes; (:attribute t) matches all
                   (:attribute (xml-query-find x xs 'atts))

                   ;; pick a single index from the results
                   (:index (let ((node (nth x xs)))
                             (when node
                               (list node))))

                   ;; apply a function to the node, use result
                   (:apply (mapcar x xs))))

     ;; done, return the results
     finally (return xs)))

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

(defun xml-query-find-descendants (name nodes)
  "Search rescursively through nodes to find descendant tags."
  (let ((pred (xml-query-name name)))
    (labels ((find-descendants (node)
               (loop
                  for child in (xml-tag-elements node)
                  for match = (funcall pred child)

                  ;; does this child match the name?
                  when match collect child into xs

                  ;; if not, recursively descend
                  unless match append (find-descendants child) into ys

                  ;; combine them together
                  finally (return (append xs ys)))))
      (mapcan #'find-descendants nodes))))
