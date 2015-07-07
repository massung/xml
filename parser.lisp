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

(in-package :xml)

(defclass xml-parser ()
  ((string :initarg :string :initform ""  :accessor xml-parser-string)
   (start  :initarg :start  :initform 0   :accessor xml-parser-start)
   (end    :initarg :end    :initform nil :accessor xml-parser-end)
   (line   :initarg :line   :initform 1   :accessor xml-parser-line)
   (source :initarg :source :initform nil :accessor xml-parser-source))
  (:documentation "An XML string parser."))

;;; ----------------------------------------------------

(defmethod print-object ((parser xml-parser) stream)
  "Output an xml-parser object to a stream so it's easily debugged."
  (print-unreadable-object (parser stream :type t)
    (with-slots (start end line string)
        parser
      (let ((eol (or (position #\newline string :start start) end)))
        (format stream "~d/~d... (line ~d) ~s" start end line (subseq string start eol))))))

;;; ----------------------------------------------------

(defmethod initialize-instance :after ((parser xml-parser) &key)
  "Set some default values for the parser and ensure validity."
  (with-slots (string start end)
      parser

    ;; make sure the end is set; default to the entire string
    (let ((n (length string)))
      (unless end (setf end n))

      ;; ensure validity
      (assert (<= 0 start end n)))))

;;; ----------------------------------------------------

(defmacro with-xml-parser ((parser &optional error &rest arguments) (var &rest vars) &body body)
  "Bind variables to a token forms. On failure, return nil for no match or error."
  (let ((p (gensym))
        (pos (gensym)))

    ;; generate token forms by type or symbol name
    (labels ((token-form (token args)
               (cond ((characterp token) `(xml-char ,p ,token))
                     ((stringp token) `(xml-string ,p ,token))
                       
                     ;; whitespace tokens
                     ((eq token t) `(xml-space ,p))
                       
                     ;; internal parser functions - must be last!
                     ((symbolp token) `(,token ,p ,@args))
                       
                     ;; unknwon token form
                     (t (error "Unknown XML token ~a" token))))
               
             ;; recursively create all the token bindings
             (make-bindings (binding rest &optional errorp)
               (destructuring-bind (var token &rest args)
                   (if (atom binding) (list (gensym) binding) binding)
                 `(if-let (,var ,(token-form token args))
                      ,(if (null rest)
                           `(progn ,@body)
                         (make-bindings (first rest) (rest rest)))
                    (prog1 nil
                      ,(if errorp
                           `(error "~@[~s ~]on line ~d~@[ of ~s~]"
                                   (format nil ,error ,@arguments)
                                   (xml-parser-line ,p)
                                   (xml-parser-source ,p))
                         `(setf (xml-parser-start ,p) ,pos)))))))
        
      ;; construction a parse chain of bindings
      `(let* ((,p ,parser) (,pos (xml-parser-start ,p)))
         ,(make-bindings var vars)))))

;;; ----------------------------------------------------

(defun xml-at-end-p (p)
  "T if the parser is at the end of the source."
  (not (< (xml-parser-start p) (xml-parser-end p))))

;;; ----------------------------------------------------

(defun xml-space (p)
  "Parse whitespace tokens."
  (loop for i from (xml-parser-start p) below (xml-parser-end p)

        ;; keep scanning whitespace characters
        for c = (char (xml-parser-string p) i)
        while (whitespace-char-p c)

        ;; track lines
        when (eql c #\newline)
        do (incf (xml-parser-line p))

        ;; update the read position
        finally (return (< (xml-parser-start p) (setf (xml-parser-start p) i)))))

;;; ----------------------------------------------------

(defun xml-char (p c)
  "Parse an exact character. Should not be a newline."
  (unless (xml-at-end-p p)
    (when (eql (char (xml-parser-string p) (xml-parser-start p)) c)
      (incf (xml-parser-start p)))))

;;; ----------------------------------------------------

(defun xml-string (p s)
  "Match an exact series of characters. Match string should have no newlines."
  (let ((end (+ (length s) (xml-parser-start p))))
    (when (and (<= end (xml-parser-end p))

               ;; do the series of characters match?
               (string= s (xml-parser-string p) :start2 (xml-parser-start p) :end2 end))
      (setf (xml-parser-start p) end))))

;;; ----------------------------------------------------

(defun xml-scan (p s)
  "Skip everything up until a string is found."
  (when-let (pos (search s (xml-parser-string p) :start2 (xml-parser-start p) :end2 (xml-parser-end p)))
    (let ((match (subseq (xml-parser-string p) (xml-parser-start p) pos)))
      (prog1 match
        (setf (xml-parser-start p) (+ pos (length s)))
        (incf (xml-parser-line p) (count #\newline match))))))

;;; ----------------------------------------------------

(defun xml-name (p)
  "Parse an XML identifier name. Updates the parse position."
  (labels ((name-char-p (c)
             (or (char= c #\:)
                 (char= c #\_)
      
                 ;; any ascii letter
                 (char<= #\A c #\Z)
                 (char<= #\a c #\z)
      
                 ;; acceptable unicode character ranges
                 (char<= #\u+000C0 c #\u+000D6)
                 (char<= #\u+000D8 c #\u+000F6)
                 (char<= #\u+000F8 c #\u+002FF)
                 (char<= #\u+00370 c #\u+0037D)
                 (char<= #\u+0037F c #\u+01FFF)
                 (char<= #\u+0200C c #\u+0200D)
                 (char<= #\u+02070 c #\u+0218F)
                 (char<= #\u+02C00 c #\u+02FEF)
                 (char<= #\u+03001 c #\u+0D7FF)
                 (char<= #\u+0F900 c #\u+0FDCF)
                 (char<= #\u+0FDF0 c #\u+0FFFD)
                 (char<= #\u+10000 c #\u+EFFFF)))

           ;; subsequence letters
           (letter-char-p (c)
             (or (name-char-p c)
                 
                 ;; one-off characters
                 (char= c #\-)
                 (char= c #\.)
                 (char= c #\u+00b7)
                 
                 ;; digits
                 (char<= #\0 c #\9)
                 
                 ;; unicode character ranges
                 (char<= #\u+00300 c #\u+0036F)
                 (char<= #\u+0203F c #\u+02040))))

    ;; if the first character begins a name, keep reading until no more
    (let ((i (xml-parser-start p)))
      (when (name-char-p (char (xml-parser-string p) i))
        (do ((c (char (xml-parser-string p) (incf i))
                (char (xml-parser-string p) (incf i))))
            ((not (letter-char-p c))
             (subseq (xml-parser-string p) (xml-parser-start p) (setf (xml-parser-start p) i))))))))

;;; ----------------------------------------------------

(defun xml-value (p)
  "Parse a quoted value with a per-character callback."
  (or (with-xml-parser (p) (#\' (quo xml-scan "'")) quo)
      (with-xml-parser (p) (#\" (quo xml-scan "\"")) quo)))

;;; ----------------------------------------------------

(defun xml-eq (p)
  "Parse an attribute assignment token."
  (xml-space p)
  (when (xml-char p #\=)
    (prog1 t
      (xml-space p))))

;;; ----------------------------------------------------

(defun xml-system-uri (p)
  "Parse a SYSTEM external reference URI string."
  (with-xml-parser (p "Malformed external SYSTEM reference")
      ("SYSTEM" t (uri xml-value))
    (list uri)))

;;; ----------------------------------------------------

(defun xml-public-id (p)
  "Parse a PUBLIC external reference public ID and URI strings."
  (with-xml-parser (p "Malformed external PUBLIC reference")
      ("PUBLIC" t (id xml-value) t (uri xml-value))
    (list uri id)))

;;; ----------------------------------------------------

(defun xml-external-ref (p)
  "Parse the SYSTEM or PUBLIC locations of an external reference."
  (or (xml-public-id p)
      (xml-system-uri p)))

;;; ----------------------------------------------------

(defun xml-character-ref (p)
  "Read a character reference and return the character."
  (when (xml-char p #\#)
    (multiple-value-bind (code pos)
        (let ((radix (if (xml-char p #\x) 16 10)))
          (parse-integer (xml-parser-string p) :start (xml-parser-start p) :radix radix :junk-allowed t))

      ;; update the parse position
      (setf (xml-parser-start p) pos)

      ;; read the terminator
      (if (xml-char p #\;)
          (code-char code)
        (error "Invalid character reference")))))

;;; ----------------------------------------------------

(defun xml-expand-refs (p context text &key allow-pe-refs stream (start 0) (end (length text)))
  "Expand references from text into a string."
  (let ((s (or stream (make-string-output-stream)))

        ;; create a new parser just for this expansion process
        (r (make-instance 'xml-parser :string text :start start :end end)))

    ;; 
    (with-slots (start end)
        r

      ;; find the next reference character
      (flet ((reserved (c) (or (and allow-pe-refs (eql c #\%)) (eql c #\&))))
        (do ((i (position-if #'reserved text :start start :end end)
                (position-if #'reserved text :start start :end end)))
            
            ;; at the end, write all remaining characters
            ((null i) (write-string text s :start start :end end))

          ;; write all text up to the reference
          (write-string text s :start start :end i)
          
          ;; update the read position
          (setf start (1+ i))
          
          ;; check to see if it's a simple character reference
          (let ((c (char text i)))
            (if-let (entity (or (xml-character-ref r)
                                
                                ;; if in the DTD, don't expand non-parameter entities
                                (case c
                                  (#\% (when allow-pe-refs (xml-entity-ref r context t)))
                                  (#\& (unless allow-pe-refs (xml-entity-ref r context nil))))))
                (typecase entity
                  (character (write-char entity s))
                  
                  ;; strings need to continue expansion until done
                  (string    (xml-expand-refs p context entity :stream s :allow-pe-refs allow-pe-refs)))

              ;; not found or skipped
              (write-char c s))))))

    ;; flush the output stream
    (if stream t (get-output-stream-string s))))

;;; ----------------------------------------------------

(defun xml-version-info (p context)
  "Parse the version attribute from the XML declaration."
  (with-xml-parser (p "Invalid 'version' attribute")
      ("version" xml-eq (value xml-value))
    (handler-case
        (prog1 t
          (sax:version context (parse-float value)))
      (condition (c)
        (error "Illegal XML version number ~s in declaration" value)))))

;;; ----------------------------------------------------

(defun xml-encoding-info (p context)
  "Parse the encoding declaration attribute."
  (with-xml-parser (p "Invalid 'encoding' attribute")
      ("encoding" xml-eq (value xml-value))
    (prog1 t
      (sax:encoding context (cond ((string-equal value "utf-8")       :utf-8)
                                  ((string-equal value "utf-16")      :utf-16)
                                  ((string-equal value "iso-8859-1")  :latin-1)
                                  ((string-equal value "euc-jp")      :euc-jp)
                                  ((string-equal value "iso-2022-jp") :jis)
                                  ((string-equal value "jis")         :jis)
                                  ((string-equal value "shift_jis")   :sjis)
                                  ((string-equal value "x-mac-roman") :macos-roman)
                                  
                                  ;; unknown encoding, default to utf-8
                                  (t (prog1 :utf-8
                                       (warn "Unknown character encoding ~s; using UTF-8..." value))))))))

;;; ----------------------------------------------------

(defun xml-standalone-info (p context)
  "Parse the standalone declaration attribute."
  (with-xml-parser (p "Invalid 'standalone' attribute")
      ("standalone" xml-eq (value xml-value))
    (prog1 t
      (sax:standalone context (cond ((string-equal value "no") nil)
                                    ((string-equal value "yes") t)
                                    
                                    ;; unknown attribute value for standalone
                                    (t (warn "Illegal standalone value ~s; using 'no'..." value)))))))

;;; ----------------------------------------------------

(defun xml-decl (p context)
  "Parse the <?xml..?> declaration attributes."
  (with-xml-parser (p "Malformed XML declaration") ("<?xml" t)

    ;; get the required version
    (unless (xml-version-info p context)
      (error "Missing version information from declaration"))

    ;; optionally get the encoding
    (xml-space p)
    (xml-encoding-info p context)

    ;; optionally get the standalone flag
    (xml-space p)
    (xml-standalone-info p context)

    ;; terminate the declaration properly
    (unless (xml-string p "?>")
      (error "Malformed XML declaration"))))

;;; ----------------------------------------------------

(defun xml-comment (p context)
  "Parse a comment."
  (with-xml-parser (p) ("<!--" (comment xml-scan "-->"))
    (prog1 t
      (sax:comment context comment))))

;;; ----------------------------------------------------

(defun xml-processing-instruction (p context)
  "Parse a processing instruction."
  (with-xml-parser (p "Malformed processing instruction")
      ("<?" (target xml-name) t (value xml-scan "?>"))
    (prog1 t
      (sax:processing-instruction context target value))))

;;; ----------------------------------------------------

(defun xml-misc (p context)
  "Whitespace, comments, and processing instructions."
  (loop until (or (xml-at-end-p p)
                  (not (or (xml-space p)
                           (xml-comment p context)
                           (xml-processing-instruction p context))))))

;;; ----------------------------------------------------

(defun xml-entity-ref (p context param-entity-p)
  "Read a named entity reference."
  (with-xml-parser (p "Malformed entity reference") ((name xml-name) #\;)
    (cond ((and (not param-entity-p) (string= name "lt")) #\<)
          ((and (not param-entity-p) (string= name "gt")) #\>)
          ((and (not param-entity-p) (string= name "amp")) #\&)
          ((and (not param-entity-p) (string= name "quot")) #\")
          ((and (not param-entity-p) (string= name "apos")) #\')
          
          ;; expand the parsed entity
          (t (if-let (value (sax:resolve-parsed-entity context name param-entity-p))
                 value
               (not (warn "Unknown entity ~s; skipping..." name)))))))

;;; ----------------------------------------------------

(defun xml-external-entity-decl (p context name &key parameter-entity-p)
  "Parse an external ENTITY declaration."
  (when-let (ref (xml-external-ref p))
    (destructuring-bind (system public)
        ref
      (let ((ndata (unless parameter-entity-p
                     (with-xml-parser (p "Malformed NDATA") (t "NDATA" t (type xml-name)) type))))
        (prog1 t
          (sax:external-entity-declaration context name parameter-entity-p system public ndata))))))

;;; ----------------------------------------------------

(defun xml-internal-entity-decl (p context name &key parameter-entity-p)
  "Parse an internal ENTITY declaration."
  (when-let (value (xml-value p))
    (prog1 t
      (let ((expanded (xml-expand-refs p context value :allow-pe-refs t)))
        (sax:internal-entity-declaration context name parameter-entity-p expanded)))))

;;; ----------------------------------------------------

(defun xml-entity-decl (p context)
  "Parse an ENTITY declaration."
  (with-xml-parser (p "Malformed ENTITY declaration") ("<!ENTITY" t)
    (let ((pe (with-xml-parser (p "Malformed parameter ENTITY declaration") (#\% t) t)))
      (with-xml-parser (p "Malformed~@[ parameter~] ENTITY declaration") ((name xml-name) t)
        (if (and (or (xml-internal-entity-decl p context name :parameter-entity-p pe)
                     (xml-external-entity-decl p context name :parameter-entity-p pe))
                 (progn
                   (xml-space p)
                   (xml-char p #\>)))
            t
          (error "Malformed ENTITY declaration"))))))

;;; ----------------------------------------------------

(defun xml-dtd (p context &optional recursive-p)
  "Parse the document type definition internal subset."
  (loop until (if recursive-p
                  (xml-at-end-p p)
                (xml-string p "]"))

        ;; try and parse each element in the DTD
        do (or (xml-space p)
               (xml-comment p context)
               (xml-processing-instruction p context)
               (xml-entity-decl p context)

               ;; parse notations
               ;; parse elements
               ;; parse attributes

               ;; expand parameter entity references
               (when (xml-char p #\%)
                 (when-let (pe (xml-entity-ref p context t))
                   (xml-dtd (make-instance 'xml-parser :string pe) context t)))

               ;; unknown element or character in DTD
               (error "Malformed DTD"))

        ;; 
        finally (return recursive-p)))
    

;;; ----------------------------------------------------

(defun xml-doctype (p context)
  "Parse the DOCTYPE declaration."
  (with-xml-parser (p "Malformed DOCTYPE") ("<!DOCTYPE" t (name xml-name))

    ;; optional external reference
    (xml-space p)
    (destructuring-bind (&optional system public)
        (xml-external-ref p)

      ;; start the DTD
      (sax:start-dtd context name system public)

      ;; parse the internal subset
      (xml-space p)
      (when (xml-string p "[")
        (xml-dtd p context)
        (xml-space p))

      ;; close the doctype
      (if (xml-string p ">")
          (sax:end-dtd context)
        (error "Malformed DOCTYPE")))))

;;; ----------------------------------------------------

(defun xml-inner-text (p context)
  "Parse inner text of an XML tag."
  (with-slots (string start end line)
      p
    (if-let (i (position #\< string :start start :end end))
        (unless (= i start)
          (let ((text (xml-expand-refs p context string :start start :end i)))
            (sax:inner-text context text)

            ;; increment the line count by how many newline were skipped
            (incf line (count #\newline string :start start :end i))

            ;; update the read position
            (setf start i)))
      (error "Missing close tag"))))

;;; ----------------------------------------------------

(defmethod xml-cdata ((p xml-parser) context)
  "Parse a CDATA section."
  (with-xml-parser (p "Malformed CDATA") ("<![CDATA[" (data xml-scan "]]>"))
    (prog1 t
      (sax:cdata context data))))

;;; ----------------------------------------------------

(defun xml-attribute (p context)
  "Parse a name=value tag attribute."
  (with-xml-parser (p "Malformed attribute") ((name xml-name) xml-eq (value xml-value))
    (list name (xml-expand-refs p context value))))

;;; ----------------------------------------------------

(defun xml-attributes (p context)
  "Parse a list of attributes."
  (loop for attr = (and (xml-space p) (xml-attribute p context)) while attr collect attr))

;;; ----------------------------------------------------

(defun xml-start-tag (p context)
  "Parse a start tag."
  (with-xml-parser (p "Malformed start tag") (#\< (name xml-name))
    (let ((atts (xml-attributes p context)))
      (sax:start-element context name atts))

    ;; check for an empty tag
    (cond ((xml-string p "/>")
           (prog1 :empty
             (sax:end-element context name)))

          ;; tag with content
          ((xml-char p #\>) :content)

          ;; failed to close properly
          (t (error "Malformed start tag ~s" name)))))

;;; ----------------------------------------------------

(defun xml-end-tag (p context)
  "Parse the end tag that matches."
  (with-xml-parser (p "Malformed close tag") ("</" (name xml-name))
    (xml-space p)
    (if (xml-char p #\>)
        (prog1 t
          (sax:end-element context name))
      (error "Malformed close tag ~s" name))))

;;; ----------------------------------------------------

(defun xml-content (p context)
  "Parse the contents of a tag."
  (do ()
      ((xml-end-tag p context) t)
    (or (xml-comment p context)
        (xml-inner-text p context)
        (xml-cdata p context)
        (xml-element p context)

        ;; malformed xml
        (error "Malformed XML"))))

;;; ----------------------------------------------------

(defun xml-element (p context)
  "Parse an empty or open tag."
  (case (xml-start-tag p context)
    (:content (xml-content p context))
    (:empty t)))
