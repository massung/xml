(defpackage :xml-asd
  (:use :cl :asdf))

(in-package :xml-asd)

(defsystem :xml
  :name "xml"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Non-validating XML parsing for Common Lisp."
  :serial t
  :components ((:file "xml"))
  :depends-on ("parse" "re" "lexer" "markup"))
