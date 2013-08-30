(defpackage :xml-asd
  (:use :cl :asdf))

(in-package :xml-asd)

(defsystem :xml
  :name "xml"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "XML parsing for LispWorks."
  :serial t
  :components ((:file "xml"))
  :depends-on ("lexer"))
