;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :asdf)

(defpackage mop-utils-system
  (:use :cl :asdf))

(in-package :mop-utils-system)

(defsystem mop-utils
  :name "MOP-utils"
  :version "0.0.1"
  :components ((:file "mop-utils"))
  :description "A set of Metaobject Protocol utilities."
  :author "Ryszard Szopa"
  :depends-on (#-sbcl :closer-mop))
  