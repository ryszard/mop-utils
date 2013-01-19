;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :asdf)
(defpackage mop-utils
  (:documentation "A set of Metaobject Protocol utilities.")
  (:use :cl #+sbcl :sb-mop #-sbcl :closer-mop)
  (:export #:defmetaclass #:class-name-of #:slots-of #:slot-names-of #:get-slot-of-by-name #:get-slot-by-name
           #:do-children #:do-macro-for-children))

(in-package :mop-utils)

(defmacro defmetaclass (metaclass supers slot-definitions &body body)
  "Macro for easy metaclass definition. It takes all the options
DEFCLASS takes, plus the following:

 - :VALIDATE-SUPERCLASSES, classes for which VALIDATE-SUPERCLASS
   methods (with METACLASS in the subclass position) will be created.

 - :VALIDATE-SUBCLASSES, a list of classes for which
   VALIDATE-SUPERCLASS methods (with METACLASS in the superclass
   position) will be created.

 - :SLOT-FIXTURES, a list of classes from which
   EFFECTIVE-SLOT-DEFINITION and DIRECT-SLOT-DEFINITION should
   inherit. This class should already exist at the time DEFMETACLASS is
   called.

DEFMETACLASS apart from creating the metaclass defines some additional things:

 - The classes <METACLASS>-DIRECT-SLOT-DEFINITION and
   <METACLASS>-EFFECTIVE-SLOT-DEFINITION (where <METACLASS> should be 
   substituted by METACLASS), which inherit from the fixtures and
   STANDARD-CLASS-DIRECT-SLOT-DEFINITION and
   STANDARD-CLASS-EFFECTIVE-SLOT-DEFINITION, respectively.

 - The methods DB-CLASS-{DIRECT|EFFECTIVE}-SLOT-DEFINITION.

"
  (let ((new-keywords (list :validate-superclasses :slot-fixtures :validate-subclasses))
        (subclasses (cdr (assoc :validate-subclasses body)))
        (superclasses (cdr (assoc :validate-superclasses body)))
        (slot-fixtures (cdr (assoc :slot-fixtures body))))
    `(progn 
      (defclass ,metaclass ,supers
        ,slot-definitions
        
        ,@(remove-if (lambda (sexp) (member (car sexp) new-keywords)) body))
      ,@(when slot-fixtures
             (let ((dir-slot-name (intern (format nil "~A-DIRECT-SLOT-DEFINITION" metaclass)))
                   (eff-slot-name (intern (format nil "~A-EFFECTIVE-SLOT-DEFINITION" metaclass))))
               
                 `((defclass ,dir-slot-name
                       (sb-mop:standard-direct-slot-definition ,@slot-fixtures)
                     ())
                   (defclass ,eff-slot-name
                       (sb-mop:standard-effective-slot-definition ,@slot-fixtures)
                     ())
                   (defmethod direct-slot-definition-class ((class ,metaclass)  &rest initargs)
                     (declare (ignore initargs))
                     (find-class ',dir-slot-name))
                   
                   (defmethod effective-slot-definition-class ((class ,metaclass)  &rest initargs)
                     (declare (ignore initargs))
                     (find-class ',eff-slot-name)))))
      ,@(when superclasses
              (loop :for super :in superclasses
                    :collect `(defmethod validate-superclass ((class ,metaclass)
                                                              (superclass ,super))
                               t)))
      ,@(when subclasses
              (loop :for sub :in subclasses
                    :collect `(defmethod validate-superclass ((class ,sub)
                                                              (superclass ,metaclass))
                               t)))
      (find-class ',metaclass))))

(defun class-name-of (object)
  "The class-name of the class of OBJECT."
  (class-name (class-of object)))


(defun slot-names-of (object)
  "List of names of the slots of OBJECT."
  (mapcar #'slot-definition-name (slots-of object)))

(defun slots-of (object)
  "List of slots of OBJECT."
  (class-slots (class-of object)))

(defun get-slot-by-name (class name)
  "Get slot whose name is `NAME' from `CLASS'."
  (find-if (lambda (slot) (eql (slot-definition-name slot) name)) (class-slots class)))

(defun get-slot-of-by-name (object name)
  "Get slot whose name is `NAME' of the class of `OBJECT'."
  (get-slot-by-name (class-of object) name))

(defmacro do-children ((var class-name) &body body)
  "For every subclass of class named by CLASS-NAME, execute BODY with VAR bind to it."
  (let ((children (class-direct-subclasses (find-class class-name))))
  `(loop :for ,var :in ',children
    :do ,@body)))

(defmacro do-macro-for-children (macro class-name)
  "Use MACRO with the names of all the subclasses of the class named
by CLASS-NAME."
  (let ((child (gensym "child"))
        (child-name (gensym "child-name")))
    `(do-children  (,child ,class-name)
      (let ((,child-name (class-name ,child)))
        (eval `(,',macro ,,child-name))))))
    