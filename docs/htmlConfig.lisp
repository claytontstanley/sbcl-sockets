(require 'asdf)

;if we're using the new asdf compiler, disable-output-translations so that compiled fasl files
;reside next to the source files (like the older asdf compiler did)
(in-package :asdf)
(if (fboundp  'disable-output-translations)
    (disable-output-translations))

;jump back in to the default common-lisp-user package
(in-package :common-lisp-user)

(load (format nil "~a/cldoc/cldoc.asd"
	(get-pandoric #'args 'platform)))

(require 'cldoc)

;each name in names will be macroexpanded-1 before trying to document
;cldoc, by default, will document defun, defmacro, defmethod, etc.
;in order to document macros that have been written that write code
;that compiles down to defun, defmacro, defmethod, etc., you have
;to tell cldoc to macroexpand each of these macros first, before trying
;to parse the forms to grab the documentation
;this allows you to write macros to extend the language (keep the power of lisp)
;while still documenting the macros you write (keep the readability of cldoc)
(defmacro macroexpand-for-documenting (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
	    `(cludg::define-descriptor-handler ,name (form)
	       (symb ',name)
	       (values nil :restart (list (let ((*print-case* :upcase))
					    (macroexpand-1 form))))))
	  names)))

;macroexpand the macros that write code that defines other macros/functions/methods
;this way the macros/functions/methods that the macros write gets documented as well
(macroexpand-for-documenting METHODS DEFMACRO/G! DEFMACRO! DEFTEST)

(defun generate-docs ()
    (cldoc:extract-documentation 'cldoc:html "html"
     '("server.lisp")
     :table-of-contents-title
     "Common Lisp Universal Documentation Generator"))
