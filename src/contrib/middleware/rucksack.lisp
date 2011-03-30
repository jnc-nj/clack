#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.rucksack
  (:use :cl
        :clack)
  (:import-from :rucksack
                :with-transaction
                :current-rucksack))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-rucksack> (<middleware>)
     ((rucksack :type rs:rucksack
                :initarg :rucksack
                :initform (current-rucksack)
                :accessor rucksack)))

(defmethod call ((this <clack-middleware-rucksack>) req)
  (with-transaction (:rs (rucksack this))
    (call-next this req)))

(doc:start)

@doc:NAME "
Clack.Middleware.Rucksack - Middleware for Rucksack connection management.
"

@doc:SYNOPSIS "
    (defvar *rs* (rs:open-rucksack \"db/\"))
    
    (builder
     (<clack-middleware-rucksack>
      :rucksack *rs*)
     app)

    ;; also same
    (builder
     <clack-middleware-rucksack>
     app)
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [Rucksack](https://github.com/arielnetworks/rucksack)
"
