(asdf:defsystem :jsown-utils
  :name "jsown-utils"
  :description "Utilities for Common Lisp JSON library jsown"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "MIT"
  :depends-on ("jsown"
               "closer-mop")
  :components ((:static-file "jsown-utils.asd")
               (:module "src"
                        :serial t
                        :components ((:file "jsown-utils")))))
