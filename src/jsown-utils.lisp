(in-package :jsown)

;;; pretty print utils

(defun pretty-json (json &key (level 0))
  (if (stringp json)
      (pretty-json (jsown:parse json))
      (if (eq :obj (car json))
          ;; json is an object
          (format nil (format nil "{~~%~~{~~A~~^,~~%~~}~~%~~~A@A}" (* level 4))
                  (loop for (key . value) in (cdr json)
                     collect (format nil
                                     (format nil "~~~A@A\"~~A\": ~~:[~~S~~;~~A~~]" (* (1+ level) 4))
                                     ""
                                     key
                                     (listp value)
                                     (if (listp value)
                                         (pretty-json value :level (1+ level))
                                         value)))
                  "")
          ;; json ia an array
          (if (eq (length json) 0)
              "[]"
              (format nil (format nil "[~~%~~~A@A~~{~~A~~^,~~%~A~~}~~%~~~A@A]"
                                  (* (1+ level) 4)
                                  (format nil (format nil "~~~A@A" (* (1+ level) 4))
                                          "")
                                  (* level 4))
                      ""
                      (loop for value in json
                         collect (format nil
                                         (format nil "~~~A@A~~A" (* (1+ level) 4))
                                         (if (listp value)
                                             (pretty-json value :level (1+ level))
                                             value)
                                         ""))
                      "")))))

(defun pprint-json (json)
  (format t "~A~%" (pretty-json json))
  (values))

;;; accessor utils

(defmacro json-val (object key-name &key key)
  (let ((result-symbol (gensym "result")))
    `(let ((,result-symbol (cdr (assoc ,key-name (cdr ,object) :test #'equal))))
       (when ,result-symbol
         (if ,key
             (funcall ,key ,result-symbol)
             ,result-symbol)))))

(defmacro json-vals (object &rest keys)
  `(reduce #'(lambda (object key)
               (json-val object key))
           ',keys
           :initial-value ,object))

(defmacro json-update-in (object key-list new-value)
  "\(jsown-update-in object \(\"key1\" \"key2\"\) \"new-value\"\)"
  `(setf (json-vals ,object ,@key-list) ,new-value))

(export 'pretty-json)
(export 'pprint-json)
(export 'json-val)
(export 'json-vals)
(export 'json-update-in)
