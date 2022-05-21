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

;;; serialize utils

(defmethod to-jsown ((object integer))
  object)

(defmethod to-jsown ((object fixnum))
  object)

(defmethod to-jsown ((object bignum))
  object)

(defmethod to-jsown ((object ratio))
  object)

(defmethod to-jsown ((object string))
  object)

(defmethod to-jsown ((object single-float))
  object)

(defmethod to-jsown ((object double-float))
  object)

(defmethod to-jsown ((object list))
  (mapcar #'to-jsown
          object))

(defmethod to-jsown ((object vector))
  (loop
    for element across object
    collect (to-jsown element)))

(defmethod to-jsown ((object hash-table))
  (append (list :obj)
          (maphash #'(lambda (key value)
                       (cons (write-to-string key)
                             (to-jsown value)))
                   object)))

(defmethod to-jsown ((object t))
  (append (list :obj)
          (mapcar #'(lambda (slot)
                      (let* ((slot-name (closer-mop:slot-definition-name slot))
                             (slot-value (slot-value object slot-name)))
                        (cons (string-downcase
                               (symbol-name slot-name))
                              (to-jsown slot-value))))
                  (closer-mop:class-slots (class-of object)))))

(defmethod to-json ((object t))
  "General-purpose to-json for all objects."
  (jsown:to-json (to-jsown object)))


(export 'pretty-json)
(export 'pprint-json)
(export 'json-val)
(export 'json-vals)
(export 'json-update-in)
