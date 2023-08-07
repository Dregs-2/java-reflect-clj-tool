(ns com.52-hz.reflect)
(require '[clojure.reflect :as clojure-reflect])
(require '[clojure.data :as clojure-data])
(defrecord JAnnotation [name type attr])
(defrecord JClass0 [name package bases flags annotations members])
(defrecord JClass [name package bases flags annotations constructors methods fields])
(defrecord JMethod [name return-type declaring-class parameter-types exception-types flags annotations])
(defrecord JField [name type declaring-class flags annotations])
(defrecord JConstructor [name declaring-class parameter-types exception-types flags annotations])


(declare resolve-annotation-value)

(defn resolve-annotation [^java.lang.annotation.Annotation annotation]
  (let [methods (seq (.getDeclaredMethods (.getClass annotation)))
        type (symbol (.getName (.annotationType annotation)))
        name (.getSimpleName (.annotationType annotation))
        data (transient {})]
    (doseq [^java.lang.reflect.Method method methods]
      (when (not (#{"equals" "hashCode" "toString" "annotationType"} (.getName method)))
        (let [value (.invoke method annotation nil)
              value (resolve-annotation-value value)]

          (assoc! data (.getName method) value))))
    (JAnnotation. name type (persistent! data))
    )
  )

(defn resolve-annotation-value [value]
  (cond
    (nil? value) nil
    (instance? java.lang.annotation.Annotation value) (resolve-annotation value)
    (.isArray (.getClass value)) (mapv resolve-annotation-value (vec (seq value)))
    :else value)
  )


;region clojure2java
(defn clojure-method-2-java-method [^clojure.reflect.Method clojure-method]
  (let [clojure-method-name (name (:name clojure-method))
        clojure-method-parameter-types (:parameter-types clojure-method)
        declaring-class (resolve (:declaring-class clojure-method))
        java-class-methods (seq (.getDeclaredMethods declaring-class))]
    (first
      (filterv #(let [^java.lang.reflect.Method java-method %
                      java-method-name (.getName java-method)
                      java-method-parameter-types (seq (.getParameterTypes java-method))]
                  (and (= clojure-method-name java-method-name)
                       (let [[$1 $2] (clojure-data/diff (mapv name clojure-method-parameter-types) (mapv (memfn getName) java-method-parameter-types))] (and (nil? $1) (nil? $2))))
                  ) java-class-methods))
    )
  )

(defn clojure-field-2-java-field [^clojure.reflect.Field clojure-field]
  (let [clojure-field-name (name (:name clojure-field))
        clojure-field-type (name (:type clojure-field))
        declaring-class (resolve (:declaring-class clojure-field))
        java-class-fields (seq (.getDeclaredFields declaring-class))]
    (first
      (filterv #(let [^java.lang.reflect.Field java-field %
                      java-field-name (.getName java-field)
                      java-field-type (.getName (.getType java-field))]
                  (and (= clojure-field-name java-field-name)
                       (= clojure-field-type java-field-type))
                  ) java-class-fields))
    )
  )

(defn clojure-constructor-2-java-constructor [^clojure.reflect.Constructor clojure-constructor]
  (let [clojure-constructor-name (name (:name clojure-constructor))
        clojure-constructor-parameter-types (:parameter-types clojure-constructor)
        declaring-class (resolve (:declaring-class clojure-constructor))
        java-class-constructors (seq (.getDeclaredConstructors declaring-class))]
    (first
      (filterv #(let [^java.lang.reflect.Constructor java-constructor %
                      java-constructor-name (.getName java-constructor)
                      java-constructor-parameter-types (seq (.getParameterTypes java-constructor))]
                  (and (= clojure-constructor-name java-constructor-name)
                       (let [[$1 $2] (clojure-data/diff (mapv name clojure-constructor-parameter-types) (mapv (memfn getName) java-constructor-parameter-types))] (and (nil? $1) (nil? $2))))
                  ) java-class-constructors))
    )
  )
;endregion



;region annotation
(defmulti get-annotation-vector (fn [input] [(type input)]))

(defmethod get-annotation-vector :default [input]
  (let [data-vec (transient [])]
    (doseq [^java.lang.annotation.Annotation annotation (seq ((memfn getDeclaredAnnotations) input))]
      (conj! data-vec (resolve-annotation annotation)))
    (persistent! data-vec)
    )
  )

(defmethod get-annotation-vector [clojure.reflect.Method] [^clojure.reflect.Method method]
  (get-annotation-vector (clojure-method-2-java-method method)))

(defmethod get-annotation-vector [clojure.reflect.Field] [^clojure.reflect.Field field]
  (get-annotation-vector (clojure-field-2-java-field field)))
(defmethod get-annotation-vector [clojure.reflect.Constructor] [^clojure.reflect.Constructor constructor]
  (get-annotation-vector (clojure-constructor-2-java-constructor constructor)))

;endregion

(defn reflect0 [^Class class]
  (let [name (symbol (.getName class))
        package (.getName (.getPackage class))
        annotations (get-annotation-vector class)
        class (clojure-reflect/reflect class)
        bases (:bases class)
        flags (:flags class)
        members (transient [])]
    (doseq [member (:members class)]
      (when-not (and (instance? clojure.reflect.Method member) (clojure.string/starts-with? (:name member) "lambda"))
        (conj!
          members
          (cond
            (instance? clojure.reflect.Method member) (JMethod. (:name member)
                                                                (:return-type member)
                                                                (:declaring-class member)
                                                                (:parameter-types member)
                                                                (:exception-types member)
                                                                (:flags member)
                                                                (get-annotation-vector member))
            (instance? clojure.reflect.Field member) (JField. (:name member)
                                                              (:type member)
                                                              (:declaring-class member)
                                                              (:flags member)
                                                              (get-annotation-vector member))
            (instance? clojure.reflect.Constructor member) (JConstructor.
                                                             (:name member)
                                                             (:declaring-class member)
                                                             (:parameter-types member)
                                                             (:exception-types member)
                                                             (:flags member)
                                                             (get-annotation-vector member))))
        ))
    (JClass0. name package bases flags annotations (set (persistent! members)))))

(defn reflect [^Class class]
  (let [name (symbol (.getName class))
        package (.getName (.getPackage class))
        annotations (get-annotation-vector class)
        class (clojure-reflect/reflect class)
        bases (:bases class)
        flags (:flags class)
        constructors (transient [])
        methods (transient [])
        fields (transient [])]
    (doseq [member (:members class)]
      (when (instance? clojure.reflect.Constructor member)
        (conj! constructors (JConstructor.
                              (:name member)
                              (:declaring-class member)
                              (:parameter-types member)
                              (:exception-types member)
                              (:flags member)
                              (get-annotation-vector member)))
        )
      (when (and (instance? clojure.reflect.Method member) (not (clojure.string/starts-with? (:name member) "lambda")))
        (conj! methods (JMethod. (:name member)
                                 (:return-type member)
                                 (:declaring-class member)
                                 (:parameter-types member)
                                 (:exception-types member)
                                 (:flags member)
                                 (get-annotation-vector member)))
        )
      (when (instance? clojure.reflect.Field member)
        (conj! fields (JField. (:name member)
                               (:type member)
                               (:declaring-class member)
                               (:flags member)
                               (get-annotation-vector member)))
        )
      )
    (JClass. name package bases flags annotations (persistent! constructors) (persistent! methods) (persistent! fields))))
