(ns cider.enrich-classpath.source-analysis
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string])
  (:import
   (java.io File)
   (java.util.zip ZipEntry ZipInputStream)))

(defmacro while-let [[sym expr] & body]
  `(loop [~sym ~expr]
     (when ~sym
       ~@body
       (recur ~expr))))

(defn ls-zip [target]
  (let [v (transient [])
        zis (-> target io/input-stream ZipInputStream.)]
    (try
      (while-let [entry (-> zis .getNextEntry)]
        (conj! v (-> ^ZipEntry entry .getName)))
      (finally
        (-> zis .close)))
    (persistent! v)))

(defn bad-source? [[id version _classifier-keyword classifier]]
  {:pre [(symbol? id)
         (string? version)
         (keyword? _classifier-keyword)
         (string? classifier)]}
  (when (#{"sources"} classifier)
    (let [[groupid artifactid :as x] (-> id str (string/split #"/"))
          artifactid (or artifactid groupid) ;; concise notation
          _ (assert artifactid x)
          segments (-> groupid (string/split #"\."))
          home (System/getProperty "user.home")
          ;; XXX honor .m2 env var?
          file (apply io/file home ".m2" "repository" segments)
          artifact (str artifactid "-" version "-" classifier  ".jar")
          ^File file (io/file file artifactid version artifact)]
      (when (-> file .exists)
        (->> file
             ls-zip
             (not-any? (fn [^String s]
                         (or (-> s (.endsWith ".java"))
                             (-> s (.endsWith ".scala"))))))))))

(comment
  ;; these cases are correct
  (bad-source? '[com.zaxxer/HikariCP "4.0.3" :classifier "sources"]) ;; false
  (bad-source? '[org.clojure/tools.namespace "0.3.1" :classifier "sources" :exclusions [[*]]]) ;; true
  )
