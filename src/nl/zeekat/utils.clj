(ns nl.zeekat.utils
  (:use [clojure.contrib [def :only (defalias)]]
        clojure.contrib.logging)
  (:import java.security.MessageDigest
           java.net.URLDecoder
           java.util.UUID))

;;; This file contains simple extensions to clojure.core

(defmacro with-fields
  [fields m & body]
  `(let [{:keys ~fields} ~m]
     ~@body))

(defn current-time []
  (System/currentTimeMillis))

(defn yesterday []
  (- (System/currentTimeMillis) (* 60 60 24 1000)))

(defn digest-sha1 [input]
  (let [d (java.security.MessageDigest/getInstance "SHA-1")] 
    (apply str (map #(format "%02x" %) (.digest d (.getBytes (str input)))))))

(def integers (iterate inc 0))

(defmacro #^{:doc "alias (using defalias) a bunch of vars from another namespace into the current one"}
  alias-from [n & vars]
  `(do ~@(map #(list 'clojure.contrib.def/defalias % (symbol (name n) (name %))) vars)))

(defn limit-string [length s]
  (if (< (.length s) length)
      s
      (str (.substring s 0 (dec length)) "...")))

(defn gen-uuid
  []
  (str (UUID/randomUUID)))

(defn url-decode
  [u]
  (URLDecoder/decode u))

(defn debug-exp [exp]
  (warn [:DEBUG exp])
  exp)

(defn merge-in
  "like assoc-in, but attempts to merge the given associative value
into the value already present. if no value is present, or the current
value isn't associative, works like assoc-in"
  [m [k & ks] as]
  (let [val (get m k)]
    (if ks
     (merge-in m k (merge-in val ks as))
     (if (associative? val)
       (assoc m k (merge val as))
       (assoc m k as)))))
