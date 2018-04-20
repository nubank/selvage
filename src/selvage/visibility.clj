(ns selvage.visibility
  (:import [com.google.common.io BaseEncoding]
           [java.util Random]))

(def ^:dynamic *cid* nil)

(def default-cid "DEFAULT")
(defn current-cid [] (or *cid* default-cid))

(defn- ^String encode-base32 [in-bytes]
  (-> (BaseEncoding/base32) .omitPadding (.encode in-bytes)))

(defn random-string []
  (let [random-bytes (byte-array 4)
        base64-string (do (.nextBytes (Random.) random-bytes)
                          (encode-base32 random-bytes))]
    (.substring base64-string 0 (- (count base64-string) 2))))

(defn split-cid
  ([] (split-cid (current-cid)))
  ([input-cid]
   (str input-cid "." (random-string))))

(defmacro with-split-cid
  ([body]
   `(binding [*cid* (split-cid (current-cid))]
      ~body))
  ([initial-cid body]
   `(binding [*cid* (split-cid (or ~initial-cid default-cid))]
      ~body)))

