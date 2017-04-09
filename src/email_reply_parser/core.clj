(ns email-reply-parser.core
  (:require [clojure.string :as string])
  (:import (java.util.regex Pattern)))

(defn boolean? [operand]
  (or (= operand true) (= operand false))
  )

;;------------FRAGMENT RECORD-----------------

(defrecord Fragment [signature hidden quoted content lines])

(defn fragment
  "Constructor for Fragment record"
  ([signature hidden quoted content lines]
    {:pre  [(boolean? signature)
            (boolean? hidden)
            (boolean? quoted)
            (string? content)
            (vector? lines)
            (empty? (->> lines (filter #(not (instance? String %)))))]
     :post [(instance? Fragment %)]}
    (->Fragment signature hidden quoted content lines)
    )
  ([quoted first-line]
    {:pre  [(boolean? quoted)
            (string? first-line)]
     :post [(instance? Fragment %)]}
    (map->Fragment {:signature false :hidden false :quoted quoted :lines (vector first-line)})
    )
  )

(defn finish [fragment]
  {:pre  [(instance? Fragment fragment)]
   :post [(instance? Fragment %)]}
  (-> fragment
      (assoc :content (->> (:lines fragment) reverse (string/join "\n") string/trim))
      (assoc :lines (vector))
      )
  )

;;------------EMAIL-MESSAGE RECORD-----------------

(defrecord EmailMessage [text fragments found-visible fragment])

(def sig-regex "(\u2014|--|__|-\\w)|(^Sent from my (\\w+\\s*){1,3})")
(def quote-hdr-regex "^:etorw.*nO")
(def multi-quote-hdr-regex "(?!On.*On\\s.+?wrote:)(On\\s(.+?)wrote:)")
(def quoted-regex "(>+)")

(defn email-message
  "Constructor for EmailMessage record"
  [text]
  {:pre  [(not (empty? text))]
   :post [(instance? EmailMessage %)]}
  (->EmailMessage (string/replace text "\r\n" "\n") (vector) false nil)
  )

(defn- quote-header [line]
  {:pre  [(string? line)]
   :post [(boolean? %)]}
  (-> (Pattern/compile quote-hdr-regex) (.matcher (-> line reverse string/join)) .lookingAt )
  )

(defn- is-string-empty [content]
  {:pre  [(string? content)]
   :post [(boolean? %)]}
  (-> content string/trim .isEmpty)
  )

(defn read-from-email-message
  "Creates EmailMessage record"
  [email-message]
  {:pre  [(instance? EmailMessage email-message)]
   :post [(instance? EmailMessage %)]}
  (let [email-message-local (atom email-message)
        working-text (atom (:text @email-message-local))
        multi-quote (.matcher (Pattern/compile (.pattern (Pattern/compile multi-quote-hdr-regex)) (bit-or Pattern/MULTILINE Pattern/DOTALL)) @working-text)
        finish-fragment (fn []
                          (when (true? (not (nil? (:fragment @email-message-local))))
                            (swap! email-message-local (fn [m]
                                                         (assoc m :fragment (finish (:fragment m)))
                                                         ))
                            (when (false? (:found-visible @email-message-local))
                              (if (or (true? (-> @email-message-local :fragment :quoted))
                                      (true? (-> @email-message-local :fragment :signature))
                                      (is-string-empty (-> @email-message-local :fragment :content)))
                                (swap! email-message-local (fn [m]
                                                             (assoc-in m [:fragment :hidden] true)
                                                             ))
                                (swap! email-message-local (fn [m]
                                                             (assoc m :found-visible true)
                                                             ))
                                )
                              )
                            (swap! email-message-local (fn [m]
                                                         (assoc m :fragments (-> m :fragments (conj (:fragment m))) )
                                                         ))
                            )
                          (swap! email-message-local (fn [m]
                                                       (assoc m :fragment nil)
                                                       ))
                          )
        scan-line (fn [line]
                    (let [line-local (atom (string/trim line))]
                      (when (true? (-> (Pattern/compile sig-regex) (.matcher @line-local) .lookingAt))
                        (reset! line-local (string/triml line))
                        )
                      (let [is-quoted (-> (Pattern/compile quoted-regex) (.matcher @line-local) (.lookingAt))]
                        (when (true? (and
                                       (and (not (nil? (:fragment @email-message-local))) (is-string-empty line))
                                       (-> (Pattern/compile sig-regex) (.matcher (last (-> (:fragment @email-message-local) (.-lines)))) (.lookingAt)))
                                     )
                          (swap! email-message-local (fn [m]
                                                       (assoc-in m [:fragment :signature] true)
                                                       ))
                          (finish-fragment)
                          )

                        (if (and (not (nil? (:fragment @email-message-local)))
                                 (or (= (-> @email-message-local :fragment :quoted) is-quoted)
                                     (and (-> @email-message-local :fragment :quoted)
                                          (or (quote-header @line-local)
                                              (is-string-empty @line-local)))))
                          (swap! email-message-local (fn [m]
                                                       (assoc-in m [:fragment :lines]
                                                                 (conj (-> m :fragment :lines) @line-local))
                                                       )
                                 )
                          (do
                            (finish-fragment)
                            (swap! email-message-local (fn [m]
                                                         (assoc m :fragment (fragment is-quoted @line-local))
                                                         ))
                            )
                          )
                        )
                      )
                    )]

    (add-watch email-message-local :test-watch (fn [k r old new]
                                                 (when (true? (vector? new))
                                                   (print "!!!!")
                                                   )
                                                 ))

    (when (true? (.find multi-quote))
      (swap! working-text
             #(let [new-quote-header (-> (.group multi-quote) (string/replace "\n" ""))]
               (-> (Pattern/compile (.pattern (Pattern/compile multi-quote-hdr-regex)) Pattern/DOTALL)
                   (.matcher %)
                   (.replaceAll new-quote-header))
               ))
      )

    (let [lines (-> @working-text string/split-lines reverse (conj ""))]
      (doseq [line lines]
        (scan-line line)
        )
      )

    (finish-fragment)

    (swap! email-message-local (fn [m]
                                 (assoc m :fragments (-> m :fragments reverse))
                                 ))

    (identity @email-message-local)
    )
  )

(defn read-from-string [content]
  {:pre  [(string? content)]
   :post [(instance? EmailMessage %)]}
  (-> (email-message content) read-from-email-message)
  )

(defn get-reply [email-message]
  (string/join "\n"
               (->> (:fragments email-message)
                    (filter #(not (or (:hidden %) (:quoted %))))
                    (map :content)))
  )
