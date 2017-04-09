(ns email-reply-parser.core-test
  (:require [clojure.test :refer :all]
            [email-reply-parser.core :refer :all]))

(defn get-email [name]
  {:pre  [(string? name)]
   :post [(string? %)]}
  (let [file-url (clojure.java.io/resource (str "resources\\" name ".txt"))]
    (slurp file-url)
    )
  )

(deftest captures-date-string
  (let [message (read-from-string (get-email "email_1_4"))
        fragments (:fragments message)]
    (is (-> fragments count (= 3)))
    (is (-> fragments (nth 0) .-content (.contains "Awesome") true?))
    (is (-> fragments (nth 1) .-content (.contains "On") true?))
    (is (-> fragments (nth 1) .-content (.contains "Loader") true?))
    )
  )

(deftest reads-top-post
  (let [message (read-from-string (get-email "email_1_3"))
        fragments (:fragments message)]
    (is (-> fragments count (= 5)))
    )
  )

(deftest multiLine-reply-header
  (let [message (read-from-string (get-email "email_1_6"))
        fragments (:fragments message)]
    (is (-> fragments count (= 3)))
    (is (-> fragments (nth 0) .-content (.contains "I get") true?))
    (is (-> fragments (nth 1) .-content (.contains "On") true?))
    )
  )

(deftest complext-body-with-one-fragment
  (let [message (read-from-string (get-email "email_1_5"))
        fragments (:fragments message)]
    (is (-> fragments count (= 1)))
    )
  )

(deftest window-line-endings
  (let [message (read-from-string (get-email "email_1_7"))
        fragments (:fragments message)]
    (is (-> fragments count (= 3)))
    (is (-> fragments (nth 0) .-content (.contains ":+1:") true?))
    (is (-> fragments (nth 1) .-content (.contains "On") true?))
    (is (-> fragments (nth 1) .-content (.contains "Steps 0-2") true?))
    )
  )

(deftest correct-signature
  (let [message (read-from-string (get-email "correct_sig"))
        fragments (:fragments message)]
    (is (-> fragments count (= 2)))
    (is (-> fragments (nth 0) .-content (.contains "--") true?))

    (is (->> fragments (map :quoted) (= [false false])))
    (is (->> fragments (map :signature) (= [false true])))
    (is (->> fragments (map :hidden) (= [false true])))
    )
  )

(deftest simple-body
  (let [message (read-from-string (get-email "email_1_1"))
        fragments (:fragments message)]
    (is (-> fragments count (= 3)))
    (is (-> fragments (nth 0) .-content (.contains "folks") true?))
    (is (-> fragments (nth 2) .-content (.contains "riak-users") true?))

    (is (->> fragments (map :signature) (= [false true true])))
    (is (->> fragments (map :hidden) (= [false true true])))
    )
  )

(deftest reads-bottom-message
  (let [message (read-from-string (get-email "email_1_2"))
        fragments (:fragments message)]
    (is (-> fragments count (= 6)))
    (is (->> fragments (map :quoted) (= [false true false true false false])))
    (is (->> fragments (map :signature) (= [false false false false false true])))
    (is (->> fragments (map :hidden) (= [false false false true true true])))

    (is (-> fragments (nth 0) .-content (.contains "Hi") true?))
    (is (-> fragments (nth 1) .-content (.contains "On") true?))
    (is (-> fragments (nth 3) .-content (.contains ">") true?))
    (is (-> fragments (nth 5) .-content (.contains "riak-users") true?))
    )
  )

(deftest sent-from-iphone
  (let [message (read-from-string (get-email "email_1_2"))]
    (is (-> message get-reply (.contains "Sent from my iPhone") false?))
    )
  )

(deftest email-one-is-not-on
  (let [message (read-from-string (get-email "email_one_is_not_on"))]
    (is (-> message get-reply (.contains "On Oct 1, 2012, at 11:55 PM, Dave Tapley wrote:") false?))
    )
  )

(deftest partial-quote-header
  (let [message (read-from-string (get-email "email_partial_quote_header"))
        reply (get-reply message)]
    (is (-> reply (.contains "On your remote host you can run:") true?))
    (is (-> reply (.contains "telnet 127.0.0.1 52698") true?))
    (is (-> reply (.contains "This should connect to TextMate") true?))
    )
  )

(deftest partial-quote-header
  (let [message (read-from-string (get-email "email_1_2"))
        reply (get-reply message)]
    (is (-> reply (.contains "You can list the keys for the bucket") true?))
    )
  )

(deftest first-driftt-reply
  (let [message (read-from-string (get-email "driftt_1"))
        reply (get-reply message)]
    (is (-> reply (= "hey notifications!") true?))
    )
  )