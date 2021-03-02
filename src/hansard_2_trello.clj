(ns hansard-2-trello
  (:require [clj-http.client :as http]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [net.cgrand.enlive-html :as html]))

(defn- commons-url [date]
  (str "https://hansard.parliament.uk/commons/" date))

(defn- lords-url [date]
  (str "https://hansard.parliament.uk/lords/" date))

(defn- hansard-item-urls [url]
  (map (fn [node]
         (str "https://hansard.parliament.uk"
              (get-in node [:attrs :href])))
       (html/select (html/html-resource (java.net.URL. url))
                    [:a.card-section])))

(defn- hansard-item [url]
  (let [page (html/html-resource (java.net.URL. url))
        title (-> (html/select page [:h1])
                  (first)
                  (html/text))
        content (-> (html/select page [:.block-page])
                    (first)
                    (html/text))
        word-count (count (string/split content #"\s+"))
        reading-time-estimate (inc (int (/ word-count 200)))]
    {:title title
     :estimate reading-time-estimate
     :url url}))

(defn- items [date]
  (concat (pmap (comp (partial merge {:house :commons
                                      :date date})
                      hansard-item)
                (hansard-item-urls (commons-url date)))
          (pmap (comp (partial merge {:house :lords
                                      :date date})
                      hansard-item)
                (hansard-item-urls (lords-url date)))))

(defn- create-card [config item]
  (let [{:keys [key token list-id estimate-field-id house->label-id]} config
        card-res (http/post "https://api.trello.com/1/cards"
                            {:content-type :json
                             :accept :json
                             :as :json
                             :query-params {:key key
                                            :token token
                                            :name (:title item)
                                            :desc (:url item)
                                            :start (str (:date item) "T08:00:00.000Z")
                                            :idList list-id
                                            :idLabels [(get house->label-id (:house item))]}})
        card-id (get-in card-res [:body :id])]
    (http/put (str "https://api.trello.com/1/card/" card-id "/customField/" estimate-field-id "/item")
              {:content-type :json
               :accept :json
               :as :json
               :query-params {:key key
                              :token token}
               :form-params {:value {:number (str (:estimate item))}}})
    nil))

(defn -main [& args]
  (let [date (first args)]
    (run! (partial create-card {:key (env :trello-key)
                                :token (env :trello-token)
                                :list-id (env :trello-list-id)
                                :estimate-field-id (env :trello-estimate-field-id)
                                :house->label-id {:commons (env :trello-commons-label-id)
                                                  :lords (env :trello-lords-label-id)}})
          (items date))))

(comment
  (hansard-item-urls (commons-url "2021-03-01"))
  (hansard-item "https://hansard.parliament.uk/commons/2021-03-01/debates/a06314da-5d25-450d-ae52-0e27df20a45a/DraftMayoralAndPoliceAndCrimeCommissionerElections(CoronavirusNominationOfCandidates)(Amendment)Order2021DraftPoliceAndCrimeCommissionerElections(Wels")
  (items "2021-03-01"))