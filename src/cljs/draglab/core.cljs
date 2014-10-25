(ns draglab.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require [cljs.core.async :as async :refer [put! chan dropping-buffer <! >! timeout pub sub unsub unsub-all]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [goog.events :as events]
            [goog.style :as gstyle])
  (:import [goog.ui IdGenerator]
           [goog.events EventType]))

(extend-type js/HTMLCollection
  ISeqable
  (-seq [array] (array-seq array 0)))

(defn log [o]
  (.log js/console o))

(defn trace [o]
  (.trace js/console o))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn in-or-eq?
  "true if x is elm or contains elm"
  [x elm]
  (or (= x elm) (in? x elm)))

(defn guid []
  (.getNextUniqueId (.getInstance IdGenerator)))

(defn to? [owner next-props next-state k]
  (or (and (not (om/get-render-state owner k))
           (k next-state))
      (and (not (k (om/get-props owner)))
           (k next-props))))

(defn from? [owner next-props next-state k]
  (or (and (om/get-render-state owner k)
           (not (k next-state)))
      (and (k (om/get-props owner))
           (not (k next-props)))))

(defn gsize->vec [size]
  [(.-width size) (.-height size)])

(defn location [e]
  [(.-clientX e) (.-clientY e)])

(defn element-offset [el]
  (let [offset (gstyle/getPageOffset el)]
    [(.-x offset) (.-y offset)]))

(defn calc-index [el]
  "Determine sorted index and then lookup original index."
  (let [children (.-children (.-parentNode el))
        dom-index (.call (.-indexOf (.-prototype js/Array)) children el)]
    dom-index))

(defn child-offset [container-offset]
  (fn [child]
    (let [offset (element-offset child)
          [_ h] (gsize->vec (gstyle/getSize child))]
      (conj (mapv - offset container-offset) h))))

(defn children-metrics [container-el]
  (let [children (.-children container-el)
        container-offset (element-offset container-el)]
    (mapv (child-offset container-offset) children)))

(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn indexv [value coll]
  (first-index-of #(= % value) coll))

(defn index-of [x v]
  (loop [i 0 v (seq v)]
    (if v
      (if (= x (first v))
        i
        (recur (inc i) (next v)))
      -1)))

(defn limit [num min max]
  (cond
    (> num max) max
    (< num min) min
    :else num))

(defn below? [y-pos]
  (fn [[_ y-offset height]]
    (< y-pos (+ y-offset (/ height 2)))))

(defn vertical-sort-index [metrics y-pos]
  (let [below (map (below? y-pos) metrics)
        mcount (count metrics)
        idx (count (filter not below))]
    (limit idx 0 (- mcount 1))))

(defn dragging? [owner]
  (om/get-state owner :dragging))

(defn drag-start [e item owner]
  (when-not (dragging? owner)
    (let [el          (om/get-node owner "draggable")
          state       (om/get-state owner)
          drag-start  (location e)
          el-offset   (element-offset el)
          drag-offset (vec (map - el-offset drag-start))]
      (om/set-state! owner :dragging true)
      (doto owner
        (om/set-state! :location el-offset)
        (om/set-state! :drag-offset drag-offset))
      (when-let [c (om/get-shared owner :drag-chan)]
        (put! c
              {:event :drag-start
               :owner owner
               :index (calc-index el)
               :location (vec (map + drag-start drag-offset))})))))

(defn drag-stop [e item owner]
  (when (dragging? owner)
    (let [state (om/get-state owner)]
      (when (:dragging state)
        (om/set-state! owner :dragging false))
      ;; rendering order issues otherwise
      (doto owner
        (om/set-state! :location nil)
        (om/set-state! :drag-offset nil))
      (when-let [c (om/get-shared owner :drag-chan)]
        (put! c {:event :drag-stop :owner owner})))))

(defn drag [e item owner]
  (let [state (om/get-state owner)]
    (when (dragging? owner)
      (let [loc ((or (:constrain state) identity)
                 (vec (map + (location e) (:drag-offset state))))]
        (om/set-state! owner :location loc)
        (when-let [c (om/get-shared owner :drag-chan)]
          (put! c {:event :drag :location loc :owner owner}))))))

(defn draggable [cursor owner]
  (reify
    om/IInitState
    (init-state [_]
      {:dragging false})
    om/IDidMount
    (did-mount [_]
      (let [dims (-> (om/get-node owner "draggable")
                     gstyle/getSize gsize->vec)]
        (om/set-state! owner :dimensions dims)))
    om/IWillUpdate
    (will-update [_ next-props next-state]
      ;; begin dragging, need to track events on window
      (when (or (to? owner next-props next-state :dragging))
        (let [mouse-up   #(drag-stop % @next-props owner)
              mouse-move #(drag % @next-props owner)]
          (om/set-state! owner :window-listeners
                         [mouse-up mouse-move])
          (doto js/window
            (events/listen EventType.MOUSEUP mouse-up)
            (events/listen EventType.MOUSEMOVE mouse-move))))
      ;; end dragging, cleanup window event listeners
      (when (from? owner next-props next-state :dragging)
        (log "end draggin")
        (let [[mouse-up mouse-move]
              (om/get-state owner :window-listeners)]
          (doto js/window
            (events/unlisten EventType.MOUSEUP mouse-up)
            (events/unlisten EventType.MOUSEMOVE mouse-move)))))
    om/IRenderState
    (render-state [_ state]
      (let [dragging (dragging? owner)
            style (cond
                    dragging
                    (let [[x y] (:location state)
                          [w h] (:dimensions state)]
                      #js {:position "absolute"
                           :top y :left x :z-index 1
                           :width w :height h})
                    :else
                    #js {:position "static" :z-index 0})]
        (dom/div #js {:ref "draggable"
                      :style style
                      :className (str "draggable"
                                      (when dragging " dragging"))
                      :onMouseDown #(drag-start % @cursor owner)
                      :onMouseUp #(drag-stop % @cursor owner)
                      :onMouseMove #(drag % @cursor owner)}
         (om/build (:view state) cursor))))))

(defn subscribe-to-drags [pub chan]
  (sub pub :drag-start chan)
  (sub pub :drag-stop chan)
  (sub pub :drag chan))

(defn unsubscribe-from-drags [pub chan]
  (unsub pub :drag-start chan)
  (unsub pub :drag-stop chan)
  (unsub pub :drag chan))

(defn sort-order [items field]
  (vec (map-indexed identity items)))

(defn insert [vec pos item]
  "https://groups.google.com/d/msg/clojure/zjZDSziZVQk/iqnLhpGy3aoJ"
  (apply merge (subvec vec 0 pos) item (subvec vec pos)))

;; bug when sorting up, or between items?
;; requires some investigatoin :'(
(defn insert-at [x idx ignore v]
  (log (str "insert-at " x " " idx " " ignore " " (pr-str v)))
  (let [len (count v)
        prev-index (indexv ignore v)
        ignore (nth v ignore)]
    (loop [i 0 v v ret []]
      (log (str "loop " i " " v " " (pr-str ret)))
      (log (str "previ " prev-index))
      (if (>= i len)
        (conj ret x)
        (let [y (first v)]
          (if (= y ignore)
            ;(if (== i idx)
              ;(if (== idx (- len 1))
            ;  (if (== prev-index i)
            ;    (into (conj ret (first v) x) (next v))
            ;    (into (conj ret x) v))
            (recur i (next v) (conj ret y))
            (if (== i idx)
              (into (conj ret x) v)
              (recur (inc i) (next v) (conj ret y)))))))))

(defn from-loc [v1 v2]
  (vec (map - v2 v1)))

(defn sorting? [owner]
  (om/get-state owner :sorting))

(defn sortable-spacer [height]
  (dom/div
    #js {:key "spacer-cell"
         :className "spacer-cell"
         :style #js {:height height}}))

(defn start-sort [owner e]
  (let [state (om/get-state owner)
        sort (:sort state)
        idx (:index e)
        sort-idx (nth sort idx)]
    (log idx)
    (log sort-idx)
    (doto owner
      (om/set-state! :sorted-index sort-idx)
      (om/set-state! :initial-sort sort)
      (om/set-state! :sorting idx)
      (om/set-state! :drop-index idx)
      (om/set-state! :sort (insert-at ::spacer idx idx sort)))))

(defn update-drop [owner e]
  (when (sorting? owner)
    (let [state (om/get-state owner)
          sorting (:sorting state)
          initial-sort (:initial-sort state)
          metrics (:metrics state)
          loc (:location e)
          [_ y] (from-loc (:location state) loc)
          new-idx (vertical-sort-index metrics y)
          nilog (log new-idx)
          new-sort (insert-at ::spacer new-idx sorting initial-sort)
          nslog (log (pr-str new-sort))]
      (doto owner
        (om/set-state! :drop-index new-idx)
        (om/set-state! :sort new-sort)))))

(defn update-order [items k new-sort]
  (for [item items
        :let [idx (get item k)]]
    (assoc item k (indexv idx new-sort))))

;; not as simple as the om sortable example
;; we need to be reset the positions (in cursor) of each element
;; use the changing "was-sorting" as an entrypoint
;; @TODO investigate using `sorting` instead of `sorted-index`
;; @TODO set the sorting key so that "position" is not hardcoded here
(defn handle-drop [owner e]
  (when (sorting? owner)
    (let [{:keys [sort sorted-index]} (om/get-state owner)
          sort (->> sort
                    (remove #{sorted-index})
                    (replace {::spacer sorted-index})
                    vec)
          ]
      (doto owner
        (om/set-state! :sorted-index nil)
        (om/set-state! :sorting nil)
        (om/set-state! :drop-index nil)
        (om/set-state! :initial-sort nil)
        (om/set-state! :sort sort))
      )))

(defn handle-drag-event [owner e]
  (case (:event e)
    :drag-start (start-sort owner e)
    :drag-stop (handle-drop owner e)
    :drag (update-drop owner e)))

;; potential variations
;; * sortable
;; * droppable
(defn draggables [cursor owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [mode (om/get-state owner :mode)
            event-pub (om/get-shared owner :drag-pub)
            event-listener (chan)
            sortable (in-or-eq? mode :sortable)
            droppable (in-or-eq? mode :droppable)]
        (when (or sortable droppable)
          (when sortable
            (om/set-state! owner :sort (sort-order cursor "position")))
          (subscribe-to-drags event-pub event-listener)
          (om/set-state! owner :event-listener event-listener)
          (go-loop []
            (let [e (<! event-listener)]
                   (handle-drag-event owner e)
                   (recur))))))
    om/IWillUnmount
    (will-unmount [_]
      (let [mode (om/get-state owner :mode)
            event-pub (om/get-shared owner :drag-pub)
            event-listener (om/get-state owner :event-listener)]
        (when (or (in-or-eq? mode :sortable)
                  (in-or-eq? mode :droppable))
          (log "should be unmounting")
          (unsubscribe-from-drags event-pub event-listener))))
    om/IDidMount
    (did-mount [_]
      (let [node (om/get-node owner "draggables")]
        ;; potentially optimize this with :location
        (om/set-state! owner :metrics (children-metrics node))
        (when-not (om/get-state owner :location)
          (om/set-state! owner :location
            (element-offset node)))))
    om/IRenderState
    (render-state [_ state]
      (apply dom/div #js {:className "draggable-container"
                          :ref "draggables"}
        (map
         (fn [index]
           (if-not (= index ::spacer)
             (om/build draggable (nth cursor index) {:key (:key-field state)
                                                     :init-state {:view (:view state)}})
             (sortable-spacer 25)))
         (:sort state))))))


(defn item-view [cursor owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil (cursor "displayvalue")))))

(def drag-chan (chan))
(def drag-pub
  (pub drag-chan (fn [e] (:event e))))

;(def drag-listen-chan (chan))
;(sub drag-pub :drag-start drag-listen-chan)
;(sub drag-pub :drag-stop drag-listen-chan)
;(sub drag-pub :drag drag-listen-chan)

(defn take-and-print [channel prefix]
  (go-loop []
           (log (str prefix ": " (<! channel)))
           (recur)))

;(take-and-print drag-listen-chan "drag-listen-chan")


(def itemlist [{"key" 1 "keycolumn" "somekeycolumn" "displayvalue" "Outgoing" "position" 0}
               {"key" 2 "keycolumn" "somekeycolumn" "displayvalue" "Reserved" "position" 1}
               {"key" 3 "keycolumn" "somekeycolumn" "displayvalue" "Easygoing" "position" 2}])

(defonce app-state (atom {:items itemlist}))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IRender
        (render [_]
          (dom/div nil
                   (dom/h4 nil "Contents")
                   (om/build draggables (:items app)
                             {:init-state {:view item-view
                                           :key-field "key"
                                           :mode [:sortable :droppable]}})))))
    app-state
    {:target (. js/document (getElementById "app"))
     :shared {:drag-chan drag-chan
              :drag-pub drag-pub}}))
