
(ns jagsrpg.main
  (:use jagsrpg.model)
  (:use jagsrpg.gui))

(gen-class
 :name jagsrpg.CharacterBuilder
 :main true)

(defn -main [& args]
  (show-frame))
    

