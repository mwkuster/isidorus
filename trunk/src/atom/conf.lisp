(in-package :atom)
(defsite psi.egovpt.org
  ;;(base-url "http://london.ztt.fh-worms.de:8000") ;the base-url
  ;;sbould be automatically generated from the hostname hunchentoot is
  ;;running at
  (author "Isidor")
  (title "Topicmaps on psi.egovpt.org")
  (relative-path "feeds")


  (collection-feed 
   (id "http://psi.egovpt.org/tm/egov-ontology")
   (title "eGov Reference Ontology")
   (relative-path "egov-ontology")
   (author "Isidor")
   (source-locator-prefix "http://psi.egovpt.org/tm/egov-ontology/")
    
   (snapshots-feed
    (id "http://psi.egovpt.org/tm/egov-ontology/snapshots")
    (title "Snapshots of the Ontology data")
    (relative-path "snapshots"))
   
   (fragments-feed
    (id "http://psi.egovpt.org/tm/egov-ontology/fragments")
    (title "A list of all change fragments for the eGov ontology")
    (relative-path "fragments")))
      
  (collection-feed 
   (id "http://psi.egovpt.org/tm/worms")
   (title "Data behind the portal of the city of Worms")
   (relative-path "worms")
   (author "Isidor")
   (source-locator-prefix "http://psi.egovpt.org/tm/worms/")
   (depends-on "http://london.ztt.fh-worms.de:8000/feeds/egov-ontology")
   
   (snapshots-feed 
    (id "http://psi.egovpt.org/tm/worms/snapshots")
    (title "Snapshots of the Worms data")
    (relative-path "snapshots"))
   
   (fragments-feed
    (id "http://psi.egovpt.org/tm/worms/fragments")
    (title "A list of all change fragments for the Worms data")
    (relative-path "fragments")))
  
  )

  

  
     
    
