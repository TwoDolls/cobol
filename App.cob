        IDENTIFICATION DIVISION.
         PROGRAM-ID. projets.

         ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

         SELECT futilisateurs ASSIGN TO "futilisateurs.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fu_num
         FILE STATUS IS fu_stat.

         SELECT fstades ASSIGN TO "fstades.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fs_num
         FILE STATUS IS fs_stat.

         SELECT fplaces ASSIGN TO "fplaces.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fp_clef
         ALTERNATE RECORD KEY fp_num_stade WITH DUPLICATES
         FILE STATUS IS fp_stat.

         SELECT fevenements ASSIGN TO "fevements.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fe_num
         ALTERNATE RECORD KEY fe_num_stade
         FILE STATUS IS fe_stat.

         SELECT freservations ASSIGN TO "freserv.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fr_clef
         ALTERNATE RECORD KEY fr_clef_place WITH DUPLICATES
         ALTERNATE RECORD KEY fr_num_event WITH DUPLICATES
         ALTERNATE RECORD KEY fr_num_utilisateur WITH DUPLICATES
         FILE STATUS IS fr_stat.

         DATA DIVISION.

         FILE SECTION.

         FD futilisateurs.
         01 userTampon.
          02 fu_num PIC 9(6).
          02 fu_nom PIC A(30).
          02 fu_prenom PIC A(20).
          02 fu_mdp PIC X(15).
          02 fu_ville PIC A(30).
          02 fu_question PIC A(100).
          02 fu_reponse PIC A(30).

          FD fstades.
          01 stadeTampon.
            02 fs_num PIC 9(6).
            02 fs_nom PIC A(30).
            02 fs_adresse PIC A(50).
            02 fs_nb_place PIC 9(3).

          FD fplaces.
          01 placeTampon.
            02 fp_clef.
             03 fp_num PIC 9(2).
             03 fp_tribune PIC 9(2).
             03 fp_rangee PIC 9(2).
             03 fp_num_stade PIC 9(6).
             03 fp_categorie PIC A(20).

         FD fevenements.
         01 evtsTampon.
          02 fe_num PIC 9(6).
          02 fe_nom PIC X(30).
          02 fe_date.
           03 fe_jour PIC 9(2).
           03 fe_mois PIC 9(2).
           03 fe_annee PIC 9(4).
          02 fe_heure PIC 9(2).
          02 fe_num_stade PIC 9(6).
          02 fe_prix_base PIC 9(3).

         FD freservations.
         01 reservTampon.
          02 fr_clef.
           03 fr_num_utilisateur PIC 9(6).
           03 fr_num_event PIC 9(6).
           03 fr_clef_place.
            04 fr_num_place PIC 9(2).
            04 fr_tribune PIC 9(2).
            04 fr_rangee PIC 9(2).
            04 fr_categorie PIC A(20).
          02 fr_age PIC 9(3).
          02 fr_prix PIC 9(3)V9(2).


         WORKING-STORAGE SECTION.
      *>Variable Tampon
         77 fu_stat PIC 9(2).
         77 fs_stat PIC 9(2).
         77 fp_stat PIC 9(2).
         77 fe_stat PIC 9(2).
         77 fr_stat PIC 9(2).

      *>Variable globales  
         77 Wmenuchoixuser PIC 9(2).
         77 WswitchMenu PIC 9(2).
         77 Wrep PIC 9.
         77 Wfin PIC 9. 
         77 WalreadyExists PIC 9.
         77 WalreadyExists2 PIC 9.
         77 WnotalreadyExists PIC 9.
         77 Wtemporaire PIC 9(2).
         77 Wswitch PIC 9(2).

      *>Variable locale à AJOUT_PLACE
         77 Wnb_PlaceStade PIC 9(3).
         77 Wlock PIC 9.
      *>Variable locale à AJOUT_RESERVATION
         77 Wnbplace PIC 9(2).
         77 nb_place PIC 9(2).
         77 Wfin2 PIC 9.
         77 Wok PIC 9.
         77 num_categ PIC A(20).
         77 Wtrouve PIC 9.
         77 nb_place_dispo PIC 9.
         77 TARIFSUPL PIC 9(3)V9(2).
         77 prix_base PIC 9(3)V9(2).
         77 prix_tot PIC 9(3)V9(2).

      
     
         PROCEDURE DIVISION.
      *>Main
      *>Vérification de la présence des fichiers
         OPEN I-O futilisateurs
         IF fu_stat =35 THEN
          OPEN OUTPUT futilisateurs
          CLOSE futilisateurs
         ELSE 
          CLOSE futilisateurs
         END-IF

         OPEN I-O fstades
         IF fs_stat =35 THEN
          OPEN OUTPUT fstades
          CLOSE fstades
         ELSE 
          CLOSE fstades
         END-IF

         OPEN I-O fplaces
         IF fp_stat =35 THEN
          OPEN OUTPUT fplaces
          CLOSE fplaces
         ELSE 
          CLOSE fplaces
         END-IF

         OPEN I-O fevenements
         IF fe_stat =35 THEN
          OPEN OUTPUT fevenements
          CLOSE fevenements
         ELSE 
          CLOSE fevenements
         END-IF

         OPEN I-O freservations
         IF fr_stat =35 THEN
          OPEN OUTPUT freservations
          CLOSE freservations
         ELSE 
          CLOSE freservations
         END-IF



      *>Menu choix catégorie utilisateur
PERFORM WITH TEST AFTER UNTIL Wmenuchoixuser <1
       DISPLAY "-------------------------------------------"
          DISPLAY "|    MENU CHOIX CATEGORIE UTILISATEUR      |"
          DISPLAY "|                                          |"
          DISPLAY "|  1  -  Menu temp                         |"
        DISPLAY "|  2  -  Administrateur                    |"
        DISPLAY "|  3  -  Clients                           |"
          DISPLAY "-------------------------------------------"
          DISPLAY " Choix ? "
          
          
          ACCEPT Wmenuchoixuser
       
          EVALUATE Wmenuchoixuser
           WHEN 1  
           
      *>Menu temporaire pour réaliser le jeu de test
                 PERFORM WITH TEST AFTER UNTIL WswitchMenu < 1
                  DISPLAY "-------------------------------------------"
                  DISPLAY "|              MENU TEMPORAIRE             |"
                  DISPLAY "|                                          |"
                  DISPLAY "|  1  -  Ajouter Stades                    |"
              DISPLAY "|  1a -  Modifier Stades                   |"
              DISPLAY "|  1b -  Supprimer Stades                  |"
                  DISPLAY "|  2  -  Ajouter Places                    |"
              DISPLAY "|  2a -  Modifier Places                   |"
              DISPLAY "|  2b -  Supprimer Places                  |"
                  DISPLAY "|  3  -  Ajouter Utilisateurs              |"
              DISPLAY "|  3a -  Modifier Utilisateurs             |"
              DISPLAY "|  3b -  Supprimer Utilisateurs            |"
                  DISPLAY "|  4  -  Ajouter Evenements                |"
              DISPLAY "|  4a -  Modifier Evenements               |"
              DISPLAY "|  4b -  Supprimer Evenements              |"
                  DISPLAY "|  5  -  Ajouter Reservations              |"
              DISPLAY "|  5a -  Modifier Reservations             |"
              DISPLAY "|  5b -  Supprimer Reservations            |"
                  DISPLAY "|  0  -  Quitter                           |"
                  DISPLAY "-------------------------------------------"
                  DISPLAY " Choix ? "
        
                  ACCEPT WswitchMenu
               
                  EVALUATE WswitchMenu
                   WHEN 1 PERFORM AJOUT_STADE
                   WHEN 11 PERFORM MODIFIER_STADE
                   WHEN 12 PERFORM SUPPRIMER_STADE
                   WHEN 2 PERFORM AJOUT_PLACE
                   WHEN 21 PERFORM SUPPRIMER_PLACE
                   WHEN 3 PERFORM AJOUT_USER
                   WHEN 31 PERFORM MODIFIER_USER
                   WHEN 32 PERFORM SUPPRIMER_USER
                   WHEN 4 PERFORM AJOUT_EVENT
                   WHEN 41 PERFORM MODIFIER_EVENT
                   WHEN 42 PERFORM SUPPRIMER_EVENT
                   WHEN 5 PERFORM AJOUT_RESERVATION
                   WHEN 52 PERFORM SUPPRIMER_RESERVATION
        
                  END-EVALUATE
                 END-PERFORM
            
         END-EVALUATE
         END-PERFORM 
         STOP RUN.


  


         AJOUT_STADE.
         OPEN I-O fstades
         PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY "Donnez les informations du stade : "
          MOVE 1 TO WalreadyExists
          PERFORM WITH TEST AFTER UNTIL WalreadyExists = 0
           DISPLAY "Numéro stade : "
           ACCEPT fs_num
           READ fstades
      *>Le code utilisateur n'existe pas alors on peut l'ajouer
            INVALID KEY
             MOVE 0 TO WalreadyExists
      *>Renseignement des valeurs
             PERFORM WITH TEST AFTER UNTIL fs_nom NOT EQUAL " "
              DISPLAY 'Nom : '
              ACCEPT fs_nom
             END-PERFORM   

             PERFORM WITH TEST AFTER UNTIL fs_adresse NOT EQUAL " "
              DISPLAY 'Adresse : '
              ACCEPT fs_adresse
             END-PERFORM   

             PERFORM WITH TEST AFTER UNTIL fs_nb_place > 0
              DISPLAY 'Nombre de places : '
              ACCEPT fs_nb_place
             END-PERFORM  

             WRITE stadeTampon 
              INVALID KEY DISPLAY 'Problème enregistrement'
              NOT INVALID KEY DISPLAY 'Enregistrement inséré'           

            NOT INVALID KEY
      *>Le numéro du stade existe déjà
           DISPLAY 'Numéro stade déjà utilisé.'
         END-READ
        END-PERFORM

         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
        END-PERFORM
        CLOSE fstades.


        MODIFIER_STADE.
        OPEN I-O fstades
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
           DISPLAY "Numéro du stade à modifier : "
           ACCEPT fs_num
           read  fstades
           INVALID KEY
      *>Le numéro du stade n'existe pas
             DISPLAY "Stade inéxistant. "
            
           NOT INVALID KEY
      *>Le stade existe
      *>Modif des valeurs
             PERFORM WITH TEST AFTER UNTIL fs_nom NOT EQUAL " "
              DISPLAY "Nouveau nom du stade : "
              ACCEPT fs_nom
             END-PERFORM   

             PERFORM WITH TEST AFTER UNTIL fs_adresse NOT EQUAL " "
              DISPLAY "Nouvelle adresse du stade : "
              ACCEPT fs_adresse
             END-PERFORM   

             PERFORM WITH TEST AFTER UNTIL fs_nb_place > 0
              DISPLAY "Nouveau nombre de place du stade : "
              ACCEPT fs_nb_place
             END-PERFORM  

             REWRITE stadeTampon 
              INVALID KEY DISPLAY 'Problème enregistrement modifications'
              NOT INVALID KEY 
               DISPLAY 'Modifications correctement enregistrées' 
             END-REWRITE
            END-READ
            
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         
        
        END-PERFORM
         END-PERFORM
        
        CLOSE fstades.
        
        SUPPRIMER_STADE.
        OPEN I-O fstades
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
           DISPLAY "Numéro du stade à supprimer : "
           ACCEPT fs_num
           READ  fstades
           INVALID KEY
      *>Le numéro du stade n'existe pas
             DISPLAY "Stade inéxistant. "
             
           NOT INVALID KEY
      *>Le stade existe
           
           DELETE fstades 
              INVALID KEY DISPLAY 'Problème lors de la suppression'
              NOT INVALID KEY DISPLAY 'Stade correctement supprimé' 
             END-DELETE
            END-READ
        
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
        
         END-PERFORM
         END-PERFORM
         CLOSE fstades.



                 AJOUT_PLACE.
         PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY "Donnez les informations de la place : "

          MOVE 0 TO WalreadyExists
          PERFORM WITH TEST AFTER UNTIL WalreadyExists = 1

      *>Vérification de l'existence du stade
      *>Récupération numéro du stade
           DISPLAY "Numéro stade : "
           ACCEPT fs_num

           OPEN INPUT fstades
      *>Tentative de lecture
           READ fstades
            INVALID KEY
      *>Le numéro du stade n'existe pas
             DISPLAY "Stade inéxistant. "
            NOT INVALID KEY
      *>Le stade existe alors Récupération du nombre de places totales pour ce stade
             MOVE 1 TO WalreadyExists
             MOVE 0 TO Wnb_PlaceStade
             MOVE fs_num TO fp_num_stade
      *>On ouvre le fichier fplaces et exécute une recherche sur zone par numero de stade
             OPEN I-O fplaces 
             START fplaces,
              KEY = fp_num_stade
               INVALID KEY 
      *>Il n'existe pas de place pour ce stade
                MOVE 0 TO Wnb_PlaceStade
               NOT INVALID KEY
      *>Il existe des places, alors on les comptes
                MOVE 0 TO Wfin
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                 READ fplaces NEXT RECORD
                  AT END
                   MOVE 1 TO Wfin                
                  NOT AT END
                   COMPUTE Wnb_PlaceStade = Wnb_PlaceStade + 1
                 END-READ
                END-PERFORM
             END-START

      *>Tant qu'il reste de la place dans le stade et que l'opérateur veut continuer
             MOVE 0 TO Wlock
             MOVE 1 TO Wrep
             PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wlock = 1

      *>On vérifie que l'on peut ajouter des places
              IF Wnb_PlaceStade < fs_nb_place

      *>Renseignement des valeurs
               DISPLAY Wnb_PlaceStade, " < ",  fs_nb_place
      *>Tant que la place n'existe pas
               MOVE 1 TO WalreadyExists
               PERFORM WITH TEST AFTER UNTIL WalreadyExists = 0

               PERFORM WITH TEST AFTER UNTIL fp_num > 0
                DISPLAY 'Numéro de la place : '
                ACCEPT fp_num
               END-PERFORM   

               PERFORM WITH TEST AFTER UNTIL fp_tribune > 0
                DISPLAY 'Numéro de la tribune  : '
                ACCEPT fp_tribune
               END-PERFORM   

               PERFORM WITH TEST AFTER UNTIL fp_rangee > 0
                DISPLAY 'Numéro de la rangée : '
                ACCEPT fp_rangee
               END-PERFORM 

               PERFORM WITH TEST AFTER UNTIL fp_categorie NOT EQUAL " "
                DISPLAY 'Catégorie de la place : '
                ACCEPT fp_categorie
               END-PERFORM

      *>Vérification que la place n'existe pas déjà dans ce stade
                READ fplaces
                 INVALID KEY MOVE 0 TO WalreadyExists
                 NOT INVALID KEY DISPLAY "La place existe déjà"
                END-READ
               END-PERFORM 
               
               DISPLAY placeTampon 

      *>Ecriture dans le fichier
               WRITE placeTampon 
                INVALID KEY 
                 DISPLAY 'Problème enregistrement'
                 DISPLAY fp_stat
                NOT INVALID KEY 
                 DISPLAY 'Enregistrement inséré' 
                 COMPUTE Wnb_PlaceStade = Wnb_PlaceStade + 1
              
      *>On demande si l'utilisateur veut ajouter d'autre place pour ce stade 
               PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1 
                DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
                ACCEPT Wrep
               END-PERFORM


      *>Si le nb de place dans le stade est supérieur à sa capacité alors on affiche un message et vérouille l'ajout           
              ELSE
               DISPLAY 'La capacité du stade à été atteinte.'
               MOVE 1 TO Wlock
              END-IF
             END-PERFORM  
             CLOSE fplaces
           END-READ
          END-PERFORM

          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1 
           DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
           ACCEPT Wrep
          END-PERFORM
         END-PERFORM 
         CLOSE fstades      
         .


        
        
        
         SUPPRIMER_PLACE.
        OPEN I-O fplaces
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
           DISPLAY "Numéro de la place à supprimer : "
           ACCEPT fp_num
           READ  fplaces
           INVALID KEY
      *>Le numéro du stade n'existe pas
             DISPLAY "Place inéxistant. "
             
           NOT INVALID KEY
      *>Le stade existe
           
           DELETE fplaces 
              INVALID KEY DISPLAY 'Problème lors de la suppression'
              NOT INVALID KEY DISPLAY 'Place correctement supprimé' 
             END-DELETE
            END-READ
        
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
        
         END-PERFORM
         END-PERFORM
         CLOSE fplaces.
         
         
         
         AJOUT_USER.
      *>Ouverture en écriture/lecture pour vérification + MAJ
         OPEN I-O futilisateurs
      *>Boucle permettant la multi-insertion
         PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY "Donnez les informations de l'utilisateurs : "
          MOVE 1 TO WalreadyExists
          PERFORM WITH TEST AFTER UNTIL WalreadyExists = 0
           DISPLAY "Numéro client : "
           ACCEPT fu_num
           READ futilisateurs
      *>Le code utilisateur n'existe pas alors on peut l'ajouer
            INVALID KEY
             MOVE 0 TO WalreadyExists
      *>Renseignement des valeurs
             PERFORM WITH TEST AFTER UNTIL fu_nom NOT EQUAL " "
              DISPLAY 'Nom : '
              ACCEPT fu_nom
             END-PERFORM 

             PERFORM WITH TEST AFTER UNTIL fu_prenom NOT EQUAL " "
              DISPLAY 'Prénom : '
              ACCEPT fu_prenom
             END-PERFORM  

             PERFORM WITH TEST AFTER UNTIL fu_mdp NOT EQUAL " "
              DISPLAY 'Mot de passe : '
              ACCEPT fu_mdp
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fu_ville NOT EQUAL " "
              DISPLAY 'Ville : '
              ACCEPT fu_ville
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fu_question NOT EQUAL " "
              DISPLAY 'Question secrète : '
              ACCEPT fu_question
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fu_reponse NOT EQUAL " "
              DISPLAY 'Réponse secrète : '
              ACCEPT fu_reponse
             END-PERFORM

             WRITE userTampon 
              INVALID KEY DISPLAY 'Problème enregistrement'
              NOT INVALID KEY DISPLAY 'Enregistrement inséré'           

            NOT INVALID KEY
      *>Le numéro utilisateur existe déjà
           DISPLAY 'Numéro utilisateur déjà utilisé.'
         END-READ
        END-PERFORM

         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
        END-PERFORM
        CLOSE futilisateurs.



         MODIFIER_USER.
        OPEN I-O futilisateurs
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
          DISPLAY "Numéro client à modifier : "
           ACCEPT fu_num
           READ futilisateurs
           INVALID KEY
      *>Le numéro du stade n'existe pas
            DISPLAY "Utilisateur inéxistant. "
             
           NOT INVALID KEY
      *>Le stade existe
      *>Modif des valeurs
             PERFORM WITH TEST AFTER UNTIL fu_nom NOT EQUAL " "
              DISPLAY 'Nom : '
              ACCEPT fu_nom
             END-PERFORM 

             PERFORM WITH TEST AFTER UNTIL fu_prenom NOT EQUAL " "
              DISPLAY 'Prénom : '
              ACCEPT fu_prenom
             END-PERFORM  

             PERFORM WITH TEST AFTER UNTIL fu_mdp NOT EQUAL " "
              DISPLAY 'Mot de passe : '
              ACCEPT fu_mdp
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fu_ville NOT EQUAL " "
              DISPLAY 'Ville : '
              ACCEPT fu_ville
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fu_question NOT EQUAL " "
              DISPLAY 'Question secrète : '
              ACCEPT fu_question
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fu_reponse NOT EQUAL " "
              DISPLAY 'Réponse secrète : '
              ACCEPT fu_reponse
             END-PERFORM  

             REWRITE userTampon 
              INVALID KEY DISPLAY 'Problème enregistrement modifications'
              NOT INVALID KEY 
               DISPLAY 'Modifications correctement enregistrées' 
             END-REWRITE
            END-READ
            
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
    
        END-PERFORM
        CLOSE futilisateurs.
        
        SUPPRIMER_USER.
        OPEN I-O futilisateurs
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
           DISPLAY "Numéro de l'utilisateur à supprimer : "
           ACCEPT fu_num
           READ  futilisateurs
           INVALID KEY
      *>Le numéro du stade n'existe pas
            DISPLAY "Utilisateur inéxistant. "
             
           NOT INVALID KEY
      *>Le stade existe
           
           DELETE futilisateurs 
              INVALID KEY DISPLAY 'Problème lors de la suppression'
              NOT INVALID KEY 
               DISPLAY 'Utilisateur correctement supprimé' 
             END-DELETE
            END-READ
        
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
         
         END-PERFORM
         CLOSE futilisateurs.


         

         AJOUT_EVENT.
      *>Ouverture en écriture/lecture pour vérification + INSERTION
         OPEN I-O fevenements

      *>Boucle permettant la multi-insertion
         PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY "Donnez les informations de l'évènement : "
          MOVE 1 TO WalreadyExists

      *>Vérification que le code évènement ne soit pas déjà utilisé
          PERFORM WITH TEST AFTER UNTIL WalreadyExists = 0
           DISPLAY "Numéro évènement : "
           ACCEPT fe_num
           READ fevenements
      *>Le code évènement n'existe pas alors on peut l'ajouter
            INVALID KEY
             MOVE 0 TO WalreadyExists

      *>Vérification que le stade existe
             MOVE 1 TO WalreadyExists2
             PERFORM WITH TEST AFTER UNTIL WalreadyExists2 = 1
      *>Saisi du numéro du stade
              PERFORM WITH TEST AFTER UNTIL fe_num_stade NOT EQUAL " "
               DISPLAY 'Numéro du stade : '
               ACCEPT fs_num
              END-PERFORM 
      *>Ouverture et lecture directe de fstades
              OPEN INPUT fstades
              READ fstades
      *>Le numéro de stade n'existe pas     
               INVALID KEY
                MOVE 0 TO WalreadyExists2
                DISPLAY "Ce stade n'existe pas"
      *>Le stade existe alors on peut ajouter l'évènement
               NOT INVALID KEY
                MOVE 1 TO WalreadyExists2
      *>Renseignement des valeurs
                MOVE fs_num TO fe_num_stade

                PERFORM WITH TEST AFTER UNTIL fe_nom NOT EQUAL " "
                 DISPLAY 'Nom : '
                 ACCEPT fe_nom
                END-PERFORM 
 
                PERFORM WITH TEST AFTER UNTIL fe_jour > 0 
                AND fe_jour <=31
                 DISPLAY 'Jour : '
                 ACCEPT fe_jour
                END-PERFORM

                PERFORM WITH TEST AFTER UNTIL fe_mois > 0
                AND fe_mois <=12 
                 DISPLAY 'Mois : '
                 ACCEPT fe_mois
                END-PERFORM

                PERFORM WITH TEST AFTER UNTIL fe_annee >= 2016 
                AND fe_annee <= 2018
                 DISPLAY 'Annee : '
                 ACCEPT fe_annee
                END-PERFORM

                PERFORM WITH TEST AFTER UNTIL fe_heure < 24
                 DISPLAY 'Heure : '
                 ACCEPT fe_heure
                END-PERFORM
                
                PERFORM WITH TEST AFTER UNTIL fe_prix_base > 0
                 DISPLAY 'Prix : '
                 ACCEPT fe_prix_base
                END-PERFORM

      *>Ajout de l'évènement
                WRITE evtsTampon 
                 INVALID KEY DISPLAY 'Problème enregistrement'
                 NOT INVALID KEY DISPLAY 'Enregistrement inséré' 
                END-WRITE
              END-READ
             END-PERFORM
             CLOSE fstades

      *>Le numéro évènement existe déjà       
           NOT INVALID KEY
            DISPLAY 'Numéro évènement déjà utilisé.'
           END-READ
          END-PERFORM

          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
           DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
           ACCEPT Wrep
          END-PERFORM

         END-PERFORM
         CLOSE fevenements.
         .         
        
    
         
          MODIFIER_EVENT.
        OPEN I-O fevenements
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
          DISPLAY "Numéro de l'évènement à modifier : "
           ACCEPT fe_num
           READ fevenements
           INVALID KEY
      *>Le numéro du stade n'existe pas
            DISPLAY "Evènement inéxistant. "
             
           NOT INVALID KEY
      *>Le stade existe
      *>Modif des valeurs
             PERFORM WITH TEST AFTER UNTIL fe_nom NOT EQUAL " "
              DISPLAY 'Nom: '
              ACCEPT fe_nom
             END-PERFORM 

             PERFORM WITH TEST AFTER UNTIL fe_jour >0 AND fe_jour<=31
              DISPLAY 'Jour: : '
              ACCEPT fe_jour
             END-PERFORM  

             PERFORM WITH TEST AFTER UNTIL fe_mois>0 AND fe_mois<=12
              DISPLAY 'Mois : '
              ACCEPT fe_mois
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fe_annee >= 2016 
             AND fe_annee <= 2018
              DISPLAY 'Annee : '
              ACCEPT fe_annee
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fe_heure < 24
              DISPLAY 'Heure : '
              ACCEPT fe_heure
             END-PERFORM

             PERFORM WITH TEST AFTER UNTIL fe_num_stade NOT EQUAL " "
              DISPLAY 'Numéro du stade concerné : '
              ACCEPT fe_num_stade
             END-PERFORM  
             
             PERFORM WITH TEST AFTER UNTIL fe_prix_base NOT EQUAL " "
              DISPLAY 'Prix de base : '
              ACCEPT fe_prix_base
             END-PERFORM 

             REWRITE evtsTampon 
              INVALID KEY DISPLAY 'Problème enregistrement modifications'
       NOT INVALID KEY DISPLAY 'Modifications correctement enregistrées' 
             END-REWRITE
            END-READ
            
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
    
        END-PERFORM
        CLOSE fevenements.
        
        SUPPRIMER_EVENT.
        OPEN I-O fevenements
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
           DISPLAY "Numéro de l'évènement à supprimer : "
           ACCEPT fe_num
           READ  fevenements
           INVALID KEY
      *>Le numéro du stade n'existe pas
            DISPLAY "Evènement inéxistant. "
             
           NOT INVALID KEY
      *>Le stade existe
           
           DELETE fevenements 
              INVALID KEY DISPLAY 'Problème lors de la suppression'
              NOT INVALID KEY DISPLAY 'Evènement correctement supprimé' 
             END-DELETE
            END-READ
        
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
         
         END-PERFORM
         CLOSE fevenements.





         AJOUT_RESERVATION.
         OPEN I-O freservations
         PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
          MOVE 0 TO nb_place_dispo
          MOVE 0 TO WalreadyExists
          MOVE 0 TO WalreadyExists2
          MOVE 0 TO Wok
          PERFORM WITH TEST AFTER UNTIL WalreadyExists = 1

      *>Vérification de l'existence de l'utilisateur
      *>Récupération numéro de l'utilisateur
           DISPLAY "Numéro de l'utilisateur : "
           ACCEPT fu_num

           OPEN INPUT futilisateurs
      *>Tentative de lecture
           READ futilisateurs
            INVALID KEY
      *>Le numéro d'utilisateur n'existe pas
             DISPLAY "Utilisateur inéxistant. "
            NOT INVALID KEY
      *>L'utilisateur existe alors 
             MOVE 1 TO WalreadyExists
             
                PERFORM WITH TEST AFTER UNTIL WalreadyExists2 = 1
      *>Vérification de l'existence de l'évènement
      *>Récupération numéro de l'évènement
                   DISPLAY "Numéro de l'évènement : "
                   ACCEPT fe_num
        
                   OPEN INPUT fevenements
     *>Tentative de lecture
                   READ fevenements
                    INVALID KEY
     *>Le numéro d'évènement n'existe pas
                     DISPLAY "Evènement inéxistant. "
                    NOT INVALID KEY
      *>L'évènement existe alors 
                    
                     MOVE fe_prix_base TO prix_base
                     MOVE 1 TO WalreadyExists2
         PERFORM WITH TEST AFTER UNTIL nb_place > 0
                       DISPLAY "Nombre de place a réserver?"
                   ACCEPT nb_place
         END-PERFORM
         DISPLAY "Catégorie des places?"
                     ACCEPT num_categ
                     MOVE fe_num_stade TO fp_num_stade
                     OPEN INPUT fplaces
                     START fplaces,
                       KEY = fp_num_stade
                          INVALID KEY
                            DISPLAY "1"
                    NOT INVALID KEY
                            MOVE 0 TO Wfin
                            
                          PERFORM WITH TEST AFTER UNTIL Wfin = 1 
                            READ fplaces NEXT RECORD
                            AT END
                             MOVE 1 TO Wfin
                             DISPLAY "Pas assez de place disponible"
                            NOT AT END
                            
                             IF fp_categorie = num_categ THEN
                               MOVE fp_num TO fr_num_place
                               MOVE fp_tribune TO fr_tribune
                               MOVE fp_rangee TO fr_rangee
                               MOVE fp_categorie TO fr_categorie
                               
                               READ freservations
                                INVALID KEY
                               COMPUTE nb_place_dispo = nb_place_dispo + 1 
                            IF nb_place_dispo >= nb_place  THEN
                                    MOVE 1 TO Wok
                                   
                                    MOVE 1 TO Wfin
                                    
                                 END-IF
                                NOT INVALID KEY
                                 DISPLAY "."
                              
            
                              
                               END-READ
                              
                             
                            END-IF
                           
                            END-READ
                          END-PERFORM 
                         END-START
                      
            END-READ
                END-PERFORM
                END-READ
                 
                END-PERFORM
                IF num_categ = 1 THEN
                   MOVE 0 TO TARIFSUPL
                END-IF
                IF num_categ = 2 THEN
                   MOVE 25 TO TARIFSUPL
                END-IF
                IF num_categ = 3
                   MOVE 50 TO TARIFSUPL
                END-IF
                
                IF Wok = 1 THEN
                MOVE 0 TO prix_tot
                    START fplaces,
                       KEY = fp_num_stade
                          INVALID KEY
                            DISPLAY "1"
                        NOT INVALID KEY
                            MOVE 0 TO Wnbplace
                             
                            PERFORM WITH TEST AFTER UNTIL Wnbplace = nb_place
                            READ fplaces NEXT RECORD
                            
                            IF fp_categorie = num_categ THEN
                               MOVE fp_num TO fr_num_place
                               MOVE fp_tribune TO fr_tribune
                               MOVE fp_rangee TO fr_rangee
                               MOVE fp_categorie TO fr_categorie
                               MOVE fu_num TO fr_num_utilisateur
                               MOVE fe_num TO fr_num_event
                               
                               READ freservations
                                INVALID KEY
                                
                                  COMPUTE Wnbplace = Wnbplace + 1 
                                  DISPLAY "Place disponible:"
                                  Display fp_num
                                  Display "Age de la personne concernée? (Un justificatif pourra être demandé en entrant dans le stade)"
                                  Accept  fr_age
                                  If  fr_age < 18 THEN
                                  COMPUTE fr_prix = TARIFSUPL + prix_base
                                  COMPUTE fr_prix = fr_prix * 0.5
                                  Display fe_prix_base
                                  
                                  DISPLAY "PRIX:"
                                  DISPLAY fr_prix
                                  COMPUTE prix_tot = prix_tot + fr_prix 
                                  END-IF
                                  IF  fr_age > 59 THEN
                                  COMPUTE fr_prix = TARIFSUPL + prix_base 
                                  DISPLAY fr_prix
                                  COMPUTE fr_prix = 0.3 * fr_prix
                                  DISPLAY "PRIX:"
                                  DISPLAY fr_prix
                                  COMPUTE prix_tot = prix_tot + fr_prix
                                  END-IF
                                  IF fr_age > 18 AND fr_age < 60 THEN
                                  COMPUTE fr_prix = TARIFSUPL + prix_base 
                                  DISPLAY "PRIX:"
                                  DISPLAY fr_prix
                                  COMPUTE prix_tot = prix_tot + fr_prix
                                  END-IF
                                  WRITE reservTampon
                                  INVALID KEY DISPLAY 'Problème enregistrement'
                                  NOT INVALID KEY DISPLAY 'Enregistrement inséré' 
                                  END-WRITE
                                  
                           NOT INVALID KEY 
                              DISPLAY "."
                               END-READ
                               
                             
                            
                            
                            END-IF
                                  
                            END-PERFORM 
                      END-START
                      DISPLAY "Prix total de votre réservation:"
                      DISPLAY prix_tot 
                    END-IF
                    CLOSE fplaces         
                    CLOSE freservations 
                    CLOSE futilisateurs
                    CLOSE fevenements
                    
                   

                   
                
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1 
           DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
           ACCEPT Wrep
          END-PERFORM 
         END-PERFORM
         .
         
         
         
        SUPPRIMER_RESERVATION.
        OPEN I-O freservations
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          
           DISPLAY "Numéro de l'utilisateur : "
           ACCEPT fr_num_utilisateur
           DISPLAY "Numéro de l'évènement : "
           ACCEPT fr_num_event
           DISPLAY "Numéro de la place : "
           ACCEPT fr_num_place
         
           READ  freservations
           INVALID KEY
      *>Le numéro du stade n'existe pas
            DISPLAY "Réservation inéxistante. "
             
           NOT INVALID KEY
      *>Le stade existe
           
           DELETE freservations 
              INVALID KEY DISPLAY 'Problème lors de la suppression'
              NOT INVALID KEY DISPLAY 'Réservation correctement supprimé' 
             END-DELETE
            END-READ
        
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
         
         END-PERFORM
         CLOSE fevenements.
         
         
         
         
       
         
         
      


        
      





