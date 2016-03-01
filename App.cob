        IDENTIFICATION DIVISION.
         PROGRAM-ID. projets.

         ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

         SELECT futilisateurs ASSIGN TO "futilisateurs.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
<<<<<<< HEAD
         RECORD KEY fu_num_utilisateurs
         FILE STATUS IS fusers_stat.
=======
         RECORD KEY fu_num
         FILE STATUS IS fu_stat.
>>>>>>> origin/master


         SELECT fstades ASSIGN TO "fstades.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
<<<<<<< HEAD
         RECORD KEY fs_num_stade
         FILE STATUS IS fstad_stat.
=======
         RECORD KEY fs_num
         FILE STATUS IS fs_stat.
>>>>>>> origin/master

         SELECT fplaces ASSIGN TO "fplaces.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fp_clef
<<<<<<< HEAD
         ALTERNATE RECORD KEY fs_num_stade
         FILE STATUS IS fplac_stat.
=======
         ALTERNATE RECORD KEY fp_num_stade
         FILE STATUS IS fp_stat.
>>>>>>> origin/master

         SELECT fevenements ASSIGN TO "fevements.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
<<<<<<< HEAD
         RECORD KEY fe_num_event
         ALTERNATE RECORD KEY fs_num_stade
         FILE STATUS IS fevmnt_stat.
=======
         RECORD KEY fe_num
         ALTERNATE RECORD KEY fe_num_stade
         FILE STATUS IS fe_stat.
>>>>>>> origin/master

         SELECT freservations ASSIGN TO "freserv.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fr_clef
         ALTERNATE RECORD KEY fr_clef_place
         ALTERNATE RECORD KEY fr_num_event
         ALTERNATE RECORD KEY fr_num_utilisateur
<<<<<<< HEAD
         FILE STATUS IS freserva_stat.
=======
         FILE STATUS IS fr_stat.
>>>>>>> origin/master


         DATA DIVISION.
         FILE SECTION.

         FD futilisateurs.
         01 userTampon.
<<<<<<< HEAD
          02 fu_num PIC X(6).
          02 fu_nom PIC X(6).
          02 fu_prenom PIC X(6).
          02 fu_mdp PIC A(20).
          02 fu_ville PIC A(20).
          02 fu_question PIC 9(2).
          02 fu_reponse PIC 9(2).

          FD fstades.
          01 stadeTampon.
            02 fs_num PIC X(6).
            02 fs_nom PIC A(20).
            02 fs_adresse PIC A(20).
            02 fs_nb_place PIC 9(2).

          FD fplaces.
          01 placeTampon.
            02 fp_clef
             03 fp_num
             03 fp_tribune
             03 fp_rangee
            02 fp_num PIC 9(12).
            02 fp_tribune PIC A(20).
            02 fp_rangee PIC A(20).
            02 fp_num_stade PIC 9(2).
            02 fp_categorie PIC 9(2).

         FD fevenements.
         01 evtsTampon.
          02 fe_num PIC X(6).
          02 fe_nom PIC A(20).
          02 fe_date PIC A(20).
          02 fe_heure PIC 9(2).
          02 fe_num_stade PIC 9(2).
          02 fe_prix_base PIC 9(2).

         FD freservations.
         01 reservTampon.
          02 fr_clef
           03 fr_num_utilisateur
           03 fr_num_event
           03 fr_num_place
           03 fr_tribunes
           03 fr_rangee
          02 fr_clef_place
           03 fr_num_place
           03 fr_tribunes
           03 fr_rangee
          02 fr_num_utilisateur PIC X(6).
          02 fr_num_event PIC A(20).
          02 fr_num_place PIC A(20).
          02 fr_tribunes PIC 9(2).
          02 fr_rangee PIC 9(2).
          02 fr_age PIC 9(2).
          02 fr_prix PIC 9(2).


         WORKING-STORAGE SECTION.


         PROCEDURE DIVISION.

         STOP RUN.
=======
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
            02 fp_categorie PIC A(20).

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
           03 fr_num_place PIC 9(2).
           03 fr_tribunes PIC 9(2).
           03 fr_rangee PIC 9(2).
          02 fr_clef_place.
           03 fr_num_place PIC 9(2).
           03 fr_tribunes PIC 9(2).
           03 fr_rangee PIC 9(2).
          02 fr_age PIC 9(3).
          02 fr_prix PIC 9(3).


         WORKING-STORAGE SECTION.
      *>Variable Tampon
         77 fu_stat PIC 9(2).
         77 fs_stat PIC 9(2).
         77 fp_stat PIC 9(2).
         77 fe_stat PIC 9(2).
         77 fr_stat PIC 9(2).

      *>Variable globales  
         77 WswitchMenu PIC 9(2).
         77 Wrep PIC 9.
         77 Wfin PIC 9. 
         77 WalreadyExists PIC 9.
         77 WnotalreadyExists PIC 9.
         77 Wtemporaire PIC 9(2).

      *>Variable locale à AJOUT_PLACE
         77 Wnb_PlaceStade PIC 9(3).
         77 Wlock PIC 9.

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

      *>Menu temporaire pour réaliser le jeu de test
         PERFORM WITH TEST AFTER UNTIL WswitchMenu < 1
          DISPLAY "-------------------------------------------"
          DISPLAY "|              MENU TEMPORAIRE             |"
          DISPLAY "|                                          |"
          DISPLAY "|  1  -  Ajouter Stades                    |"
      DISPLAY "|  1a -  Modifier Stades                    |"
      DISPLAY "|  1b -  Supprimer Stades                   |"
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
           WHEN 3 PERFORM AJOUT_USER
           WHEN 31 PERFORM MODIFIER_USER
           WHEN 32 PERFORM SUPPRIMER_USER
           WHEN 4 PERFORM AJOUT_EVENT
           WHEN 5 PERFORM AJOUT_RESA
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
              NOT INVALID KEY DISPLAY 'Modifications correctement enregistrées' 
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
         OPEN I-O fplaces 
         OPEN INPUT fstades
         PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY "Donnez les informations de la place : "
          MOVE 0 TO WalreadyExists
          PERFORM WITH TEST AFTER UNTIL WalreadyExists = 1
           DISPLAY "Numéro stade : "
           ACCEPT fs_num
           READ fstades
            INVALID KEY
      *>Le numéro du stade n'existe pas
             DISPLAY "Stade inéxistant. "
            NOT INVALID KEY
      *>Le stade existe
             MOVE 1 TO WalreadyExists
      *>Récupération du nombre de places totales pour ce stade
             MOVE 0 TO Wnb_PlaceStade
             START fplaces,
              KEY = fp_num_stade
               INVALID KEY 
      *>Il n'existe pas de place pour ce stade
                MOVE 0 TO Wnb_PlaceStade
               NOT INVALID KEY
      *>Il existe des places, alors on les comptes
                MOVE 1 TO Wfin
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                 READ fplaces NEXT RECORD
                  AT END
                   MOVE 1 TO Wfin                
                  NOT AT END
                   COMPUTE Wnb_PlaceStade = Wnb_PlaceStade + 1
                 END-READ
                END-PERFORM
             END-START
             MOVE 0 TO Wlock
             PERFORM WITH TEST AFTER UNTIL Wrep = 0 AND Wlock = 1
      *>On vérifie que le nombre de place et ajoute si OK
              IF Wnb_PlaceStade < fs_nb_place
      *>Renseignement des valeurs
               MOVE fs_num TO fp_num_stade
      *>Vérification que la place n'existe pas déjà dans ce stade
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
               READ fplaces
                INVALID KEY MOVE 0 TO WalreadyExists
                NOT INVALID KEY DISPLAY "La place existe déjà"
              END-PERFORM                

               PERFORM WITH TEST AFTER UNTIL fp_categorie NOT EQUAL " "
                DISPLAY 'Catégorie de la place : '
                ACCEPT fp_categorie
               END-PERFORM

               WRITE placeTampon 
                INVALID KEY 
                 DISPLAY 'Problème enregistrement'
                NOT INVALID KEY 
                 DISPLAY 'Enregistrement inséré' 
                 COMPUTE Wnb_PlaceStade = Wnb_PlaceStade + 1             
              ELSE
               DISPLAY 'La capacité du stade à été atteinte.'
               MOVE 1 TO Wlock
              END-IF
              PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1 
              AND Wlock = 1
               DISPLAY 'ici'
               DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
               ACCEPT Wrep
              END-PERFORM
             END-PERFORM            
         END-READ
        END-PERFORM

         PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1 
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
        END-PERFORM
        CLOSE fplaces  
        CLOSE fstades      
        .

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
        CLOSE futilisateurs
        .       






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

             REWRITE stadeTampon 
              INVALID KEY DISPLAY 'Problème enregistrement modifications'
              NOT INVALID KEY DISPLAY 'Modifications correctement enregistrées' 
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
              NOT INVALID KEY DISPLAY 'Utilisateur correctement supprimé' 
             END-DELETE
            END-READ
        
           PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
          DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
          ACCEPT Wrep
         END-PERFORM
         
         END-PERFORM
         CLOSE futilisateurs.
         
         
         
         
         
         
         AJOUT_EVENT.
         .

         AJOUT_RESA.
         .


       
>>>>>>> origin/master

