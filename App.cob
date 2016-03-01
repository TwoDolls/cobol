        IDENTIFICATION DIVISION.
         PROGRAM-ID. projets.

         ENVIRONMENT DIVISION.
         INPUT-OUTPUT SECTION.
         FILE-CONTROL.

         SELECT futilisateurs ASSIGN TO "futilisateurs.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fu_num_utilisateurs
         FILE STATUS IS fusers_stat.

         SELECT fstades ASSIGN TO "fstades.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fs_num_stade
         FILE STATUS IS fstad_stat.

         SELECT fplaces ASSIGN TO "fplaces.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fp_clef
         ALTERNATE RECORD KEY fs_num_stade
         FILE STATUS IS fplac_stat.

         SELECT fevenements ASSIGN TO "fevements.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fe_num_event
         ALTERNATE RECORD KEY fs_num_stade
         FILE STATUS IS fevmnt_stat.

         SELECT freservations ASSIGN TO "freserv.dat"
         ORGANIZATION indexed
         ACCESS IS dynamic
         RECORD KEY fr_clef
         ALTERNATE RECORD KEY fr_clef_place
         ALTERNATE RECORD KEY fr_num_event
         ALTERNATE RECORD KEY fr_num_utilisateur
         FILE STATUS IS freserva_stat.


         DATA DIVISION.
         FILE SECTION.

         FD futilisateurs.
         01 userTampon.
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

