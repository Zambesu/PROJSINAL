       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROJSINANAL.
      *****AREA DE COMENTARIOS******************************************
      *AUTOR: MATEUS RIBEIRO
      *OBJETIVO: COMPARAR DOIS ARQUIVOS
      *DATA:27/06/2024
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *CARREGA O ARQUIVO SINTÉTICO
         SELECT SINTETICO ASSIGN TO 'SINTETICO.TXT'
         ORGANIZATION IS LINE SEQUENTIAL.

      *CARREGA O ARQUIVO ANALÍTICO
         SELECT ANALITICO ASSIGN TO 'ANALITICO.TXT'
         ORGANIZATION IS LINE SEQUENTIAL.

      *CARREGA O ARQUIVO DE RESULTADO
      * QUE ENCONTROU NO SINTETICO E NO ANALITICO.
         SELECT RESULTADO1 ASSIGN TO 'RESULTADO1.TXT'
         ORGANIZATION IS LINE SEQUENTIAL.

      *CARREGA O ARQUIVO DE RESULTADO
      * QUE ENCONTROU NO SINTETICO MAS NÃO NO ANALITICO.
         SELECT RESULTADO2 ASSIGN TO 'RESULTADO2.TXT'
         ORGANIZATION IS LINE SEQUENTIAL.

      *CARREGA O ARQUIVO DE RESULTADO
      * QUE ENCONTROU NO ANALITICO MAS NÃO NO SINTETICO.
         SELECT RESULTADO3 ASSIGN TO 'RESULTADO3.TXT'
         ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD SINTETICO.
         01 SINT-REG.
           05 SINT-PREFIXO             PIC 9(03).
           05 SINT-BALANCETE           PIC 9(09).
           05 SINT-PARTIDA             PIC 9(17).
           05 SINT-LCTO                PIC 9(02).
           05 SINT-VALOR               PIC 9(15).
           05 SINT-HST                 PIC 9(03).

       FD ANALITICO.
         01 ANAL-REG.
           05 ANAL-PREF-ORIG           PIC 9(03).
           05 ANAL-BALANCETE           PIC 9(09).
           05 ANAL-PREF-DEST           PIC 9(05).
           05 ANAL-PARTIDA             PIC 9(17).
           05 ANAL-LCTO                PIC 9(02).
           05 ANAL-VALOR               PIC 9(15).
           05 ANAL-HST                 PIC 9(03).

       FD RESULTADO1.
         01 RESUL1-REG.
           05 RESUL1-PREFIXO           PIC 9(05) VALUE ZEROS.
           05 RESUL1-BALANCETE         PIC 9(09) VALUE ZEROS.
           05 RESUL1-QT-PARTIDAS       PIC 9(06) VALUE ZEROS.
           05 RESUL1-QT-LCTOS          PIC 9(04) VALUE ZEROS.
           05 RESUL1-TOTAL             PIC 9(17) VALUE ZEROS.

       FD RESULTADO2.
         01 RESUL2-REG.
           05 RESUL2-PREFIXO           PIC 9(05) VALUE ZEROS.
           05 RESUL2-BALANCETE         PIC 9(09) VALUE ZEROS.
           05 RESUL2-PARTIDA           PIC 9(06) VALUE ZEROS.
           05 RESUL2-LCTO              PIC 9(04) VALUE ZEROS.
           05 RESUL2-VALOR             PIC 9(17) VALUE ZEROS.
           05 RESUL2-HST               PIC 9(03) VALUE ZEROS.

       FD RESULTADO3.
         01 RESUL3-REG.
           05 RESUL3-PREF-ORIG         PIC 9(03) VALUE ZEROS.
           05 RESUL3-BALANCETE         PIC 9(09) VALUE ZEROS.
           05 RESUL3-PREF-DEST         PIC 9(05) VALUE ZEROS.
           05 RESUL3-PARTIDA           PIC 9(17) VALUE ZEROS.
           05 RESUL3-LCTO              PIC 9(02) VALUE ZEROS.
           05 RESUL3-VALOR             PIC 9(15) VALUE ZEROS.
           05 RESUL3-HST               PIC 9(03) VALUE ZEROS.

       WORKING-STORAGE SECTION.

       77 WRK-DATA                     PIC 9(08).
       77 WRK-STATUS-SINT              PIC X(03) VALUE SPACES.
       77 WRK-STATUS-ANAL              PIC X(03) VALUE SPACES.
       77 WRK-R1-QT-PARTIDAS           PIC 9(06) VALUE ZEROS.
       77 WRK-R1-QT-LCTOS              PIC 9(04) VALUE ZEROS.
       77 WRK-R1-VALORTOTAL            PIC 9(15) VALUE ZEROS.

       PROCEDURE DIVISION.
       0001-MAIN-PROCEDURE.
           DISPLAY 'INSIRA A DATA...'
           ACCEPT WRK-DATA FROM CONSOLE.
           OPEN INPUT SINTETICO.
           OPEN INPUT ANALITICO.
           OPEN OUTPUT RESULTADO1.
           OPEN OUTPUT RESULTADO2.
           OPEN OUTPUT RESULTADO3.
           PERFORM 1000-PROC-SINT.
           PERFORM 2000-PROC-ANAL.
           CLOSE SINTETICO.
           CLOSE ANALITICO.
           CLOSE RESULTADO1.
           CLOSE RESULTADO2.
           CLOSE RESULTADO3.
           DISPLAY '-------------------'.
           DISPLAY 'FIM DO PROGRAMA'.
           STOP RUN.

       1000-PROC-SINT.
       PERFORM UNTIL WRK-STATUS-SINT = 'FIM'
         READ SINTETICO INTO SINT-REG
         AT END
           MOVE 'FIM' TO WRK-STATUS-SINT
         NOT AT END
           PERFORM UNTIL WRK-STATUS-ANAL = 'FIM'
               READ ANALITICO INTO ANAL-REG
             AT END
               MOVE 'FIM' TO WRK-STATUS-ANAL
             NOT AT END
             IF ANAL-REG = SINT-REG
                MOVE SINT-PREFIXO TO RESUL1-PREFIXO
                MOVE SINT-BALANCETE TO RESUL1-BALANCETE
                MOVE 1 TO RESUL1-QT-PARTIDAS
                ADD 1 TO RESUL1-QT-LCTOS
                ADD SINT-VALOR TO RESUL1-TOTAL
                WRITE RESUL1-REG
             ELSE
                MOVE SINT-PREFIXO TO RESUL2-PREFIXO
                MOVE SINT-BALANCETE TO RESUL2-BALANCETE
                MOVE SINT-PARTIDA TO RESUL2-PARTIDA
                MOVE SINT-LCTO TO RESUL2-LCTO
                ADD SINT-VALOR TO RESUL2-VALOR
                WRITE RESUL2-REG
           END-PERFORM
       END-PERFORM.

       2000-PROC-ANAL.
           MOVE SPACES TO WRK-STATUS-SINT
           PERFORM UNTIL WRK-STATUS-SINT = 'FIM'
           READ ANALITICO INTO ANAL-REG
           AT END
            MOVE 'FIM' TO WRK-STATUS-ANAL
           NOT AT END
            PERFORM UNTIL WRK-STATUS-SINT = 'FIM'
              IF ANAL-REG NOT EQUAL SINT-REG
                 MOVE ANAL-PREF-ORIG TO RESUL3-PREF-ORIG
                 MOVE ANAL-BALANCETE TO RESUL3-BALANCETE
                 MOVE ANAL-PREF-DEST TO RESUL3-PREF-DEST
                 MOVE ANAL-PARTIDA   TO RESUL3-PARTIDA
                 MOVE ANAL-LCTO      TO RESUL3-LCTO
                 MOVE ANAL-VALOR     TO RESUL3-VALOR
                 MOVE ANAL-HST       TO RESUL3-HST
                 WRITE RESUL3-REG
             END-IF
           END-PERFORM
           END-PERFORM.
