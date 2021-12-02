       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GETCLAIM.
       AUTHOR.        DONGWON K.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNEMP-CLAIM-FILE ASSIGN TO CLAIMS
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS UNEMP-CLAIM-KEY
           FILE STATUS IS WS-FILE-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  UNEMP-CLAIM-FILE
           RECORD IS VARYING 1 TO 260 CHARACTERS
           DEPENDING ON WS-REC-LEN.

       01  UNEMP-CLAIM-REC.
           05 FILLER                        OCCURS 1 TO 260 TIMES
                                            DEPENDING ON WS-REC-LEN
                                            PIC X.

       01  UNEMP-CLAIM-KEY                  PIC X(8).
      *
       WORKING-STORAGE SECTION. 

       01  WS-FILE-STATUS.
           05 WS-VSAM-FS                    PIC X(02).
              88 C-VSAM-OK                  VALUE '00'.
              88 C-VSAM-WRONGLEN            VALUE '04'.
              88 C-VSAM-ENDFILE             VALUE '10'.
              88 C-VSAM-DUPREC              VALUE '22'.
              88 C-VSAM-NOTFND              VALUE '23'.
              88 C-VSAM-NOTEXST             VALUE '35'.
              88 C-VSAM-OPEN-NORMAL         VALUE '97'.

       01  WS-REC-LEN                       PIC 9(3) COMP.

       01  WS-FILE-FLAG.
           05 WS-VSAM-OPEN                  PIC X(01) VALUE 'N'.
           05 WS-VSAM-FOUND                 PIC X(01) VALUE 'N'.

       01  WS-RETURN-CODE                   PIC X(02).
           88 C-STATUS-OK                   VALUE '00'.
           88 C-STATUS-NOTFND               VALUE '04'.
           88 C-STATUS-WRONGNO              VALUE '08'.
           88 C-STATUS-ABEND                VALUE '12'.

       01  WS-SUBSCRIPTS.
           05 SUB1                          PIC 9(03).

      *
       LINKAGE SECTION.

       01  LS-SEARCH-DATA.
           05 LS-SEARCH-CMD                 PIC X(01).
              88 C-COMMAND-READ             VALUE 'R'.
              88 C-COMMAND-DELETE           VALUE 'D'.
              88 C-COMMAND-INSERT           VALUE 'I'.
              88 C-COMMAND-UPDATE           VALUE 'U'.      
           05 LS-SEARCH-REC.             
              10 LS-SEARCH-ID               PIC X(08).         
              10 LS-SEARCH-CLAIMS           PIC X(252).
          
       01  LS-NUM-OF-RECS                   PIC 9(03).

       01  LS-RETURN-DATA.
           05 FILLER                        OCCURS 1 TO 200 DEPENDING ON
                                            LS-NUM-OF-RECS.
              10 LS-RETURN-REC-LEN          PIC 9(03) COMP.                     
              10 LS-RETURN-REC              PIC X(260).

       01  LS-RETURN-CODE                   PIC X(02).

       PROCEDURE DIVISION USING LS-SEARCH-DATA,
                                LS-NUM-OF-RECS,                     
                                LS-RETURN-DATA, 
                                LS-RETURN-CODE. 
      *
       0000-MAIN-PARA.
      *--------------*
           SET C-STATUS-OK TO TRUE.

           PERFORM 1000-FILE-OPEN       
              THRU 1000-FILE-OPEN-EXIT.

           EVALUATE TRUE
              WHEN C-COMMAND-READ
                 PERFORM 2000-CLAIM-READ
                    THRU 2000-CLAIM-READ-EXIT
              WHEN C-COMMAND-DELETE
                 PERFORM 3000-CLAIM-DELETE
                    THRU 3000-CLAIM-DELETE-EXIT
              WHEN C-COMMAND-INSERT
                 PERFORM 4000-CLAIM-INSERT
                    THRU 4000-CLAIM-INSERT-EXIT
              WHEN C-COMMAND-UPDATE
                 PERFORM 5000-CLAIM-UPDATE
                    THRU 5000-CLAIM-UPDATE-EXIT
              WHEN OTHER
                 DISPLAY 'WRONG COMMAND SPECIFIED'
                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.

           PERFORM 9000-FILE-CLOSE      
              THRU 9000-FILE-CLOSE-EXIT.

           MOVE WS-RETURN-CODE TO LS-RETURN-CODE.

           EXIT PROGRAM.
      *

       1000-FILE-OPEN.
      *--------------*
           IF LS-NUM-OF-RECS >= 1 THEN
               OPEN I-O UNEMP-CLAIM-FILE

               IF C-VSAM-OK OR C-VSAM-OPEN-NORMAL THEN
                    DISPLAY 'FILE OPENED : UNEMP-CLAIM-FILE'
                    MOVE 'Y' TO WS-VSAM-OPEN
               ELSE
                    DISPLAY 'FILE OPEN ERROR: UNEMP-CLAIM-FILE'
                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
               END-IF
           ELSE
               PERFORM 7000-NEED-GT-ZERO 
                  THRU 7000-NEED-GT-ZERO-EXIT
           END-IF.
       1000-FILE-OPEN-EXIT.
      *-------------------*
           EXIT.

       2000-CLAIM-READ.
      *---------------* 
           IF C-STATUS-OK THEN
                IF LS-NUM-OF-RECS = 1 THEN
                    PERFORM 2100-CLAIM-READ-ONE
                       THRU 2100-CLAIM-READ-ONE-EXIT
                ELSE IF LS-NUM-OF-RECS > 1 THEN
                    PERFORM 2200-CLAIM-READ-MANY
                       THRU 2200-CLAIM-READ-MANY-EXIT
                END-IF
           END-IF.
       2000-CLAIM-READ-EXIT.
      *--------------------* 
           EXIT.

       2100-CLAIM-READ-ONE.
      *-------------------*
           MOVE LS-SEARCH-ID TO UNEMP-CLAIM-KEY.
           
           READ UNEMP-CLAIM-FILE KEY IS UNEMP-CLAIM-KEY.

           EVALUATE TRUE
               WHEN C-VSAM-OK
                    PERFORM 2110-CLAIM-GET-REC
                       THRU 2110-CLAIM-GET-REC-EXIT
               WHEN C-VSAM-NOTFND
                    PERFORM 6000-NO-RECORD-FOUND
                       THRU 6000-NO-RECORD-FOUND-EXIT
               WHEN OTHER
                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.
       2100-CLAIM-READ-ONE-EXIT.
      *------------------------*
           EXIT.

       2110-CLAIM-GET-REC.
      *------------------*
           MOVE UNEMP-CLAIM-REC TO LS-RETURN-REC (1).
           MOVE WS-REC-LEN      TO LS-RETURN-REC-LEN (1).
       2110-CLAIM-GET-REC-EXIT.
      *-----------------------*
           EXIT.

       2200-CLAIM-READ-MANY.
      *--------------------*
           MOVE LS-SEARCH-ID TO UNEMP-CLAIM-KEY.

           START UNEMP-CLAIM-FILE
                KEY IS NOT LESS THAN UNEMP-CLAIM-KEY
           END-START.

           EVALUATE TRUE
               WHEN C-VSAM-OK
                    MOVE 'Y' TO WS-VSAM-FOUND

                    PERFORM 2210-CLAIM-READ-NEXT
                       THRU 2210-CLAIM-READ-NEXT-EXIT
                       VARYING SUB1 FROM 1 BY 1
                       UNTIL WS-VSAM-FOUND = 'N' OR
                             SUB1 > LS-NUM-OF-RECS OR
                             C-STATUS-ABEND

                    IF WS-VSAM-FOUND = 'N' THEN
                       COMPUTE LS-NUM-OF-RECS = SUB1 - 2
                    ELSE
                       COMPUTE LS-NUM-OF-RECS = SUB1 - 1
                    END-IF
               WHEN C-VSAM-NOTFND
                    PERFORM 6000-NO-RECORD-FOUND
                       THRU 6000-NO-RECORD-FOUND-EXIT
               WHEN C-VSAM-ENDFILE
                    DISPLAY 'END OF FILE REACHED : ' UNEMP-CLAIM-KEY
               WHEN OTHER
                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.
       2200-CLAIM-READ-MANY-EXIT.
      *-------------------------*
           EXIT.

       2210-CLAIM-READ-NEXT.
      *--------------------*
           READ UNEMP-CLAIM-FILE NEXT RECORD.

           EVALUATE TRUE
               WHEN C-VSAM-OK
                    MOVE UNEMP-CLAIM-REC
                                TO LS-RETURN-REC (SUB1)
                    MOVE WS-REC-LEN
                                TO LS-RETURN-REC-LEN (SUB1)
               WHEN C-VSAM-ENDFILE
                    MOVE 'N' TO WS-VSAM-FOUND
               WHEN OTHER
                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.
       2210-CLAIM-READ-NEXT-EXIT.
      *-------------------------*
           EXIT.

       3000-CLAIM-DELETE.
      *-----------------* 
           MOVE LS-SEARCH-ID TO UNEMP-CLAIM-KEY.

           READ UNEMP-CLAIM-FILE KEY IS UNEMP-CLAIM-KEY.

           EVALUATE TRUE
               WHEN C-VSAM-OK
                    DELETE UNEMP-CLAIM-FILE
                    IF C-VSAM-OK THEN
                       DISPLAY 'RECORD IS DELETED ' LS-SEARCH-ID

                       PERFORM 2110-CLAIM-GET-REC
                          THRU 2110-CLAIM-GET-REC-EXIT                          
                    ELSE
                       DISPLAY 'RECORD IS NOT DELETED' LS-SEARCH-ID

                       PERFORM 9999-ABEND-PARA 
                          THRU 9999-ABEND-PARA-EXIT                    
                    END-IF 
               WHEN C-VSAM-NOTFND
                    PERFORM 6000-NO-RECORD-FOUND
                       THRU 6000-NO-RECORD-FOUND-EXIT
               WHEN OTHER
                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.       
       3000-CLAIM-DELETE-EXIT.
      *----------------------* 
           EXIT.       

       4000-CLAIM-INSERT.
      *-----------------* 
           MOVE  0  TO WS-REC-LEN.
           INSPECT FUNCTION TRIM(LS-SEARCH-REC, TRAILING)
               TALLYING WS-REC-LEN  FOR CHARACTERS.     

           MOVE LS-SEARCH-REC(1:WS-REC-LEN) TO UNEMP-CLAIM-REC.

      D    DISPLAY ' WS-REC-LEN ' WS-REC-LEN.
      D    DISPLAY ' UNEMP-CLAIM-REC ' UNEMP-CLAIM-REC.           

           WRITE UNEMP-CLAIM-REC.
           EVALUATE TRUE
               WHEN C-VSAM-OK
                    DISPLAY 'RECORD IS INSERTED ' LS-SEARCH-ID     

                    PERFORM 2100-CLAIM-READ-ONE
                       THRU 2100-CLAIM-READ-ONE-EXIT
               WHEN OTHER
                    DISPLAY 'RECORD IS NOT INSERTED ' LS-SEARCH-ID    

                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.       
       4000-CLAIM-INSERT-EXIT.
      *----------------------*
           EXIT.

       5000-CLAIM-UPDATE.
      *-----------------* 
           MOVE LS-SEARCH-ID TO UNEMP-CLAIM-KEY.

           READ UNEMP-CLAIM-FILE KEY IS UNEMP-CLAIM-KEY.

           EVALUATE TRUE
               WHEN C-VSAM-OK
                    MOVE  0  TO WS-REC-LEN
                    INSPECT FUNCTION TRIM(LS-SEARCH-REC, TRAILING)
                       TALLYING WS-REC-LEN  FOR CHARACTERS         

                    MOVE LS-SEARCH-REC(1:WS-REC-LEN)
                                            TO UNEMP-CLAIM-REC

      D             DISPLAY ' WS-REC-LEN ' WS-REC-LEN
      D             DISPLAY ' UNEMP-CLAIM-REC ' UNEMP-CLAIM-REC

                    REWRITE UNEMP-CLAIM-REC
                    IF C-VSAM-OK THEN
                       DISPLAY 'RECORD IS UPDATED ' LS-SEARCH-ID 

                       PERFORM 2100-CLAIM-READ-ONE
                          THRU 2100-CLAIM-READ-ONE-EXIT           
                    ELSE
                       DISPLAY 'RECORD IS NOT UPDATED ' LS-SEARCH-ID

                       PERFORM 9999-ABEND-PARA 
                          THRU 9999-ABEND-PARA-EXIT                    
                    END-IF 
               WHEN C-VSAM-NOTFND
                    PERFORM 6000-NO-RECORD-FOUND
                       THRU 6000-NO-RECORD-FOUND-EXIT
               WHEN OTHER
                    PERFORM 9999-ABEND-PARA 
                       THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.       
       5000-CLAIM-UPDATE-EXIT.
      *----------------------* 
           EXIT.      


       6000-NO-RECORD-FOUND.
      *--------------------*
           DISPLAY 'NO CLAIM RECORD FOUND : ' UNEMP-CLAIM-KEY.

           MOVE  ZERO              TO LS-NUM-OF-RECS.           
           SET   C-STATUS-NOTFND   TO TRUE.
       6000-NO-RECORD-FOUND-EXIT.
      *-------------------------*
           EXIT.

       7000-NEED-GT-ZERO.
      *-----------------*
           DISPLAY 'NUMBER OF RECORDS SHOULD BE GREATER THAN ZERO : '
           LS-NUM-OF-RECS.

           SET   C-STATUS-WRONGNO  TO TRUE.
       7000-NEED-GT-ZERO-EXIT.
      *----------------------*
           EXIT.


       9000-FILE-CLOSE.
      *---------------*
           IF WS-VSAM-OPEN = 'Y'
              CLOSE UNEMP-CLAIM-FILE

              DISPLAY 'FILE CLOSED'         
           END-IF.
       9000-FILE-CLOSE-EXIT.
      *--------------------*
           EXIT.


       9999-ABEND-PARA.
      *---------------*
           DISPLAY 'FILE STATUS : ' WS-VSAM-FS

           SET   C-STATUS-ABEND TO TRUE.
       9999-ABEND-PARA-EXIT.
      *--------------------*
           EXIT.
