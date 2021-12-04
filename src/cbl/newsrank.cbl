       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NEWSRANK.
       AUTHOR.        DONGWON K.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN001   ASSIGN TO INDD01
                          ORGANIZATION IS SEQUENTIAL
                          ACCESS MODE IS SEQUENTIAL.

           SELECT OUT001  ASSIGN TO OUDD01
                          ORGANIZATION IS SEQUENTIAL
                          ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN001          RECORDING MODE IS F
                          RECORD CONTAINS 143 CHARACTERS
                          DATA RECORD IS INPUT-REC.                   
       01  INPUT-REC      PIC X(143).                         

       FD  OUT001         RECORDING MODE F
                          RECORD CONTAINS 137 CHARACTERS
                          DATA RECORD IS OUTPUT-REC.
       01  OUTPUT-REC     PIC X(137).

       WORKING-STORAGE SECTION.
      
       77  WS-COUNT                   PIC 9(03) COMP.
       77  WS-PUBLISH-HOUR            PIC 9(02) COMP VALUE 23.
       77  WS-PUBLISH-MINUTE          PIC 9(02) COMP VALUE 59.
       77  WS-VOTES-EXPNT             PIC 9(01)V9(01) COMP-3 VALUE 0.8.
       77  WS-AGE-EXPNT               PIC 9(01)V9(01) COMP-3 VALUE 1.8.

       01  UNSTRING-DATE-TIME.
           05 UDT-HACKER-DATE         PIC X(10).
           05 UDT-HACKER-TIME         PIC X(06).

       01  UNSTRING-TIME.              
           05 UT-HOUR                 PIC X(02).
           05 UT-MINUTE               PIC X(02).

       01  WS-RANKING-SCORE           PIC S9999V999999 COMP-3.   
       01  WS-SCORE-POINT             PIC S9999V999999 COMP-3.
       01  WS-SCORE-AGE               PIC S9999V999999 COMP-3.

       01  WS-AGE-HOURS               PIC 9(2) COMP.
       01  WS-POST-TIME.   
           05  WS-POST-HOUR           PIC 9(2).
           05  FILLER                 PIC X(1) VALUE ':'.
           05  WS-POST-MINUTE         PIC 9(2).

       01  WS-PUB-TIME-MIN            PIC 9(04) COMP. 
       01  WS-POST-TIME-MIN           PIC 9(04) COMP.                 
      
       01  HACKER-IN-FIELDS. 
           05 HACK-IN-ID              PIC X(08).
           05 HACK-IN-TITLE           PIC X(95).
           05 HACK-IN-POINTS          PIC 9(04).
           05 HACK-IN-COMMENTS        PIC 9(04).
           05 HACK-IN-AUTHOR          PIC X(15).
           05 HACK-IN-CREATE-DT       PIC X(16).
      
       01  HACKER-OUT-FIELDS.   
           05 HACK-OUT-ID             PIC X(08).
           05 HACK-OUT-TITLE          PIC X(95).
           05 HACK-OUT-POINTS         PIC 9(04).
           05 HACK-OUT-COMMENTS       PIC 9(04).
           05 HACK-OUT-AUTHOR         PIC X(15).
           05 HACK-OUT-TIME           PIC X(05).
           05 HACK-OUT-RANKING-SCORE  PIC S9999V999999 COMP-3. 

       01  FILLER.
           05 LASTREC                 PIC X VALUE SPACE.

      ****************************************************************
      *                  PROCEDURE DIVISION                          *
      ****************************************************************
       PROCEDURE DIVISION.
       
      *
       0000-MAIN-PARA.
      *--------------* 
           PERFORM 1000-OPEN-FILES 
              THRU 1000-OPEN-FILES-EXIT.

           PERFORM 2000-READ-RECORD
              THRU 2000-READ-RECORD-EXIT.

           PERFORM UNTIL LASTREC = 'Y' 

                 PERFORM 3000-PARSE-RECORD
                    THRU 3000-PARSE-RECORD-EXIT
                 
                 PERFORM 4000-CHECK-TITLE
                    THRU 4000-CHECK-TITLE-EXIT

                 IF WS-COUNT > 0 THEN
      D             DISPLAY 'INPUT-REC ' INPUT-REC

                    PERFORM 5000-GET-RANK-SCORE
                       THRU 5000-GET-RANK-SCORE-EXIT

                    PERFORM 6000-PRINT-OUTPUT
                       THRU 6000-PRINT-OUTPUT-EXIT
                 END-IF 

                 PERFORM 2000-READ-RECORD
                    THRU 2000-READ-RECORD-EXIT
           END-PERFORM.

           PERFORM 9000-CLOSE-FILES 
              THRU 9000-CLOSE-FILES-EXIT.

           GOBACK.

      *
       1000-OPEN-FILES.
      *---------------* 
           OPEN INPUT  IN001.
           OPEN OUTPUT OUT001.
       1000-OPEN-FILES-EXIT.
      *--------------------* 
           EXIT.    

      *
       2000-READ-RECORD.
      *----------------* 
           READ IN001
              AT END MOVE 'Y' TO LASTREC
           END-READ.
       2000-READ-RECORD-EXIT.
      *---------------------*     
           EXIT.

      *
       3000-PARSE-RECORD.
      *-----------------* 
           INSPECT INPUT-REC
              REPLACING ALL ', ' BY '# ' AFTER QUOTE.

           UNSTRING INPUT-REC DELIMITED BY ','
           INTO HACK-IN-ID, HACK-IN-TITLE, HACK-IN-POINTS, 
           HACK-IN-COMMENTS, HACK-IN-AUTHOR, HACK-IN-CREATE-DT
           END-UNSTRING.

           INSPECT HACK-IN-TITLE
              REPLACING ALL '# ' BY ', ' AFTER QUOTE.           
       3000-PARSE-RECORD-EXIT.
      *----------------------*   
           EXIT.

      *
       4000-CHECK-TITLE.
      *----------------*
           MOVE  0  TO WS-COUNT.
           INSPECT FUNCTION UPPER-CASE( HACK-IN-TITLE )
              TALLYING WS-COUNT FOR ALL 'MAINFRAME' 'COBOL'.   
       4000-CHECK-TITLE-EXIT.
      *---------------------* 
           EXIT.

      *
       5000-GET-RANK-SCORE.
      *------------------*
           PERFORM 5100-GET-AGE-HOURS
              THRU 5100-GET-AGE-HOURS-EXIT.

           COMPUTE WS-SCORE-POINT        =
                                (HACK-IN-POINTS - 1) ** WS-VOTES-EXPNT.  

           COMPUTE WS-SCORE-AGE          = 
                                (WS-AGE-HOURS + 2) ** WS-AGE-EXPNT.             

           IF WS-SCORE-AGE NOT = 0 THEN
              COMPUTE WS-RANKING-SCORE   = WS-SCORE-POINT                       
                                           / WS-SCORE-AGE
           ELSE 
              MOVE  0  TO WS-RANKING-SCORE                         
           END-IF.                                    
       5000-GET-RANK-SCORE-EXIT.
      *-----------------------* 
           EXIT.
      
      *
       5100-GET-AGE-HOURS.
      *------------------* 
           UNSTRING HACK-IN-CREATE-DT DELIMITED BY ' '
           INTO UDT-HACKER-DATE, UDT-HACKER-TIME,
           END-UNSTRING.

           UNSTRING UDT-HACKER-TIME DELIMITED BY ':'
           INTO UT-HOUR, UT-MINUTE,
           END-UNSTRING.       
           
           COMPUTE WS-POST-HOUR       =  FUNCTION NUMVAL(UT-HOUR).
           COMPUTE WS-POST-MINUTE     =  FUNCTION NUMVAL(UT-MINUTE).

      D    DISPLAY 'WS-POST-HOUR ' WS-POST-HOUR.
      D    DISPLAY 'WS-POST-MINUTE ' WS-POST-MINUTE.       

           COMPUTE WS-PUB-TIME-MIN    = 
                   WS-PUBLISH-HOUR * 60 + WS-PUBLISH-MINUTE.

           COMPUTE WS-POST-TIME-MIN   = 
                   WS-POST-HOUR * 60 + WS-POST-MINUTE.

           COMPUTE WS-AGE-HOURS       =  
                   ( WS-PUB-TIME-MIN - WS-POST-TIME-MIN ) / 60 .   

      D    DISPLAY 'WS-AGE-HOURS ' WS-AGE-HOURS.  

      *
       5100-GET-AGE-HOURS-EXIT.
      *-----------------------*  
           EXIT.        

      *
       6000-PRINT-OUTPUT.
      *-----------------*
           MOVE  HACK-IN-ID        TO HACK-OUT-ID.
           MOVE  HACK-IN-TITLE     TO HACK-OUT-TITLE.
           MOVE  HACK-IN-POINTS    TO HACK-OUT-POINTS.
           MOVE  HACK-IN-COMMENTS  TO HACK-OUT-COMMENTS.
           MOVE  HACK-IN-AUTHOR    TO HACK-OUT-AUTHOR.
           MOVE  WS-POST-TIME      TO HACK-OUT-TIME.              
           MOVE  WS-RANKING-SCORE  TO HACK-OUT-RANKING-SCORE.
           WRITE OUTPUT-REC        FROM HACKER-OUT-FIELDS.
       6000-PRINT-OUTPUT-EXIT.
      *----------------------* 
           EXIT. 

      *
       9000-CLOSE-FILES.
      *---------------*
           CLOSE IN001.
           CLOSE OUT001.
       9000-CLOSE-FILES-EXIT.
      *--------------------*
           EXIT.