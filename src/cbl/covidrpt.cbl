       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COVIDRPT.
       AUTHOR.        DONGWON K.
      *
       ENVIRONMENT DIVISION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN001 ASSIGN TO INDD01
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

           SELECT OUT001 ASSIGN TO OUTCOVID
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  IN001 RECORDING MODE F
               RECORD CONTAINS 170 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS COVID-REC.
       01  COVID-REC            PIC X(170).

       FD  OUT001 RECORDING MODE F
               RECORD CONTAINS 166 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS OUTPUT-REC.
       01  OUTPUT-REC           PIC X(166).

       WORKING-STORAGE SECTION.

       77  WS-COUNT             PIC 9(02) VALUE ZEROS.
       77  WS-PRT-LENGTH        PIC 9(02) VALUE ZEROS.
       77  WS-PRT-START         PIC 9(02) VALUE ZEROS.

       01  FILLER.
           05 LASTREC           PIC X VALUE SPACE.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR         PIC 9(04).
               10  WS-CURRENT-MONTH        PIC 9(02).
               10  WS-CURRENT-DAY          PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR         PIC 9(02).
               10  WS-CURRENT-MINUTE       PIC 9(02).
               10  WS-CURRENT-SECOND       PIC 9(02).
               10  WS-CURRENT-CENTISECOND  PIC 9(02).

       01  UNSTRING-COVID-REC.
           05  UCR-ID                      PIC X(38).
           05  UCR-COUNTRY                 PIC X(33).
           05  UCR-COUNTRY-CODE            PIC X(4).
           05  UCR-SLUG                    PIC X(34).
           05  UCR-NEW-CONFIRMED           PIC X(7).
           05  UCR-TOTAL-CONFIRMED         PIC X(9).
           05  UCR-NEW-DEATHS              PIC X(5).
           05  UCR-TOTAL-DEATHS            PIC X(7).
           05  UCR-NEW-RECOVERED           PIC X(5).
           05  UCR-TOTAL-RECOVERED         PIC X(7).
           05  UCR-DATE-TIME-REC.
               10  FILLER                  PIC X(1).
               10  UCR-DATE-TIME           PIC X(24).
               10  FILLER                  PIC X(1).

       01  HEADER-1.
           05  FILLER         PIC X(67) VALUE SPACES.
           05  FILLER         PIC X(26) VALUE
           'Covid-19 Report By Country'.
           05  FILLER         PIC X(67) VALUE SPACES.
      *
       01  HEADER-2.
           05  FILLER         PIC X(132) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'Year '.
           05  HDR-YR         PIC 9(04).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(06) VALUE 'Month '.
           05  HDR-MO         PIC X(02).
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(04) VALUE 'Day '.
           05  HDR-DAY        PIC X(02).

       01  HEADER-3.
           05  FILLER         PIC X(08) VALUE SPACE.
           05  FILLER         PIC X(09) VALUE 'TIMESTAMP'.
           05  FILLER         PIC X(22) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'COUNTRY'.
           05  FILLER         PIC X(16) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'COUNTRY'.
           05  FILLER         PIC X(17) VALUE SPACES.
           05  FILLER         PIC X(04) VALUE 'SLUG'.
           05  FILLER         PIC X(19) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE 'NEW'.
           05  FILLER         PIC X(06) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'TOTAL'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(03) VALUE 'NEW'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'TOTAL'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(04) VALUE 'NEW'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'TOTAL'.
           05  FILLER         PIC X(03) VALUE SPACES.

       01  HEADER-4.
           05  FILLER         PIC X(63) VALUE SPACES.
           05  FILLER         PIC X(04) VALUE 'CODE'.
           05  FILLER         PIC X(41) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'CASES'.
           05  FILLER         PIC X(05) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'CASES'.
           05  FILLER         PIC X(04) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'DEATH'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE 'DEATH'.
           05  FILLER         PIC X(03) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'RECOVER'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE 'RECOVER'.
           05  FILLER         PIC X(01) VALUE SPACE.

       01  HEADER-5.
           05  FILLER         PIC X(01) VALUE SPACE.
           05  FILLER         PIC X(24) VALUE "************************"
           .
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(33) VALUE "*************************
      -     "********".
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '*******'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(34) VALUE "*************************
      -     "*********".
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '*******'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(09) VALUE '*********'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(05) VALUE '*****'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '*******'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '*******'.
           05  FILLER         PIC X(02) VALUE SPACES.
           05  FILLER         PIC X(07) VALUE '*******'.
           05  FILLER         PIC X(01) VALUE SPACE.

       01  WS-PRT-REC.
           05  FILLER                      PIC X(01) VALUE SPACE.
           05  WS-PRT-TIMESTAMP            PIC X(24).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-PRT-COUNTRY              PIC X(33).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-PRT-COUNTRY-CODE         PIC X(04).
           05  FILLER                      PIC X(04) VALUE SPACES.
           05  WS-PRT-SLUG                 PIC X(34).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-PRT-NEW-CONFIRMED        PIC X(07).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-PRT-TOTAL-CONFIRMED      PIC X(09).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-PRT-NEW-DEATHS           PIC X(05).
           05  FILLER                      PIC X(02) VALUE SPACES.
           05  WS-PRT-TOTAL-DEATHS         PIC X(07).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-PRT-NEW-RECOVERED        PIC X(05).
           05  FILLER                      PIC X(03) VALUE SPACES.
           05  WS-PRT-TOTAL-RECOVERED      PIC X(07).
           05  FILLER                      PIC X(01) VALUE SPACE.



      ****************************************************************
      *                  PROCEDURE DIVISION                          *
      ****************************************************************
       PROCEDURE DIVISION.
      *
       1000-OPEN-FILES.
      *---------------* 
           OPEN INPUT  IN001.
           OPEN OUTPUT OUT001.
      *
       2000-WRITE-HEADERS.
      *------------------* 
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR  TO HDR-YR.
           MOVE WS-CURRENT-MONTH TO HDR-MO.
           MOVE WS-CURRENT-DAY   TO HDR-DAY.
           WRITE OUTPUT-REC FROM HEADER-1.
           WRITE OUTPUT-REC FROM HEADER-2.
           MOVE SPACES TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC FROM HEADER-5.
           WRITE OUTPUT-REC FROM HEADER-3.
           WRITE OUTPUT-REC FROM HEADER-4.
           WRITE OUTPUT-REC FROM HEADER-5.

      *
       3000-PROC-RECORDS.
      *-----------------* 
           PERFORM 5000-READ-RECORD
           PERFORM UNTIL LASTREC = 'Y'
                 PERFORM 6000-PARSE-RECORD
                 IF UCR-ID NOT = "ID" THEN
                       PERFORM 7000-WRITE-CONTENT
                 END-IF
                 PERFORM 5000-READ-RECORD
           END-PERFORM.
           WRITE OUTPUT-REC FROM HEADER-5.

      *
       4000-CLOSE-STOP.
      *---------------* 
           CLOSE IN001.
           CLOSE OUT001.
           GOBACK.
      *
       5000-READ-RECORD.
      *----------------* 
           READ IN001
           AT END MOVE 'Y' TO LASTREC
           END-READ.
           EXIT.
      *
       6000-PARSE-RECORD.
      *-----------------* 
      * FOR COUNTRY NAMES CONTAINING ','
           INSPECT COVID-REC
              REPLACING FIRST ", " BY '# '.
           UNSTRING COVID-REC DELIMITED BY ','
           INTO UCR-ID, UCR-COUNTRY, UCR-COUNTRY-CODE, UCR-SLUG,
           UCR-NEW-CONFIRMED, UCR-TOTAL-CONFIRMED, UCR-NEW-DEATHS,
           UCR-TOTAL-DEATHS, UCR-NEW-RECOVERED, UCR-TOTAL-RECOVERED,
           UCR-DATE-TIME-REC
           END-UNSTRING.
           EXIT.
      *
       7000-WRITE-CONTENT.
      *------------------* 
           INSPECT UCR-DATE-TIME REPLACING FIRST '"' BY ' '.
           MOVE UCR-DATE-TIME TO WS-PRT-TIMESTAMP.

           PERFORM 7100-MOVE-COUNTRY.

           MOVE UCR-COUNTRY-CODE TO WS-PRT-COUNTRY-CODE.

           PERFORM 7200-MOVE-SLUG.

           PERFORM 7300-MOVE-NEW-CONFIRMED.

           PERFORM 7400-MOVE-TOTAL-CONFIRMED.

           PERFORM 7500-MOVE-NEW-DEATHS.

           PERFORM 7600-MOVE-TOTAL-DEATHS.

           PERFORM 7700-MOVE-NEW-RECOVERED.

           PERFORM 7800-MOVE-TOTAL-RECOVERED.
           
           WRITE OUTPUT-REC FROM WS-PRT-REC.
           EXIT.
      *
       7100-MOVE-COUNTRY.
      *-----------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT UCR-COUNTRY TALLYING WS-COUNT FOR ALL '#'
      * FOR COUNTRY NAMES CONTAINING '#'
           IF WS-COUNT > 0 THEN
                INSPECT UCR-COUNTRY
                    REPLACING FIRST '#' BY ','
           END-IF.
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-COUNTRY, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-COUNTRY.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-COUNTRY).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-COUNTRY, TRAILING)
                TO WS-PRT-COUNTRY(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7200-MOVE-SLUG.
      *--------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-SLUG, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-SLUG.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-SLUG).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-SLUG, TRAILING)
                TO WS-PRT-SLUG(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7300-MOVE-NEW-CONFIRMED.
      *-----------------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-NEW-CONFIRMED, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-NEW-CONFIRMED.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-NEW-CONFIRMED).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-NEW-CONFIRMED, TRAILING)
                TO WS-PRT-NEW-CONFIRMED(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7400-MOVE-TOTAL-CONFIRMED.
      *-------------------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-TOTAL-CONFIRMED, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-TOTAL-CONFIRMED.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-TOTAL-CONFIRMED).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-TOTAL-CONFIRMED, TRAILING)
                TO WS-PRT-TOTAL-CONFIRMED(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7500-MOVE-NEW-DEATHS.
      *--------------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-NEW-DEATHS, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-NEW-DEATHS.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-NEW-DEATHS).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-NEW-DEATHS, TRAILING)
                TO WS-PRT-NEW-DEATHS(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7600-MOVE-TOTAL-DEATHS.
      *----------------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-TOTAL-DEATHS, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-TOTAL-DEATHS.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-TOTAL-DEATHS).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-TOTAL-DEATHS, TRAILING)
                TO WS-PRT-TOTAL-DEATHS(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7700-MOVE-NEW-RECOVERED.
      *-----------------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-NEW-RECOVERED, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-NEW-RECOVERED.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-NEW-RECOVERED).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-NEW-RECOVERED, TRAILING)
                TO WS-PRT-NEW-RECOVERED(WS-PRT-START:WS-COUNT).
           EXIT.
      *
       7800-MOVE-TOTAL-RECOVERED.
      *-------------------------* 
           MOVE 0 TO WS-COUNT.
           INSPECT FUNCTION TRIM(UCR-TOTAL-RECOVERED, TRAILING)
                   TALLYING WS-COUNT FOR CHARACTERS.
           MOVE SPACES TO WS-PRT-TOTAL-RECOVERED.
           COMPUTE WS-PRT-LENGTH = FUNCTION
                                       LENGTH(WS-PRT-TOTAL-RECOVERED).
           COMPUTE WS-PRT-START = (WS-PRT-LENGTH - WS-COUNT)
                                       / 2 + 1.
           MOVE FUNCTION TRIM(UCR-TOTAL-RECOVERED, TRAILING)
                TO WS-PRT-TOTAL-RECOVERED(WS-PRT-START:WS-COUNT).
           EXIT.
