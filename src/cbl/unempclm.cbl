      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    UNEMPCLM.
       AUTHOR.        DONGWON K.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT001 ASSIGN TO OUTCLAIM
              ORGANIZATION IS SEQUENTIAL
              ACCESS MODE IS SEQUENTIAL.

      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  OUT001 RECORDING MODE V
           RECORD IS VARYING 1 TO 582 CHARACTERS
           DEPENDING ON WS-OUT-REC-LEN
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS OUTPUT-REC.

       01  OUTPUT-REC.
           05 FILLER                  OCCURS 1 TO 582 TIMES
                                      DEPENDING ON WS-OUT-REC-LEN
                                      PIC X.
      *
       WORKING-STORAGE SECTION.

       77  WS-BY-AGE                  PIC X(12) VALUE 'BY AGE'.
       77  WS-BY-ETHNICITY            PIC X(12) VALUE 'BY ETHNICITY'.
       77  WS-BY-INDUSTRY             PIC X(12) VALUE 'BY INDUSTRY'.
       77  WS-BY-RACE                 PIC X(12) VALUE 'BY RACE'.
       77  WS-BY-GENDER               PIC X(12) VALUE 'BY GENDER'.

       01  WS-INPUT-REC.
           05 WS-INP-CMD                    PIC X(01).
              88 C-CMD-READ                 VALUE 'R'.
              88 C-CMD-DELETE               VALUE 'D'.
              88 C-CMD-INSERT               VALUE 'I'.
              88 C-CMD-UPDATE               VALUE 'U'.
              88 C-CMD-EOF                  VALUE 'E'.
           05 FILLER                        PIC X(01).
           05 WS-INP-ID                     PIC X(08).
           05 FILLER                        PIC X(01).
           05 WS-INP-DATA                   PIC X(252).
           05 WS-INP-READ                   REDEFINES WS-INP-DATA.
              10 WS-INP-NUM-OF-RECS         PIC 9(03).
              10 FILLER                     PIC X(01).
              10 WS-INP-CATEGORY            PIC X(12).
                 88 C-BY-AGE                VALUE 'BY AGE'.
                 88 C-BY-ETHNICITY          VALUE 'BY ETHNICITY'.
                 88 C-BY-INDUSTRY           VALUE 'BY INDUSTRY'.
                 88 C-BY-RACE               VALUE 'BY RACE'.
                 88 C-BY-GENDER             VALUE 'BY GENDER'.

       01  WS-NUM-OF-RECS                   PIC 9(03).

       01  WS-SEARCH-DATA.
           05 WS-SEARCH-CMD                 PIC X(01).
           05 WS-SEARCH-REC.          
              10 WS-SEARCH-ID               PIC X(08) VALUE SPACES.        
              10 WS-SEARCH-CLAIMS           PIC X(252) VALUE SPACES.      

       01  WS-RETURN-DATA.
           05 FILLER                        OCCURS 1 TO 200
                                            DEPENDING ON WS-NUM-OF-RECS.
              10 WS-RETURN-REC-LEN          PIC 9(03) COMP.         
              10 WS-RETURN-ID               PIC X(08).                   
              10 WS-RETURN-REC              PIC X(252).

       01  WS-OUT-REC-LEN                   PIC 9(03) COMP.
           88 C-OUT-DIR-REC                 VALUE 70.
           88 C-BY-AGE-OUT-REC              VALUE 86.
           88 C-BY-ETHNICITY-OUT-REC        VALUE 76.
           88 C-BY-INDUSTRY-OUT-REC         VALUE 582.
           88 C-BY-RACE-OUT-REC             VALUE 150.
           88 C-BY-GENDER-OUT-REC           VALUE 46.

       01  WS-SUBSCRIPTS.
           05 ACCEPT-SUB                    PIC 9(03).
           05 SUB1                          PIC 9(03).

       01  WS-CURRENT-DATE-REC.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR          PIC 9(04).
               10  WS-CURRENT-MONTH         PIC 9(02).
               10  WS-CURRENT-DAY           PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR          PIC 9(02).
               10  WS-CURRENT-MINUTE        PIC 9(02).
               10  WS-CURRENT-SECOND        PIC 9(02).
               10  WS-CURRENT-CENTISECOND   PIC 9(02).

       01  WS-RETURN-CODE                PIC X(02).
           88 C-STATUS-OK                VALUE '00'.
           88 C-STATUS-ABEND             VALUE '12'.

       01  HEADER-1-DIR-READ.
           05  FILLER                    PIC X(32) VALUE
              'Unemployment Claims By Record ID'.
           05  FILLER                    PIC X(38) VALUE SPACES.

       01  HEADER-1-DIR-DELETE.
           05  FILLER                    PIC X(31) VALUE
              'Unemployment Claims [ DELETED ]'.              
           05  FILLER                    PIC X(39) VALUE SPACES.    

       01  HEADER-1-DIR-INSERT.
           05  FILLER                    PIC X(32) VALUE
              'Unemployment Claims [ INSERTED ]'.              
           05  FILLER                    PIC X(38) VALUE SPACES.    

       01  HEADER-1-DIR-UPDATE.
           05  FILLER                    PIC X(31) VALUE       
              'Unemployment Claims [ UPDATED ]'.                        
           05  FILLER                    PIC X(39) VALUE SPACES.                

       01  HEADER-1-SEQ-BY-AGE.
           05 FILLER                     PIC X(30) VALUE SPACES.
           05 FILLER                     PIC X(26) VALUE
              'Unemployment Claims By Age'.

       01 HEADER-1-SEQ-BY-ETHNICITY.
           05 FILLER                     PIC X(22) VALUE SPACES.
           05 FILLER                     PIC X(32) VALUE
              'Unemployment Claims By Ethnicity'.

       01  HEADER-1-SEQ-BY-INDUSTRY.
           05 FILLER                     PIC X(275) VALUE SPACES.
           05 FILLER                     PIC X(31) VALUE
              'Unemployment Claims By Industry'.

       01  HEADER-1-SEQ-BY-RACE.
           05 FILLER                     PIC X(61) VALUE SPACES.
           05 FILLER                     PIC X(27) VALUE
              'Unemployment Claims By Race'.

       01  HEADER-1-SEQ-BY-GENDER.
           05 FILLER                     PIC X(08) VALUE SPACES.
           05 FILLER                     PIC X(29) VALUE
              'Unemployment Claims By Gender'.

       01  HEADER-2.
           05  FILLER                    PIC X(05) VALUE 'Year '.
           05  HDR-YR                    PIC 9(04).
           05  FILLER                    PIC X(02) VALUE SPACES.
           05  FILLER                    PIC X(06) VALUE 'Month '.
           05  HDR-MO                    PIC X(02).
           05  FILLER                    PIC X(02) VALUE SPACES.
           05  FILLER                    PIC X(04) VALUE 'Day '.
           05  HDR-DAY                   PIC X(02).
           05  FILLER                    PIC X(56) VALUE SPACES.

       01  HEADER-3-DIR.
           05 FILLER                     PIC X(12) VALUE 'RECORD-ID : '.
           05 DIR-RECORD-ID              PIC X(08).
           05 FILLER                     PIC X(50) VALUE SPACES.

       01  HEADER-3-SEQ.
           05 FILLER                     PIC X(09) VALUE 'Record ID'.
           05 FILLER                     PIC X(09) VALUE SPACES.
           05 FILLER                     PIC X(04) VALUE 'Date'.
           05 FILLER                     PIC X(10) VALUE SPACES.
           05 HEADER-3-SEQ-REC           PIC X(550).
           05 HEADER-3-SEQ-BY-AGE        REDEFINES HEADER-3-SEQ-REC
                                         PIC X(54).
           05 HEADER-3-SEQ-BY-ETHNICITY  REDEFINES HEADER-3-SEQ-REC
                                         PIC X(44).
           05 HEADER-3-SEQ-BY-INDUSTRY   REDEFINES HEADER-3-SEQ-REC
                                         PIC X(550).
           05 HEADER-3-SEQ-BY-RACE       REDEFINES HEADER-3-SEQ-REC
                                         PIC X(118).
           05 HEADER-3-SEQ-BY-GENDER     REDEFINES HEADER-3-SEQ-REC
                                         PIC X(14).

       01  WS-HEADER-3-SEQ-BY-AGE.
           05 FILLER                  PIC X(01) VALUE SPACES.
           05 FILLER                  PIC X(03) VALUE '<22'.
           05 FILLER                  PIC X(01) VALUE SPACES.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '22-24'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '25-34'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '35-44'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '45-54'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '55-59'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '60-64'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(04) VALUE '>=65'.
           05 FILLER                  PIC X(01) VALUE SPACE.

       01  WS-HEADER-3-SEQ-BY-ETHNICITY.
           05 FILLER                  PIC X(18) VALUE
              'Hispanic or Latino'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(22) VALUE
              'Not Hispanic or Latino'.

       01  WS-HEADER-3-SEQ-BY-INDUSTRY.
           05 FILLER                  PIC X(15) VALUE
              'Wholesale Trade'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(26) VALUE
              'Transportation & Warehouse'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(12) VALUE
              'Construction'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(19) VALUE
              'Finance & Insurance'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(13) VALUE
              'Manufacturing'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(34) VALUE
              'Agricult./Forestry/Fishing/Hunting'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(21) VALUE
              'Public Administration'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(09) VALUE
              'Utilities'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(29) VALUE
              'Accommodation & Food Services'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(11) VALUE
              'Information'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(38) VALUE
              'Professional/Scientific/Tech. Services'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(30) VALUE
              'Real Estate & Rental & Leasing'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(45) VALUE
              'Other Services (except Public Administration)'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(37) VALUE
              'Management of Companies & Enterprises'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(20) VALUE
              'Educational Services'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(06) VALUE 'Mining'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(31) VALUE
              'Health Care & Social Assistance'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(32) VALUE
              'Arts, Entertainment & Recreation'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(43) VALUE
              'Admin. & Support/Waste Mgmt./Remedia. Serv.'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(12) VALUE
              'Retail Trade'.

       01  WS-HEADER-3-SEQ-BY-RACE.
           05 FILLER                  PIC X(05) VALUE 'White'.
           05 FILLER                  PIC X(01) VALUE SPACES.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE 'Asian'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(25) VALUE
              'Black or African American'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(33) VALUE
              'American Indian or Alaskan Native'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(41) VALUE
              'Native Hawaiian or Other Pacific Islander'.

       01  WS-HEADER-3-SEQ-BY-GENDER.
           05 FILLER                  PIC X(06) VALUE 'FEMALE'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(04) VALUE 'MALE'.
           05 FILLER                  PIC X(02) VALUE SPACES.

       01  WS-DIR-EMPTY-LN.
           05  FILLER                 PIC X(70) VALUE SPACES.

       01  WS-DIR-SEPARATOR.
           05  FILLER                 PIC X(70) VALUE
           '************************************************************
      -    '**********'.

       01  WS-DIR-SEPARATOR-BY-SEQ-REC.
           05 FILLER                  PIC X(29) VALUE
           '**************************** '.
           05 DIR-SEPARATOR-BY-CAT    PIC X(12).
           05 FILLER                  PIC X(29) VALUE
           ' ****************************'.

       01  WS-SEQ-SEPARATOR.
           05 FILLER                     PIC X(09) VALUE '*********'.
           05 FILLER                     PIC X(02) VALUE SPACES.
           05 FILLER                     PIC X(19) VALUE
              '*******************'.
           05 FILLER                     PIC X(02) VALUE SPACES.
           05 SEQ-SEPARATOR-REC          PIC X(550).
           05 SEQ-SEPARATOR-BY-AGE       REDEFINES SEQ-SEPARATOR-REC
                                         PIC X(54).
           05 SEQ-SEPARATOR-BY-ETHNICITY REDEFINES SEQ-SEPARATOR-REC
                                         PIC X(44).
           05 SEQ-SEPARATOR-BY-INDUSTRY  REDEFINES SEQ-SEPARATOR-REC
                                         PIC X(550).
           05 SEQ-SEPARATOR-BY-RACE      REDEFINES SEQ-SEPARATOR-REC
                                         PIC X(118).
           05 SEQ-SEPARATOR-BY-GENDER    REDEFINES SEQ-SEPARATOR-REC
                                         PIC X(14).

       01  WS-SEQ-SEPARATOR-BY-AGE.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.

       01  WS-SEQ-SEPARATOR-BY-ETHNICITY.
           05 FILLER                  PIC X(18) VALUE
              '******************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(22) VALUE
              '**********************'.

       01  WS-SEQ-SEPARATOR-BY-INDUSTRY.
           05 FILLER                  PIC X(15) VALUE
              '***************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(26) VALUE
              '**************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(12) VALUE
              '************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(19) VALUE
              '*******************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(13) VALUE
              '*************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(34) VALUE
              '**********************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(21) VALUE
              '*********************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(09) VALUE
              '*********'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(29) VALUE
              '*****************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(11) VALUE
              '***********'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(38) VALUE
              '**************************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(30) VALUE
              '******************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(45) VALUE
               '*********************************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(37) VALUE
              '*************************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(20) VALUE
              '********************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(06) VALUE '******'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(31) VALUE
              '*******************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(32) VALUE
              '********************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(43) VALUE
              '*******************************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(12) VALUE
              '************'.

       01  WS-SEQ-SEPARATOR-BY-RACE.
           05 FILLER                  PIC X(06) VALUE '******'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(05) VALUE '*****'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(25) VALUE
              '*************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(33) VALUE
              '*********************************'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(41) VALUE
              '*****************************************'.

       01  WS-SEQ-SEPARATOR-BY-GENDER.
           05 FILLER                  PIC X(06) VALUE '******'.
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 FILLER                  PIC X(06) VALUE '******'.

       01  WS-SEQ-OUTPUT.
           05 SEQ-OUT-RECORD-ID          PIC X(08).
           05 FILLER                     PIC X(03) VALUE SPACES.
           05 SEQ-OUT-DATE-TIME          PIC X(19).
           05 FILLER                     PIC X(02) VALUE SPACES.
           05 SEQ-OUT-REC                PIC X(550).
           05 SEQ-OUT-BY-AGE             REDEFINES SEQ-OUT-REC
                                         PIC X(54).
           05 SEQ-OUT-BY-ETHNICITY       REDEFINES SEQ-OUT-REC
                                         PIC X(44).
           05 SEQ-OUT-BY-INDUSTRY        REDEFINES SEQ-OUT-REC
                                         PIC X(550).
           05 SEQ-OUT-BY-RACE            REDEFINES SEQ-OUT-REC
                                         PIC X(118).
           05 SEQ-OUT-BY-GENDER          REDEFINES SEQ-OUT-REC
                                         PIC X(14).

       01  WS-OUT-SEQ-BY-AGE.
           05 CAT-AGE-LE-22           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-22-24           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-25-34           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-35-44           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-45-54           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-55-59           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-60-64           PIC X(05).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-AGE-GR-65           PIC X(05).

       01  WS-OUT-SEQ-BY-ETHNICITY.
           05 CAT-ETH-HIS-LAT         PIC X(05).
           05 FILLER                  PIC X(15) VALUE SPACES.
           05 CAT-ETH-NOT-HIS-LAT     PIC X(06).
           05 FILLER                  PIC X(16) VALUE SPACES.

       01  WS-OUT-SEQ-BY-INDUSTRY.
           05 CAT-IND-SALE-TRADE      PIC X(04).
           05 FILLER                  PIC X(13) VALUE SPACES.
           05 CAT-IND-TRANS           PIC X(05).
           05 FILLER                  PIC X(23) VALUE SPACES.
           05 CAT-IND-CONST           PIC X(05).
           05 FILLER                  PIC X(09) VALUE SPACES.
           05 CAT-IND-FINAN           PIC X(04).
           05 FILLER                  PIC X(17) VALUE SPACES.
           05 CAT-IND-MANUF           PIC X(05).
           05 FILLER                  PIC X(10) VALUE SPACES.
           05 CAT-IND-AGRIC           PIC X(03).
           05 FILLER                  PIC X(33) VALUE SPACES.
           05 CAT-IND-PUBLI           PIC X(04).
           05 FILLER                  PIC X(19) VALUE SPACES.
           05 CAT-IND-UTILI           PIC X(03).
           05 FILLER                  PIC X(08) VALUE SPACES.
           05 CAT-IND-ACCOM           PIC X(05).
           05 FILLER                  PIC X(26) VALUE SPACES.
           05 CAT-IND-INFOM           PIC X(04).
           05 FILLER                  PIC X(09) VALUE SPACES.
           05 CAT-IND-TECH-SERV       PIC X(04).
           05 FILLER                  PIC X(36) VALUE SPACES.
           05 CAT-IND-RENTAL          PIC X(04).
           05 FILLER                  PIC X(28) VALUE SPACES.
           05 CAT-IND-OTHER-SERV      PIC X(05).
           05 FILLER                  PIC X(42) VALUE SPACES.
           05 CAT-IND-MANAG           PIC X(04).
           05 FILLER                  PIC X(35) VALUE SPACES.
           05 CAT-IND-EDUC            PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.
           05 CAT-IND-MINING          PIC X(03).
           05 FILLER                  PIC X(05) VALUE SPACES.
           05 CAT-IND-HEALTH          PIC X(05).
           05 FILLER                  PIC X(28) VALUE SPACES.
           05 CAT-IND-ARTS-ENTER      PIC X(05).
           05 FILLER                  PIC X(29) VALUE SPACES.
           05 CAT-IND-ADMIN           PIC X(05).
           05 FILLER                  PIC X(40) VALUE SPACES.
           05 CAT-RETAIL-TRADE        PIC X(05).
           05 FILLER                  PIC X(07) VALUE SPACES.

       01  WS-OUT-SEQ-BY-RACE.
           05 CAT-RACE-WHITE          PIC X(6).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-RACE-ASIAN          PIC X(04).
           05 FILLER                  PIC X(03) VALUE SPACES.
           05 CAT-RACE-BLACK          PIC X(05).
           05 FILLER                  PIC X(22) VALUE SPACES.
           05 CAT-RACE-NATIVE         PIC X(4).
           05 FILLER                  PIC X(31) VALUE SPACES.
           05 CAT-RACE-NAT-HWAWIIAN   PIC X(4).
           05 FILLER                  PIC X(38) VALUE SPACES.

       01  WS-OUT-SEQ-BY-GENDER.
           05 CAT-GEN-FEMALE          PIC X(6).
           05 FILLER                  PIC X(02) VALUE SPACES.
           05 CAT-GEN-MALE            PIC X(6).

       01  WS-OUT-DIR-DATE-TIME-REC.
           05 FILLER                  PIC X(48) VALUE 'DATE : '.
           05 DIR-DATE-TIME           PIC X(19).
           05 FILLER                  PIC X(03) VALUE SPACES.

       01  WS-OUT-DIR-AGE-LE-22-REC.
           05 FILLER                  PIC X(48) VALUE '<22 : '.
           05 DIR-AGE-LE-22           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-22-24-REC.
           05 FILLER                  PIC X(48) VALUE '22-24 : '.
           05 DIR-AGE-22-24           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-25-34-REC.
           05 FILLER                  PIC X(48) VALUE '25-34 : '.
           05 DIR-AGE-25-34           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-35-44-REC.
           05 FILLER                  PIC X(48) VALUE '35-44 : '.
           05 DIR-AGE-35-44           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-45-54-REC.
           05 FILLER                  PIC X(48) VALUE '45-54 : '.
           05 DIR-AGE-45-54           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-55-59-REC.
           05 FILLER                  PIC X(48) VALUE '55-59 : '.
           05 DIR-AGE-55-59           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-60-64-REC.
           05 FILLER                  PIC X(48) VALUE '60-64 : '.
           05 DIR-AGE-60-64           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-AGE-GR-65-REC.
           05 FILLER                  PIC X(48) VALUE '>=65 : '.
           05 DIR-AGE-GR-65           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-ETH-HIS-LAT-REC.
           05 FILLER                  PIC X(48) VALUE
              'Hispanic or Latino : '.
           05 DIR-ETH-HIS-LAT         PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-ETH-NOT-HIS-LAT-REC.
           05 FILLER                  PIC X(48) VALUE
              'Not Hispanic or Latino : '.
           05 DIR-ETH-NOT-HIS-LAT     PIC X(06).
           05 FILLER                  PIC X(16) VALUE SPACES.

       01  WS-OUT-DIR-IND-SALE-TRADE-REC.
           05 FILLER                  PIC X(48) VALUE
              'Wholesale Trade : '.
           05 DIR-IND-SALE-TRADE      PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-TRANS-REC.
           05 FILLER                  PIC X(48) VALUE
              'Transportation & Warehouse : '.
           05 DIR-IND-TRANS           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-IND-CONST-REC.
           05 FILLER                  PIC X(48) VALUE
              'Construction : '.
           05 DIR-IND-CONST           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-IND-FINAN-REC.
           05 FILLER                  PIC X(48) VALUE
              'Finance & Insurance : '.
           05 DIR-IND-FINAN           PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-MANUF-REC.
           05 FILLER                  PIC X(48) VALUE
              'Manufacturing : '.
           05 DIR-IND-MANUF           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-IND-AGRIC-REC.
           05 FILLER                  PIC X(48) VALUE
              'Agricult./Forestry/Fishing/Hunting : '.
           05 DIR-IND-AGRIC           PIC X(03).
           05 FILLER                  PIC X(19) VALUE SPACES.

       01  WS-OUT-DIR-IND-PUBLI-REC.
           05 FILLER                  PIC X(48) VALUE
              'Public Administration : '.
           05 DIR-IND-PUBLI           PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-UTILI-REC.
           05 FILLER                  PIC X(48) VALUE
              'Utilities : '.
           05 DIR-IND-UTILI           PIC X(03).
           05 FILLER                  PIC X(19) VALUE SPACES.

       01  WS-OUT-DIR-IND-ACCOM-REC.
           05 FILLER                  PIC X(48) VALUE
              'Accomodation & Food Services : '.
           05 DIR-IND-ACCOM           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-IND-INFOM-REC.
           05 FILLER                  PIC X(48) VALUE
              'Information : '.
           05 DIR-IND-INFOM           PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-TECH-SERV-REC.
           05 FILLER                  PIC X(48) VALUE
              'Professional/Scientific/Tech. Services : '.
           05 DIR-IND-TECH-SERV       PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-RENTAL-REC.
           05 FILLER                  PIC X(48) VALUE
              'Real Estate & Rental & Leasing : '.
           05 DIR-IND-RENTAL          PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-OTHER-SERV-REC.
           05 FILLER                  PIC X(48) VALUE
              'Other Services (except Public Administration) : '.
           05 DIR-IND-OTHER-SERV      PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-IND-MANAG-REC.
           05 FILLER                  PIC X(48) VALUE
              'Management of Companies & Enterprises : '.
           05 DIR-IND-MANAG           PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-EDUC-REC.
           05 FILLER                  PIC X(48) VALUE
              'Educational Services : '.
           05 DIR-IND-EDUC            PIC X(04).
           05 FILLER                  PIC X(19) VALUE SPACES.

       01  WS-OUT-DIR-IND-MINING-REC.
           05 FILLER                  PIC X(48) VALUE 'Mining : '.
           05 DIR-IND-MINING          PIC X(03).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-IND-HEALTH-REC.
           05 FILLER                  PIC X(48) VALUE
              'Health Care & Social Assistance : '.
           05 DIR-IND-HEALTH          PIC X(05).
           05 FILLER                  PIC X(19) VALUE SPACES.

       01  WS-OUT-DIR-IND-ARTS-ENTER-REC.
           05 FILLER                  PIC X(48) VALUE
              'Arts, Entertainment & Recreation : '.
           05 DIR-IND-ARTS-ENTER      PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-IND-ADMIN-REC.
           05 FILLER                  PIC X(48) VALUE
              'Admin. & Support/Waste Mgmt./Remedia. Serv. : '.
           05 DIR-IND-ADMIN           PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-RETAIL-TRADE-REC.
           05 FILLER                  PIC X(48) VALUE
              'Retail Trade : '.
           05 DIR-RETAIL-TRADE        PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-RACE-WHITE-REC.
           05 FILLER                  PIC X(48) VALUE 'White : '.
           05 DIR-RACE-WHITE          PIC X(06).
           05 FILLER                  PIC X(16) VALUE SPACES.

       01  WS-OUT-DIR-RACE-ASIAN-REC.
           05 FILLER                  PIC X(48) VALUE 'Asian : '.
           05 DIR-RACE-ASIAN          PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-RACE-BLACK-REC.
           05 FILLER                  PIC X(48) VALUE
              'Black or African American : '.
           05 DIR-RACE-BLACK          PIC X(05).
           05 FILLER                  PIC X(17) VALUE SPACES.

       01  WS-OUT-DIR-RACE-NATIVE-REC.
           05 FILLER                  PIC X(48) VALUE
              'American Indian or Alaskan Native : '.
           05 DIR-RACE-NATIVE         PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-RACE-NAT-HWA-REC.
           05 FILLER                  PIC X(48) VALUE
              'Native Hawaiian or Other Pacific Islander : '.
           05 DIR-RACE-NAT-HWAWIIAN   PIC X(04).
           05 FILLER                  PIC X(18) VALUE SPACES.

       01  WS-OUT-DIR-GEN-FEMALE-REC.
           05 FILLER                  PIC X(48) VALUE 'FEMALE : '.
           05 DIR-GEN-FEMALE          PIC X(06).
           05 FILLER                  PIC X(16) VALUE SPACES.

       01  WS-OUT-DIR-GEN-MALE-REC.
           05 FILLER                  PIC X(48) VALUE 'MALE : '.
           05 DIR-GEN-MALE            PIC X(06).
           05 FILLER                  PIC X(16) VALUE SPACES.

       01  UNSTRING-CLAIM-REC.
           05 UCR-DATE-TIME              PIC X(19).
           05 UCR-BY-AGE.
              10 UCR-AGE-INA             PIC X(02).
              10 UCR-AGE-LE-22           PIC X(05).
              10 UCR-AGE-22-24           PIC X(05).
              10 UCR-AGE-25-34           PIC X(05).
              10 UCR-AGE-35-44           PIC X(05).
              10 UCR-AGE-45-54           PIC X(05).
              10 UCR-AGE-55-59           PIC X(05).
              10 UCR-AGE-60-64           PIC X(05).
              10 UCR-AGE-GR-65           PIC X(05).
           05 UCR-BY-ETHNICITY.
              10 UCR-ETH-INA             PIC X(05).
              10 UCR-ETH-HIS-LAT         PIC X(05).
              10 UCR-ETH-NOT-HIS-LAT     PIC X(06).
           05 UCR-BY-INDUSTRY.
              10 UCR-IND-INA             PIC X(04).
              10 UCR-IND-SALE-TRADE      PIC X(04).
              10 UCR-IND-TRANS           PIC X(05).
              10 UCR-IND-CONST           PIC X(05).
              10 UCR-IND-FINAN           PIC X(04).
              10 UCR-IND-MANUF           PIC X(05).
              10 UCR-IND-AGRIC           PIC X(03).
              10 UCR-IND-PUBLI           PIC X(04).
              10 UCR-IND-UTILI           PIC X(03).
              10 UCR-IND-ACCOM           PIC X(05).
              10 UCR-IND-INFOM           PIC X(04).
              10 UCR-IND-TECH-SERV       PIC X(04).
              10 UCR-IND-RENTAL          PIC X(04).
              10 UCR-IND-OTHER-SERV      PIC X(05).
              10 UCR-IND-MANAG           PIC X(04).
              10 UCR-IND-EDUC            PIC X(04).
              10 UCR-IND-MINING          PIC X(03).
              10 UCR-IND-HEALTH          PIC X(05).
              10 UCR-IND-ARTS-ENTER      PIC X(05).
              10 UCR-IND-ADMIN           PIC X(05).
              10 UCR-RETAIL-TRADE        PIC X(05).
           05 UCR-BY-RACE.
              10 UCR-RACE-INA            PIC X(05).
              10 UCR-RACE-WHITE          PIC X(06).
              10 UCR-RACE-ASIAN          PIC X(04).
              10 UCR-RACE-BLACK          PIC X(05).
              10 UCR-RACE-NATIVE         PIC X(04).
              10 UCR-RACE-NAT-HWAWIIAN   PIC X(04).
           05 UCR-BY-GENDER.
              10 UCR-GEN-INA             PIC X(02).
              10 UCR-GEN-FEMALE          PIC X(06).
              10 UCR-GEN-MALE            PIC X(06).

      *------------------
       PROCEDURE DIVISION.
      *------------------

       0000-MAIN-PARA.
      *--------------*
           SET   C-STATUS-OK TO TRUE.      

           PERFORM 1000-OPEN-FILE        
              THRU 1000-OPEN-FILE-EXIT.

           PERFORM 2000-ACCEPT-INPUT     
              THRU 2000-ACCEPT-INPUT-EXIT.

           PERFORM VARYING ACCEPT-SUB FROM 1 BY 1
                   UNTIL C-CMD-EOF OR C-STATUS-ABEND
                   OR ACCEPT-SUB > 200

              PERFORM 3000-GET-CLAIMS 
                 THRU 3000-GET-CLAIMS-EXIT
      
              IF C-STATUS-OK THEN
                 PERFORM 4000-PRINT-HEADER  
                    THRU 4000-PRINT-HEADER-EXIT
      
                 PERFORM  VARYING SUB1 FROM 1 BY 1
                          UNTIL SUB1 > WS-NUM-OF-RECS OR C-STATUS-ABEND

                     PERFORM 5000-PARSE-RECORD
                        THRU 5000-PARSE-RECORD-EXIT

                     PERFORM 6000-PRINT-RECORD
                        THRU 6000-PRINT-RECORD-EXIT
                 END-PERFORM
      
                 PERFORM 7000-PRINT-SEPARATOR
                    THRU 7000-PRINT-SEPARATOR-EXIT
                                         
                 PERFORM 2000-ACCEPT-INPUT  
                    THRU 2000-ACCEPT-INPUT-EXIT 

                 IF NOT C-CMD-EOF AND C-STATUS-OK THEN
                    PERFORM 8000-WRITE-EMPTY-LN
                       THRU 8000-WRITE-EMPTY-LN-EXIT          
                 END-IF                       
              END-IF   
           END-PERFORM.

           PERFORM 9000-CLOSE-FILE      
              THRU 9000-CLOSE-FILE-EXIT.

           GOBACK.
      *

       1000-OPEN-FILE.
      *--------------*
           OPEN OUTPUT OUT001.     
       1000-OPEN-FILE-EXIT.
      *-------------------*
           EXIT.

       2000-ACCEPT-INPUT.
      *-----------------*
           ACCEPT   WS-INPUT-REC   FROM  SYSIN.
           
           MOVE     WS-INP-CMD     TO    WS-SEARCH-CMD.
           MOVE     WS-INP-ID      TO    WS-SEARCH-ID.

      D    DISPLAY 'WS-INPUT-REC ' WS-INPUT-REC.        
           
           EVALUATE TRUE
              WHEN C-CMD-READ
                 PERFORM 2100-ACCEPT-READ-INPUT
                    THRU 2100-ACCEPT-READ-INPUT-EXIT

              WHEN C-CMD-DELETE
                 PERFORM 2200-ACCEPT-DELETE-INPUT
                    THRU 2200-ACCEPT-DELETE-INPUT-EXIT

              WHEN C-CMD-INSERT OR C-CMD-UPDATE
                 PERFORM 2300-ACCEPT-OTHER-INPUT
                    THRU 2300-ACCEPT-OTHER-INPUT-EXIT

              WHEN C-CMD-EOF
                 DISPLAY 'REACH TO END OF FILE'      

              WHEN OTHER
                 DISPLAY 'WRONG COMMAND SPECIFIED'
                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.

       2000-ACCEPT-INPUT-EXIT.
      *----------------------*
           EXIT.


       2100-ACCEPT-READ-INPUT.
      *----------------------*
           MOVE  WS-INP-NUM-OF-RECS   TO WS-NUM-OF-RECS.

           IF WS-NUM-OF-RECS = 1 THEN
              SET   C-OUT-DIR-REC  TO TRUE 

           ELSE IF WS-NUM-OF-RECS > 1 THEN
              EVALUATE TRUE
                  WHEN C-BY-AGE
                     SET  C-BY-AGE-OUT-REC        TO TRUE

                  WHEN C-BY-ETHNICITY
                     SET  C-BY-ETHNICITY-OUT-REC  TO TRUE

                  WHEN C-BY-INDUSTRY
                     SET  C-BY-INDUSTRY-OUT-REC   TO TRUE

                  WHEN C-BY-RACE
                     SET  C-BY-RACE-OUT-REC       TO TRUE

                  WHEN C-BY-GENDER
                     SET  C-BY-GENDER-OUT-REC     TO TRUE

                  WHEN OTHER
                     DISPLAY 'WRONG CATEGORY SPECIFIED'

                     PERFORM 9999-ABEND-PARA 
                        THRU 9999-ABEND-PARA-EXIT
              END-EVALUATE
           ELSE
              DISPLAY 'NUMBER OF RECORDS SHOULD BE GREATER THAN ZERO : '
                       WS-NUM-OF-RECS

              PERFORM 9999-ABEND-PARA 
                 THRU 9999-ABEND-PARA-EXIT
           END-IF.
       2100-ACCEPT-READ-INPUT-EXIT.
      *---------------------------*
           EXIT.

       2200-ACCEPT-DELETE-INPUT.
      *------------------------*
           SET   C-OUT-DIR-REC  TO TRUE.
           MOVE  1              TO WS-NUM-OF-RECS. 
       2200-ACCEPT-DELETE-INPUT-EXIT.
      *-----------------------------*
           EXIT.

       2300-ACCEPT-OTHER-INPUT.
      *-----------------------*
           SET   C-OUT-DIR-REC  TO TRUE.
           MOVE  1              TO WS-NUM-OF-RECS.      
           MOVE  WS-INP-DATA    TO WS-SEARCH-CLAIMS.      
       2300-ACCEPT-OTHER-INPUT-EXIT.
      *----------------------------*
           EXIT.


       3000-GET-CLAIMS.
      *---------------*
      D    DISPLAY 'WS-SEARCH-DATA ' WS-SEARCH-DATA    
      D    DISPLAY 'WS-SEARCH-REC ' WS-SEARCH-CLAIMS.          
      D    DISPLAY 'WS-NUM-OF-RECS ' WS-NUM-OF-RECS
      
           CALL  'GETCLAIM'  USING BY CONTENT     WS-SEARCH-DATA,
                                   BY REFERENCE   WS-NUM-OF-RECS,               
                                   BY REFERENCE   WS-RETURN-DATA, 
                                   BY REFERENCE   WS-RETURN-CODE.              
       3000-GET-CLAIMS-EXIT.
      *--------------------*
           EXIT.

       4000-PRINT-HEADER.
      *-----------------*
           IF WS-NUM-OF-RECS = 1 THEN
              PERFORM 4100-WRITE-DIR-HEADERS
                 THRU 4100-WRITE-DIR-HEADERS-EXIT
           ELSE IF WS-NUM-OF-RECS > 1 THEN
              PERFORM 4300-WRITE-SEQ-HEADERS
                 THRU 4300-WRITE-SEQ-HEADERS-EXIT.
       4000-PRINT-HEADER-EXIT.
      *----------------------*
           EXIT.

       4100-WRITE-DIR-HEADERS.
      *----------------------*
           EVALUATE TRUE
              WHEN C-CMD-READ
                 WRITE OUTPUT-REC  FROM HEADER-1-DIR-READ

              WHEN C-CMD-DELETE
                 WRITE OUTPUT-REC  FROM HEADER-1-DIR-DELETE

              WHEN C-CMD-INSERT 
                 WRITE OUTPUT-REC  FROM HEADER-1-DIR-INSERT

              WHEN C-CMD-UPDATE
                 WRITE OUTPUT-REC  FROM HEADER-1-DIR-UPDATE

              WHEN OTHER
                 DISPLAY 'WRONG COMMAND SPECIFIED'

                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.

           PERFORM 4200-WRITE-HEADER-2
              THRU 4200-WRITE-HEADER-2-EXIT.

           MOVE  WS-INP-ID         TO DIR-RECORD-ID.
           WRITE OUTPUT-REC        FROM HEADER-3-DIR.

           PERFORM 7000-PRINT-SEPARATOR
              THRU 7000-PRINT-SEPARATOR-EXIT.
       4100-WRITE-DIR-HEADERS-EXIT.
      *---------------------------*
           EXIT.

       4200-WRITE-HEADER-2.
      *-------------------*
           MOVE FUNCTION CURRENT-DATE(1:8)
                                   TO WS-CURRENT-DATE-REC.
           MOVE WS-CURRENT-YEAR    TO HDR-YR.
           MOVE WS-CURRENT-MONTH   TO HDR-MO.
           MOVE WS-CURRENT-DAY     TO HDR-DAY.

           WRITE OUTPUT-REC        FROM HEADER-2.
       4200-WRITE-HEADER-2-EXIT.
      *------------------------*
           EXIT.

       4300-WRITE-SEQ-HEADERS.
      *----------------------*
           PERFORM 4310-WRITE-SEQ-HEADER-1
              THRU 4310-WRITE-SEQ-HEADER-1-EXIT.

           PERFORM 4200-WRITE-HEADER-2
              THRU 4200-WRITE-HEADER-2-EXIT.

           PERFORM 4320-WRITE-SEQ-HEADER-3
              THRU 4320-WRITE-SEQ-HEADER-3-EXIT.

           PERFORM 7000-PRINT-SEPARATOR
              THRU 7000-PRINT-SEPARATOR-EXIT.
       4300-WRITE-SEQ-HEADERS-EXIT.
      *---------------------------*
           EXIT.

       4310-WRITE-SEQ-HEADER-1.
      *-----------------------*           
           EVALUATE TRUE
              WHEN C-BY-AGE
                 WRITE OUTPUT-REC  FROM HEADER-1-SEQ-BY-AGE

              WHEN C-BY-ETHNICITY
                 WRITE OUTPUT-REC  FROM HEADER-1-SEQ-BY-ETHNICITY

              WHEN C-BY-INDUSTRY
                 WRITE OUTPUT-REC  FROM HEADER-1-SEQ-BY-INDUSTRY

              WHEN C-BY-RACE
                 WRITE OUTPUT-REC  FROM HEADER-1-SEQ-BY-RACE

              WHEN C-BY-GENDER
                 WRITE OUTPUT-REC  FROM HEADER-1-SEQ-BY-GENDER

              WHEN OTHER
                 DISPLAY 'WRONG CATEGORY SPECIFIED'

                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.
       4310-WRITE-SEQ-HEADER-1-EXIT.
      *----------------------------*
           EXIT.

       4320-WRITE-SEQ-HEADER-3.
      *-----------------------*
           EVALUATE TRUE
              WHEN C-BY-AGE
                 MOVE WS-HEADER-3-SEQ-BY-AGE
                                         TO HEADER-3-SEQ-BY-AGE

              WHEN C-BY-ETHNICITY
                 MOVE WS-HEADER-3-SEQ-BY-ETHNICITY
                                         TO HEADER-3-SEQ-BY-ETHNICITY

              WHEN C-BY-INDUSTRY
                 MOVE WS-HEADER-3-SEQ-BY-INDUSTRY
                                         TO HEADER-3-SEQ-BY-INDUSTRY

              WHEN C-BY-RACE
                 MOVE WS-HEADER-3-SEQ-BY-RACE
                                         TO HEADER-3-SEQ-BY-RACE

              WHEN C-BY-GENDER
                 MOVE WS-HEADER-3-SEQ-BY-GENDER
                                         TO HEADER-3-SEQ-BY-GENDER

              WHEN OTHER
                 DISPLAY 'WRONG CATEGORY SPECIFIED'

                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.

           WRITE OUTPUT-REC              FROM HEADER-3-SEQ.
       4320-WRITE-SEQ-HEADER-3-EXIT.
      *----------------------------*
           EXIT.

       5000-PARSE-RECORD.
      *-----------------*
           MOVE ALL SPACES               TO UNSTRING-CLAIM-REC.

           UNSTRING WS-RETURN-REC (SUB1)  DELIMITED BY ','
           INTO UCR-DATE-TIME, UCR-AGE-INA, UCR-AGE-LE-22,
           UCR-AGE-22-24, UCR-AGE-25-34, UCR-AGE-35-44, UCR-AGE-45-54,
           UCR-AGE-55-59, UCR-AGE-60-64, UCR-AGE-GR-65, UCR-ETH-INA,
           UCR-ETH-HIS-LAT, UCR-ETH-NOT-HIS-LAT, UCR-IND-INA,
           UCR-IND-SALE-TRADE, UCR-IND-TRANS, UCR-IND-CONST,
           UCR-IND-FINAN, UCR-IND-MANUF, UCR-IND-AGRIC, UCR-IND-PUBLI,
           UCR-IND-UTILI, UCR-IND-ACCOM, UCR-IND-INFOM,
           UCR-IND-TECH-SERV, UCR-IND-RENTAL, UCR-IND-OTHER-SERV,
           UCR-IND-MANAG, UCR-IND-EDUC, UCR-IND-MINING, UCR-IND-HEALTH,
           UCR-IND-ARTS-ENTER, UCR-IND-ADMIN, UCR-RETAIL-TRADE,
           UCR-RACE-INA, UCR-RACE-WHITE, UCR-RACE-ASIAN, UCR-RACE-BLACK,
            UCR-RACE-NATIVE, UCR-RACE-NAT-HWAWIIAN, UCR-GEN-INA,
            UCR-GEN-FEMALE, UCR-GEN-MALE
           END-UNSTRING.
       5000-PARSE-RECORD-EXIT.
      *----------------------*
           EXIT.

       6000-PRINT-RECORD.
      *-----------------*
           IF WS-NUM-OF-RECS = 1   THEN
                PERFORM 6100-PRINT-DIR-RECORD
                   THRU 6100-PRINT-DIR-RECORD-EXIT
           ELSE IF WS-NUM-OF-RECS > 1 THEN
                PERFORM 6200-PRINT-SEQ-RECORD
                   THRU 6200-PRINT-SEQ-RECORD-EXIT
           END-IF.
       6000-PRINT-RECORD-EXIT.
      *----------------------*
           EXIT.

       6100-PRINT-DIR-RECORD.
      *---------------------*
           PERFORM 6110-MOVE-DIR-RECORD
              THRU 6110-MOVE-DIR-RECORD-EXIT.
           PERFORM 6120-WRITE-DIR-RECORD
              THRU 6120-WRITE-DIR-RECORD-EXIT.
       6100-PRINT-DIR-RECORD-EXIT.
      *--------------------------*
           EXIT.

       6110-MOVE-DIR-RECORD.
      *---------------------*
           MOVE UCR-DATE-TIME         TO DIR-DATE-TIME.
           MOVE UCR-AGE-LE-22         TO DIR-AGE-LE-22.
           MOVE UCR-AGE-22-24         TO DIR-AGE-22-24.
           MOVE UCR-AGE-25-34         TO DIR-AGE-25-34.
           MOVE UCR-AGE-35-44         TO DIR-AGE-35-44.
           MOVE UCR-AGE-45-54         TO DIR-AGE-45-54.
           MOVE UCR-AGE-55-59         TO DIR-AGE-55-59.
           MOVE UCR-AGE-60-64         TO DIR-AGE-60-64.
           MOVE UCR-AGE-GR-65         TO DIR-AGE-GR-65.
           MOVE UCR-ETH-HIS-LAT       TO DIR-ETH-HIS-LAT.
           MOVE UCR-ETH-NOT-HIS-LAT   TO DIR-ETH-NOT-HIS-LAT.
           MOVE UCR-IND-SALE-TRADE    TO DIR-IND-SALE-TRADE.
           MOVE UCR-IND-TRANS         TO DIR-IND-TRANS.
           MOVE UCR-IND-CONST         TO DIR-IND-CONST.
           MOVE UCR-IND-FINAN         TO DIR-IND-FINAN.
           MOVE UCR-IND-MANUF         TO DIR-IND-MANUF.
           MOVE UCR-IND-AGRIC         TO DIR-IND-AGRIC.
           MOVE UCR-IND-PUBLI         TO DIR-IND-PUBLI.
           MOVE UCR-IND-UTILI         TO DIR-IND-UTILI.
           MOVE UCR-IND-ACCOM         TO DIR-IND-ACCOM.
           MOVE UCR-IND-INFOM         TO DIR-IND-INFOM.
           MOVE UCR-IND-TECH-SERV     TO DIR-IND-TECH-SERV.
           MOVE UCR-IND-RENTAL        TO DIR-IND-RENTAL.
           MOVE UCR-IND-OTHER-SERV    TO DIR-IND-OTHER-SERV.
           MOVE UCR-IND-MANAG         TO DIR-IND-MANAG.
           MOVE UCR-IND-EDUC          TO DIR-IND-EDUC.
           MOVE UCR-IND-MINING        TO DIR-IND-MINING.
           MOVE UCR-IND-HEALTH        TO DIR-IND-HEALTH.
           MOVE UCR-IND-ARTS-ENTER    TO DIR-IND-ARTS-ENTER.
           MOVE UCR-IND-ADMIN         TO DIR-IND-ADMIN.
           MOVE UCR-RETAIL-TRADE      TO DIR-RETAIL-TRADE.
           MOVE UCR-RACE-WHITE        TO DIR-RACE-WHITE.
           MOVE UCR-RACE-ASIAN        TO DIR-RACE-ASIAN.
           MOVE UCR-RACE-BLACK        TO DIR-RACE-BLACK.
           MOVE UCR-RACE-NATIVE       TO DIR-RACE-NATIVE.
           MOVE UCR-RACE-NAT-HWAWIIAN TO DIR-RACE-NAT-HWAWIIAN.
           MOVE UCR-GEN-FEMALE        TO DIR-GEN-FEMALE.
           MOVE UCR-GEN-MALE          TO DIR-GEN-MALE.
       6110-MOVE-DIR-RECORD-EXIT.
      *-------------------------*
           EXIT.

       6120-WRITE-DIR-RECORD.
      *----------------------*
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-DATE-TIME-REC.

           MOVE  WS-BY-AGE         TO DIR-SEPARATOR-BY-CAT.
           WRITE OUTPUT-REC        FROM WS-DIR-SEPARATOR-BY-SEQ-REC.

           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-LE-22-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-22-24-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-25-34-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-35-44-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-45-54-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-55-59-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-60-64-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-AGE-GR-65-REC.

           MOVE  WS-BY-ETHNICITY   TO DIR-SEPARATOR-BY-CAT.
           WRITE OUTPUT-REC        FROM WS-DIR-SEPARATOR-BY-SEQ-REC.

           WRITE OUTPUT-REC        FROM WS-OUT-DIR-ETH-HIS-LAT-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-ETH-NOT-HIS-LAT-REC.

           MOVE  WS-BY-INDUSTRY    TO DIR-SEPARATOR-BY-CAT.
           WRITE OUTPUT-REC        FROM WS-DIR-SEPARATOR-BY-SEQ-REC.

           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-SALE-TRADE-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-TRANS-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-CONST-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-FINAN-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-MANUF-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-AGRIC-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-PUBLI-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-UTILI-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-ACCOM-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-INFOM-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-TECH-SERV-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-RENTAL-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-OTHER-SERV-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-MANAG-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-EDUC-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-MINING-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-HEALTH-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-ARTS-ENTER-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-IND-ADMIN-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-RETAIL-TRADE-REC.

           MOVE  WS-BY-RACE        TO DIR-SEPARATOR-BY-CAT.
           WRITE OUTPUT-REC        FROM WS-DIR-SEPARATOR-BY-SEQ-REC.

           WRITE OUTPUT-REC        FROM WS-OUT-DIR-RACE-WHITE-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-RACE-ASIAN-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-RACE-BLACK-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-RACE-NATIVE-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-RACE-NAT-HWA-REC.

           MOVE  WS-BY-GENDER      TO DIR-SEPARATOR-BY-CAT.
           WRITE OUTPUT-REC        FROM WS-DIR-SEPARATOR-BY-SEQ-REC.

           WRITE OUTPUT-REC        FROM WS-OUT-DIR-GEN-FEMALE-REC.
           WRITE OUTPUT-REC        FROM WS-OUT-DIR-GEN-MALE-REC.
       6120-WRITE-DIR-RECORD-EXIT.
      *--------------------------*
           EXIT.

       6200-PRINT-SEQ-RECORD.
      *---------------------*
           PERFORM 6220-MOVE-SEQ-RECORD
              THRU 6220-MOVE-SEQ-RECORD-EXIT.
           PERFORM 6230-WRITE-SEQ-RECORD
              THRU 6230-WRITE-SEQ-RECORD-EXIT.
       6200-PRINT-SEQ-RECORD-EXIT.
      *--------------------------*
           EXIT.

       6220-MOVE-SEQ-RECORD.
      *--------------------*
           MOVE ALL SPACES                  TO SEQ-OUT-REC.
           MOVE WS-RETURN-ID (SUB1)         TO SEQ-OUT-RECORD-ID.
           MOVE UCR-DATE-TIME               TO SEQ-OUT-DATE-TIME.

           EVALUATE TRUE
              WHEN C-BY-AGE
                 MOVE UCR-AGE-LE-22         TO CAT-AGE-LE-22
                 MOVE UCR-AGE-22-24         TO CAT-AGE-22-24
                 MOVE UCR-AGE-25-34         TO CAT-AGE-25-34
                 MOVE UCR-AGE-35-44         TO CAT-AGE-35-44
                 MOVE UCR-AGE-45-54         TO CAT-AGE-45-54
                 MOVE UCR-AGE-55-59         TO CAT-AGE-55-59
                 MOVE UCR-AGE-60-64         TO CAT-AGE-60-64
                 MOVE UCR-AGE-GR-65         TO CAT-AGE-GR-65

                 MOVE WS-OUT-SEQ-BY-AGE     TO SEQ-OUT-BY-AGE

              WHEN C-BY-ETHNICITY
                 MOVE UCR-ETH-HIS-LAT       TO CAT-ETH-HIS-LAT
                 MOVE UCR-ETH-NOT-HIS-LAT   TO CAT-ETH-NOT-HIS-LAT

                 MOVE WS-OUT-SEQ-BY-ETHNICITY
                                            TO SEQ-OUT-BY-ETHNICITY

              WHEN C-BY-INDUSTRY
                 MOVE UCR-IND-SALE-TRADE    TO CAT-IND-SALE-TRADE
                 MOVE UCR-IND-TRANS         TO CAT-IND-TRANS
                 MOVE UCR-IND-CONST         TO CAT-IND-CONST
                 MOVE UCR-IND-FINAN         TO CAT-IND-FINAN
                 MOVE UCR-IND-MANUF         TO CAT-IND-MANUF
                 MOVE UCR-IND-AGRIC         TO CAT-IND-AGRIC
                 MOVE UCR-IND-PUBLI         TO CAT-IND-PUBLI
                 MOVE UCR-IND-UTILI         TO CAT-IND-UTILI
                 MOVE UCR-IND-ACCOM         TO CAT-IND-ACCOM
                 MOVE UCR-IND-INFOM         TO CAT-IND-INFOM
                 MOVE UCR-IND-TECH-SERV     TO CAT-IND-TECH-SERV
                 MOVE UCR-IND-RENTAL        TO CAT-IND-RENTAL
                 MOVE UCR-IND-OTHER-SERV    TO CAT-IND-OTHER-SERV
                 MOVE UCR-IND-MANAG         TO CAT-IND-MANAG
                 MOVE UCR-IND-EDUC          TO CAT-IND-EDUC
                 MOVE UCR-IND-MINING        TO CAT-IND-MINING
                 MOVE UCR-IND-HEALTH        TO CAT-IND-HEALTH
                 MOVE UCR-IND-ARTS-ENTER    TO CAT-IND-ARTS-ENTER
                 MOVE UCR-IND-ADMIN         TO CAT-IND-ADMIN
                 MOVE UCR-RETAIL-TRADE      TO CAT-RETAIL-TRADE

                 MOVE WS-OUT-SEQ-BY-INDUSTRY
                                            TO SEQ-OUT-BY-INDUSTRY

              WHEN C-BY-RACE
                 MOVE UCR-RACE-WHITE        TO CAT-RACE-WHITE
                 MOVE UCR-RACE-ASIAN        TO CAT-RACE-ASIAN
                 MOVE UCR-RACE-BLACK        TO CAT-RACE-BLACK
                 MOVE UCR-RACE-NATIVE       TO CAT-RACE-NATIVE
                 MOVE UCR-RACE-NAT-HWAWIIAN TO CAT-RACE-NAT-HWAWIIAN

                 MOVE WS-OUT-SEQ-BY-RACE    TO SEQ-OUT-BY-RACE

              WHEN C-BY-GENDER
                 MOVE UCR-GEN-FEMALE        TO CAT-GEN-FEMALE
                 MOVE UCR-GEN-MALE          TO CAT-GEN-MALE

                 MOVE WS-OUT-SEQ-BY-GENDER  TO SEQ-OUT-BY-GENDER

              WHEN OTHER
                 DISPLAY 'WRONG CATEGORY SPECIFIED'

                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.
       6220-MOVE-SEQ-RECORD-EXIT.
      *--------------------------*
           EXIT.


       6230-WRITE-SEQ-RECORD.
      *----------------------*
           WRITE OUTPUT-REC  FROM WS-SEQ-OUTPUT.
       6230-WRITE-SEQ-RECORD-EXIT.
      *---------------------------*
           EXIT.

       7000-PRINT-SEPARATOR.
      *--------------------* 
           IF WS-NUM-OF-RECS = 1   THEN
              PERFORM 7100-WRITE-DIR-SEPARATOR
                 THRU 7100-WRITE-DIR-SEPARATOR-EXIT
           ELSE IF WS-NUM-OF-RECS > 1 THEN
              PERFORM 7200-WRITE-SEQ-SEPARATOR
                 THRU 7200-WRITE-SEQ-SEPARATOR-EXIT
           END-IF.
       7000-PRINT-SEPARATOR-EXIT.
      *-------------------------* 
           EXIT.


       7100-WRITE-DIR-SEPARATOR.
      *------------------------*
           MOVE WS-DIR-SEPARATOR   TO OUTPUT-REC.
           WRITE OUTPUT-REC.
       7100-WRITE-DIR-SEPARATOR-EXIT.
      *-----------------------------*
           EXIT.


       7200-WRITE-SEQ-SEPARATOR.
      *------------------------*
           EVALUATE TRUE
              WHEN C-BY-AGE
                 MOVE WS-SEQ-SEPARATOR-BY-AGE
                                   TO SEQ-SEPARATOR-BY-AGE

              WHEN C-BY-ETHNICITY
                 MOVE WS-SEQ-SEPARATOR-BY-ETHNICITY
                                   TO SEQ-SEPARATOR-BY-ETHNICITY

              WHEN C-BY-INDUSTRY
                 MOVE WS-SEQ-SEPARATOR-BY-INDUSTRY
                                   TO SEQ-SEPARATOR-BY-INDUSTRY

              WHEN C-BY-RACE
                 MOVE WS-SEQ-SEPARATOR-BY-RACE
                                   TO SEQ-SEPARATOR-BY-RACE

              WHEN C-BY-GENDER
                 MOVE WS-SEQ-SEPARATOR-BY-GENDER
                                   TO SEQ-SEPARATOR-BY-GENDER

              WHEN OTHER
                 DISPLAY 'WRONG CATEGORY SPECIFIED'

                 PERFORM 9999-ABEND-PARA 
                    THRU 9999-ABEND-PARA-EXIT
           END-EVALUATE.

           WRITE OUTPUT-REC  FROM WS-SEQ-SEPARATOR.
       7200-WRITE-SEQ-SEPARATOR-EXIT.
      *-----------------------------*
           EXIT.

       8000-WRITE-EMPTY-LN.
      *-------------------* 
           WRITE OUTPUT-REC  FROM WS-DIR-EMPTY-LN.
           WRITE OUTPUT-REC  FROM WS-DIR-EMPTY-LN.           
       8000-WRITE-EMPTY-LN-EXIT.
      *------------------------* 
           EXIT.
      
       9000-CLOSE-FILE.
      *---------------*
           CLOSE OUT001.
       9000-CLOSE-FILE-EXIT.
      *--------------------*
           EXIT.

       9999-ABEND-PARA.
      *---------------*
           SET   C-STATUS-ABEND TO TRUE.
       9999-ABEND-PARA-EXIT.
      *--------------------*
           EXIT.
