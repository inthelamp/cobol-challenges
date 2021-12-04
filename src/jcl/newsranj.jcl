//NEWSRANJ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(NEWSRANK),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(NEWSRANK),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=NEWSRANK
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//INDD01    DD DSN=ZOS.PUBLIC.HACKER.NEWS,DISP=SHR
//OUDD01    DD DSN=&&TEMPDS,DISP=(NEW,PASS,DELETE),
//             DCB=(RECFM=FB,LRECL=137,BLKSIZE=0),
//             SPACE=(TRK,15),UNIT=SYSDA
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//OUTFIL  EXEC PGM=SORT                                      
//SYSOUT    DD SYSOUT=*                                         
//SORTIN    DD DSN=&&TEMPDS,DISP=(OLD,DELETE,DELETE)
//HNRANK    DD SYSOUT=A,
//             DCB=(LRECL=155,BLKSIZE=0)
//SYSIN     DD *
    SORT FIELDS=(132,6,PD,D)
    OUTFIL FNAMES=HNRANK,LINES=44,BLKCCH2,

    HEADER1=(56:'Hacker News Front Page    ',
             82:DATE=(MD4/),
             92:' at 23:59',/,
             56:'All Mainframe/COBOL stories'),

    HEADER2=(1:'--------------------------------------------------',
            51:'--------------------------------------------------',
           101:'--------------------------------------------------',
           151:'----',/,
             4:'ID',57:'TITLE',108:'Points',115:'Comments',
             124:'Author',139:'Time',147:'Score',/,
             1:'--------------------------------------------------',
            51:'--------------------------------------------------',
           101:'--------------------------------------------------',
           151:'----'),

    TRAILER2=(1:'--------------------------------------------------',
             51:'--------------------------------------------------',
            101:'--------------------------------------------------',
            151:'----'),

    OUTREC=(1,8,2X,9,95,2X,104,4,ZD,TO=FS,LENGTH=4,3X,108,4,
            ZD,TO=FS,LENGTH=4,5X,112,15,127,2,ZD,TO=FS,LENGTH=2,
            129,3,3X,132,6,PD,EDIT=(T.TTTTTT))
/*
// ELSE
// ENDIF
// ELSE
// ENDIF