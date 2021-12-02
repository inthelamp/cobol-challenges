//COVIDRPJ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COVIDRPT),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COVIDRPT),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=COVIDRPT
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//INDD01    DD DSN=&SYSUID..DATA,DISP=SHR
//OUTCOVID  DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
