//SMPJOB05 JOB 'RECEIVE USERMODS',                                      00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
/*JOBPARM   LINES=100                                                   00030000
//*                                                                     00040000
//* ***************************************************************** * 00050000
//* RECEIVE JIM MORRISON'S 3375+3380+3390 USERMODS                    * 00060000
//* ***************************************************************** * 00070000
//*                                                                     00080000
//DLBUCL EXEC DLBSMP,TIME.SMP=1439                                      00090000
//SMPCNTL  DD  *                                                        00100000
  RECEIVE .                                                             00110000
//SMPPTFIN  DD UNIT=TAPE,VOL=SER=J90009,DISP=(OLD,PASS),DSN=SMPMCS,     00120000
//             LABEL=(1,SL)                                             00130000
//                                                                      00140000
