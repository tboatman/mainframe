//SMPJOB01 JOB (SYSGEN),'RECEIVE PRODUCT TAPE',                         00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
/*JOBPARM   LINES=100                                                   00030000
//*                                                                     00040000
//* ***************************************************************** * 00050000
//* RECEIVE MVS COMPONENTS FROM DISTRIBUTION TAPE INTO SMP DATASETS   * 00060000
//* ***************************************************************** * 00070000
//*                                                                     00080000
//DLBUCL   EXEC DLBSMP,TIME.SMP=1439                                    00090000
//SMPCNTL   DD *                                                        00100000
  RECEIVE .                                                             00110000
//SMPPTFIN  DD UNIT=TAPE,VOL=SER=MVS38J,DISP=(OLD,PASS),DSN=SMPMCS,     00120000
//             LABEL=(1,SL)                                             00130000
//                                                                      00140000