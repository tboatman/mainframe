//SMPJOB02 JOB 'RECEIVE ALL PTFS',                                      00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
/*JOBPARM   LINES=100                                                   00030000
//*                                                                     00040000
//* ***************************************************************** * 00050000
//* RECEIVE PTFS TO APPLY TO MVS ELEMENTS                             * 00060000
//* ***************************************************************** * 00070000
//*                                                                     00080000
//DLBUCL EXEC DLBSMP                                                    00090000
//SMPCNTL  DD  *                                                        00100000
  RECEIVE .                                                             00110000
//SMPPTFIN DD  DSN=PTFS,                                                00120000
//             UNIT=(TAPE,,DEFER),                                      00130000
//             DISP=OLD,                                                00140000
//             LABEL=(,SL),VOL=SER=MVS38J                               00150000
//                                                                      00160000
