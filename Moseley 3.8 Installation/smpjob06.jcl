//SMPJOB06 JOB 'ACCEPT USERMODS',                                       00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
/*JOBPARM   LINES=100                                                   00030000
//*                                                                     00040000
//* ***************************************************************** * 00050000
//* ACCEPT JIM MORRISON'S 3375+3380+3390 USERMODS                     * 00060000
//* ***************************************************************** * 00070000
//*                                                                     00080000
//DLBUCL EXEC DLBSMP,TIME.SMP=1439                                      00090000
//SMPCNTL  DD  *                                                        00100000
                                                                        00110000
 /* Use the following set of SMP4 control statements         */         00120000
 /* if you have NOT previously installed 3380 + 3390 support */         00130000
                                                                        00140000
 ACCEPT S(M023000                                                       00150000
          M023100                                                       00160000
          M023200 M023201 M023202 M023203 M023204                       00170000
          M023300 M023301 M023302                                       00180000
          M023400 M023401 M023402 M023403 M023404 M023405               00190000
          M024001                                                       00200000
          M024101                                                       00210000
          M024205 M024206 M024207                                       00220000
          M024303 M024304 M024305                                       00230000
          M024406 M024407 M024408                                       00240000
   ) USERMODS NOAPPLY  DIS(WRITE) COMPRESS(ALL)                         00250000
 .                                                                      00260000
//SMPPTFIN DD  DUMMY                                                    00270000
//                                                                      00280000
