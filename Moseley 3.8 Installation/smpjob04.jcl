//SMPJOB04 JOB 'REJECT-CLEANUP',                                        00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
/*JOBPARM   LINES=100                                                   00030000
//*                                                                     00040000
//* ***************************************************************** * 00050000
//* REMOVE WORK DATASETS FROM WORK01 AND CLEAN UP AFTER SMP           * 00060000
//* ***************************************************************** * 00070000
//*                                                                     00080000
//DLBUCL EXEC DLBSMP,TIME.SMP=1439                                      00090000
//SMPCNTL  DD  *                                                        00100000
     REJECT S(EAS1102) .                                                00110000
     REJECT S(EBB1102) .                                                00120000
     REJECT S(EBT1102) .                                                00130000
     REJECT S(EDE1102) .                                                00140000
     REJECT S(EDM1102) .                                                00150000
     REJECT S(EDS1102) .                                                00160000
     REJECT S(EER1400) .                                                00170000
     REJECT S(EGA1102) .                                                00180000
     REJECT S(EGS1102) .                                                00190000
     REJECT S(EIP1102) .                                                00200000
     REJECT S(EJE1103) .                                                00210000
     REJECT S(EMF1102) .                                                00220000
     REJECT S(EMI1102) .                                                00230000
     REJECT S(EML1102) .                                                00240000
     REJECT S(EMS1102) .                                                00250000
     REJECT S(EPM1102) .                                                00260000
     REJECT S(EST1102) .                                                00270000
     REJECT S(ESU1102) .                                                00280000
     REJECT S(ESY1400) .                                                00290000
     REJECT S(ETC0108) .                                                00300000
     REJECT S(ETI1106) .                                                00310000
     REJECT S(ETV0108) .                                                00320000
     REJECT S(EUT1102) .                                                00330000
     REJECT S(EVT0108) .                                                00340000
     REJECT S(EXW1102) .                                                00350000
     REJECT S(FBB1221) .                                                00360000
     REJECT S(FDM1133) .                                                00370000
     REJECT S(FDS1122) .                                                00380000
     REJECT S(FDS1133) .                                                00390000
     REJECT S(FDZ1610) .                                                00400000
     REJECT S(FUT1133) .                                                00410000
//*                                                                     00420000
//SMPPTFIN DD  DUMMY                                                    00430000
//*                                                                     00440000
//                                                                      00450000
