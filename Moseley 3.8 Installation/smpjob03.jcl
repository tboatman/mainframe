//SMPJOB03 JOB 'ACCEPT PRODUCTS & PTFS',                                00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
/*JOBPARM   LINES=100                                                   00030000
//*                                                                     00040000
//* ***************************************************************** * 00050000
//* ACCEPT ALL PRODUCTS PLUS REQUIRED PTFS                            * 00060000
//* ***************************************************************** * 00070000
//*                                                                     00080000
//DLBUCL1 EXEC DLBSMP                                                   00090000
//SMPCNTL  DD  *                                                        00100000
 ACCEPT G(EBT1102   /* BTAM                                         */  00110000
         )                                                              00120000
        DIS(WRITE)                                                      00130000
        NOAPPLY                                                         00140000
        COMPRESS(ALL)                                                   00150000
 .                                                                      00160000
//*                                                                     00170000
//SMPPTFIN DD  DUMMY                                                    00180000
//*                                                                     00190000
//DLBUCL2 EXEC DLBSMP                                                   00200000
//SMPCNTL  DD  *                                                        00210000
 ACCEPT G(EAS1102   /* XF ASSEMBLER                                 */  00220000
          EBB1102   /* BASE CONTROL PROGRAM                         */  00230000
          EDE1102   /* DISPLAY EXCEPTION MONITORING FACILITY        */  00240000
          EER1400   /* ENVIRONMENT RECORDING EDITING & PRINTING     */  00250000
          EGA1102   /* GRAPHIC ACCESS METHOD                        */  00260000
          EGS1102   /* GRAPHIC SUBROUTINE PACKAGE                   */  00270000
          EIP1102   /* INTERACTIVE PROBLEM CONTROL SYSTEM           */  00280000
          EMF1102   /* MF/1                                         */  00290000
          EMI1102   /* MICR/OCR                                     */  00300000
          EDM1102   /* DATA MANAGEMENT                              */  00310000
          EDS1102   /* DATA MANAGEMENT SUPPORT                      */  00320000
          EPM1102   /* PROGRAM MANAGEMENT                           */  00330000
          EST1102   /* SYSTEM SUPPORT                               */  00340000
          ESU1102   /* SU BIT STRING                                */  00350000
          ESY1400   /* SYSTEM MODIFICATION PROGRAM 4                */  00360000
          ETV0108   /* TSO/VTAM                                     */  00370000
          EUT1102   /* UTILITIES                                    */  00380000
          EVT0108   /* VTAM                                         */  00390000
          EXW1102   /* EXTERNAL WRITER                              */  00400000
         )                                                              00410000
     DIS(WRITE)                                                         00420000
     NOAPPLY                                                            00430000
     COMPRESS(ALL)                                                      00440000
 .                                                                      00450000
//*                                                                     00460000
//SMPPTFIN DD  DUMMY                                                    00470000
//*                                                                     00480000
//DLBUCL3 EXEC DLBSMP                                                   00490000
//SMPCNTL  DD  *                                                        00500000
 ACCEPT G(EML1102   /* MULTI-LEAVING WORK STATION                   */  00510000
          EMS1102   /* MASS STORAGE SYSTEM                          */  00520000
          FDZ1610   /* DEVICE SUPPORT FACILITY                      */  00530000
          ETI1106   /* TERMINAL I/O CONTROLLER                      */  00540000
          ETC0108   /* TCAM                                         */  00550000
         )                                                              00560000
     DIS(WRITE)                                                         00570000
     NOAPPLY                                                            00580000
     COMPRESS(ALL)                                                      00590000
 .                                                                      00600000
//*                                                                     00610000
//SMPPTFIN DD  DUMMY                                                    00620000
//*                                                                     00630000
//DLBUCL4 EXEC DLBSMP                                                   00640000
//SMPCNTL  DD  *                                                        00650000
 ACCEPT G(FBB1221   /* MVS PROCESSOR SUPPORT 2                      */  00660000
            UY10657                                                     00670000
            UZ64216                                                     00680000
            UZ67485                                                     00690000
            UZ69168                                                     00700000
            UZ72152                                                     00710000
            UZ72384                                                     00720000
          FDS1122   /* MVS PROCESSOR SUPPORT 2                      */  00730000
          )                                                             00740000
     DIS(WRITE)                                                         00750000
     NOAPPLY                                                            00760000
     COMPRESS(ALL)                                                      00770000
 .                                                                      00780000
//*                                                                     00790000
//SMPPTFIN DD  DUMMY                                                    00800000
//*                                                                     00810000
//DLBUCL5 EXEC DLBSMP                                                   00820000
//SMPCNTL  DD  *                                                        00830000
 ACCEPT G(EJE1103   /* JES2 WITH 3800 ENHANCEMENTS                  */  00840000
          FDM1133   /* 3800 ENHANCEMENTS - DATA MANAGEMENT          */  00850000
          FDS1133   /* 3800 ENHANCEMENTS - DATA MANAGEMENT SUPPORT  */  00860000
          FUT1133   /* 3800 ENHANCEMENTS - UTILITIES                */  00870000
          )                                                             00880000
     DIS(WRITE)                                                         00890000
     NOAPPLY                                                            00900000
     COMPRESS(ALL)                                                      00910000
 .                                                                      00920000
//                                                                      00930000