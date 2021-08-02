//FDZ1D02  JOB (SYS),'INSTALL DSF R13',CLASS=A,MSGCLASS=A               00010000
//*                                                                     00020000
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00030000
//* INSTALL ICKDSF (DEVICE SUPPORT FACILITIES) RELEASE 13.0 FROM        00040000
//* MVS3.8J DISTRIBUTION TAPE RENAMING LOAD MODULE TO RETAIN ORIGINAL   00050000
//* ICKDSF FROM BASE INSTALLATION.                                      00060000
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00070000
//*                                                                     00080000
//IEBCOPY  EXEC PGM=IEBCOPY,REGION=1024K                                00090000
//SYSPRINT DD  SYSOUT=*                                                 00100000
//LIBIN1   DD  DSN=FDZ1D02.F1,UNIT=TAPE,DISP=(OLD,PASS),                00110000
//             LABEL=(79,SL),VOL=(PRIVATE,RETAIN,SER=MVS38J)            00120000
//LIBOUT1  DD  DSN=SYS1.FDZ1D02.F1,DISP=(,KEEP,DELETE),                 00130000
//             UNIT=SYSDA,VOL=SER=WORK01,SPACE=(CYL,(1,1,5))            00140000
//LIBIN2   DD  DSN=FDZ1D02.F2,UNIT=TAPE,DISP=OLD,                       00150000
//             LABEL=(80,SL),VOL=(PRIVATE,RETAIN,SER=MVS38J)            00160000
//LIBOUT2  DD  DSN=SYS1.FDZ1D02.F2,DISP=(,KEEP,DELETE),                 00170000
//             UNIT=SYSDA,VOL=SER=WORK01,SPACE=(CYL,(1,1,20))           00180000
//SYSUT3   DD  UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(NEW,DELETE)          00190000
//SYSIN    DD  *                                                        00200000
  COPY INDD=LIBIN1,OUTDD=LIBOUT1                                        00210000
  COPY INDD=LIBIN2,OUTDD=LIBOUT2                                        00220000
//*                                                                     00230000
//IDCAMS   EXEC PGM=IDCAMS,REGION=1024K,COND=(0,NE)                     00240000
//SYSPRINT DD  SYSOUT=*                                                 00250000
//JCLPART1 DD  DATA,DLM='><'                                            00260000
//FDZ1D02  JOB (SYS),'INSTALL DSF R13',CLASS=A,MSGCLASS=A               00270000
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00280000
//IEWL     EXEC PGM=IEWL,REGION=512K,PARM='XREF,LIST,LET,RENT,REFR'     00290000
//SYSPRINT DD  SYSOUT=*                                                 00300000
//SYSUT1   DD  SPACE=(CYL,(5,1)),UNIT=SYSDA                             00310000
//SYSLMOD  DD  DSN=SYS2.LINKLIB,DISP=MOD                                00320000
//AOSU0    DD  DSN=SYS1.FDZ1D02.F2,DISP=SHR,                            00330000
//             UNIT=SYSDA,VOL=SER=WORK01                                00340000
//SYSLIN   DD  *                                                        00350000
><                                                                      00360000
//JCLPART2 DD  DISP=SHR,DSN=SYS1.FDZ1D02.F1(FDZ1D02),                   00370000
//             UNIT=SYSDA,VOL=SER=WORK01                                00380000
//JCLPART3 DD  DATA,DLM='><'                                            00390000
  NAME ICKDSF13(R)                                                      00400000
//IEHPROGM EXEC PGM=IEHPROGM                                            00410000
//SYSPRINT DD  SYSOUT=*                                                 00420000
//DD1      DD  UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD                       00430000
//SYSIN    DD  *                                                        00440000
  SCRATCH DSNAME=SYS1.FDZ1D02.F1,VOL=SYSDA=WORK01,PURGE                 00450000
  SCRATCH DSNAME=SYS1.FDZ1D02.F2,VOL=SYSDA=WORK01,PURGE                 00460000
><                                                                      00470000
//SYSIN    DD  *                                                        00480000
  REPRO INFILE(JCLPART1) OUTFILE(TEMPDD)                                00490000
  REPRO INFILE(JCLPART2) OUTFILE(TEMPDD) SKIP(4) COUNT(93)              00500000
  REPRO INFILE(JCLPART3) OUTFILE(TEMPDD)                                00510000
//TEMPDD   DD  DSN=&&TEMPDD,UNIT=SYSDA,DISP=(MOD,PASS),SPACE=(CYL,2),   00520000
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=80)                       00530000
//*                                                                     00540000
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE)                                00550000
//SYSPRINT DD  SYSOUT=*                                                 00560000
//SYSIN    DD  DUMMY                                                    00570000
//SYSUT1   DD  DSN=&&TEMPDD,UNIT=SYSDA,DISP=(OLD,DELETE)                00580000
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        00590000
//                                                                      00600000
