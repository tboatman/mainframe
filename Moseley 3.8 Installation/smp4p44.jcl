//SMP4P44  JOB (SYSGEN),'SMP 4.44 FROM MVS38J',                         00010000
//             CLASS=A,                                                 00020000
//             MSGCLASS=A,MSGLEVEL=(1,1)                                00030000
//*                                                                     00040000
//********************************************************************* 00050000
//*                                                                     00060000
//*                       MVS 3.8 SYSGEN                                00070000
//*                       ==============                                00080000
//*                                                                     00090000
//* FUNC: 1. Install SMP 4.44 load modules into SYS1.LINKLIB from the   00100000
//*          MVS 3.8J DLIB tape (VOL=SER=MVS38J; file: product.het).    00110000
//*       2. Install SMP 4 JCL procedures into SYS1.PROCLIB from the    00120000
//*          MVS 3.8J DTR tape (VOL=SER=T74172; file: smp4b.het).       00130000
//*       3. Install a modified SMP 4 JCL procedure into SYS1.PROCLIB.  00140000
//*          This procedure, based upon those contained on the DTR tape 00150000
//*          correct some errors and include changes that make the      00160000
//*          installation process easier.                               00170000
//*       4. Initialize WORK00, WORK01, and SMP000 volumes              00180000
//*                                                                     00190000
//* NOTE: 1. The level of the SMP 4 load modules supplied on the        00200000
//*          MVS 3.8J DTR tape (4.22) is lower than the level required  00210000
//*          for the installation of MVS 3.8J (4.24).  This job builds  00220000
//*          SMP 4 load modules at a higher level (4.44) from:          00230000
//*          (a) Linkage editor control statements extracted from the   00240000
//*              last of three jobs contained within the JCLIN for      00250000
//*              function ESY1400 contained within the SMPMCS data set  00260000
//*              on the MVS 3.8J DLIB tape.                             00270000
//*          (b) Modules extracted from the ESY1400.F1 data set on the  00280000
//*              MVS 3.8J DLIB tape.                                    00290000
//*       2. An IPL is required to activate the new load modules.       00300000
//*                                                                     00310000
//* HIST: 2002.01.25: Created by Peter Stockdill.                       00320000
//*       2014.12.05: Modified by Jay Moseley                           00330000
//*                                                                     00340000
//********************************************************************* 00350000
//*                                                                     00360000
/*MESSAGE  ************************************************************ 00370000
/*MESSAGE  * An IPL is required after this job has completed!!!       * 00380000
/*MESSAGE  ************************************************************ 00390000
//*-------------------------------------------------------------------- 00400000
//S1       EXEC PGM=IDCAMS,REGION=1024K                                 00410000
//SYSIN    DD  *                                                        00420000
 REPRO INFILE(I1) OUTFILE(O1) SKIP(1607) COUNT(161)                     00430000
//I1       DD  DSN=SMPMCS,DISP=OLD,                                     00440000
//             UNIT=(TAPE,,DEFER),VOL=(,RETAIN,SER=MVS38J),LABEL=(1,SL) 00450000
//O1       DD  DISP=(NEW,PASS),                                         00460000
//             UNIT=SYSDA,SPACE=(TRK,(1,1)),                            00470000
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00480000
//SYSPRINT DD  SYSOUT=*                                                 00490000
//*-------------------------------------------------------------------- 00500000
//S2       EXEC PGM=IEBCOPY,REGION=1024K                                00510000
//SYSIN    DD  *                                                        00520000
         COPY  OUTDD=O1,INDD=I1                                         00530000
         COPY  OUTDD=O2,INDD=((I2,R))                                   00540000
//I1       DD  DSN=ESY1400.F1,DISP=OLD,                                 00550000
//             VOL=REF=*.S1.I1,LABEL=(4,SL)                             00560000
//I2       DD  DSN=SMPMVS,DISP=OLD,                                     00570000
//             UNIT=AFF=I1,VOL=SER=T74172,LABEL=(1,SL)                  00580000
//O1       DD  DISP=(NEW,PASS),                                         00590000
//             UNIT=SYSDA,SPACE=(TRK,(50,20,30))                        00600000
//O2       DD  DSN=SYS1.PROCLIB,DISP=SHR                                00610000
//SYSPRINT DD  SYSOUT=*                                                 00620000
//*-------------------------------------------------------------------- 00630000
//S3       EXEC LINKS,                                                  00640000
//             PARM='LIST,NCAL,SIZE=(512K,100K)',CLASS='*',             00650000
//             OBJ=,UNIT=,SER=,N=' ',NAME=,P1=,MOD=,P2=                 00660000
//SYSPUNCH DD  DUMMY                                                    00670000
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                00680000
//SYSLIN   DD  DSN=*.S1.O1,DISP=(OLD,DELETE)                            00690000
//AOS12    DD  DSN=*.S2.O1,DISP=(OLD,DELETE)                            00700000
//*-------------------------------------------------------------------- 00710000
//S4       EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      00720000
//SYSUT2    DD DISP=MOD,DSN=SYS1.PROCLIB                                00730000
//SYSPRINT  DD SYSOUT=*                                                 00740000
//SYSIN     DD DATA,DLM='><'                                            00750000
./ ADD NAME=DLBSMP,LIST=ALL                                             00760000
//DLBSMP  PROC TLIB=WORK01             WORK VOLUME SERIAL               00770000
//SMP     EXEC PGM=HMASMP,PARM='DATE=U',REGION=4096K,TIME=999           00780000
//********************************************************************* 00790000
//*             OS/VS2 (MVS) DLIB BUILD PROCEDURE                       00800000
//********************************************************************* 00810000
//SMPLOG   DD  DUMMY                                 LOG                00820000
//SMPOUT   DD  SYSOUT=*                              SUMMARY REPORTS    00830000
//SMPTLIB  DD  UNIT=SYSDA,VOL=SER=&TLIB,DISP=OLD     RELFILE TLIBS      00840000
//SMPWRK1  DD  UNIT=SYSDA,SPACE=(CYL,(5,10,84)),     WORK-COPY/UPDTE    00850000
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     00860000
//SMPWRK2  DD  UNIT=SYSDA,SPACE=(CYL,(5,10,84)),     WORK-COPY/UPDTE    00870000
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     00880000
//SMPWRK3  DD  UNIT=SYSDA,SPACE=(CYL,(5,10,84)),     WORK-OBJECT/ZAP    00890000
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     00900000
//SMPWRK4  DD  UNIT=SYSDA,SPACE=(CYL,(1,10,84)),     WORK-EXPAND/ZAP    00910000
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     00920000
//SMPWRK5  DD  UNIT=SYSDA,SPACE=(CYL,(30,10,250))    WORK-LOAD MODS     00930000
//SYSPRINT DD  SYSOUT=*                              UTILITIES DEFAULT  00940000
//SYSUDUMP DD  SYSOUT=*                              ABEND DUMP         00950000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))     WORK               00960000
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(600,100))     WORK               00970000
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(600,100))     WORK               00980000
//SYSUT4   DD  UNIT=SYSDA,SPACE=(80,(2,2))           WORK RETRY         00990000
//********************************************************************* 01000000
//ASMPRINT DD  SYSOUT=*                              ASSEMBLER          01010000
//CMPPRINT DD  SYSOUT=*                              COMPRESS           01020000
//COPPRINT DD  SYSOUT=*                              IEBCOPY            01030000
//LKDPRINT DD  SYSOUT=*                              LINK EDITOR        01040000
//E37PRINT DD  SYSOUT=*                              RETRY              01050000
//UPDPRINT DD  SYSOUT=*                              IEBUPDTE           01060000
//ZAPPRINT DD  SYSOUT=*                              AMASPZAP           01070000
//SYSLIB   DD  DSN=SYS1.AMACLIB,DISP=SHR             ASM MACROS/COPY    01080000
//         DD  DSN=SYS1.AMODGEN,DISP=SHR                                01090000
//         DD  DSN=SYS1.AGENLIB,DISP=SHR                                01100000
//         DD  DSN=SYS1.SMPMTS,DISP=SHR                                 01110000
//********************************************************************* 01120000
//SMPACDS  DD  DSN=SYS1.SMPACDS,DISP=OLD                                01130000
//SMPACRQ  DD  DSN=SYS1.SMPACRQ,DISP=OLD                                01140000
//SMPMTS   DD  DSN=SYS1.SMPMTS,DISP=OLD                                 01150000
//SMPPTS   DD  DSN=SYS1.SMPPTS,DISP=OLD                                 01160000
//SMPSTS   DD  DSN=SYS1.SMPSTS,DISP=OLD                                 01170000
//********************************************************************* 01180000
//ACMDLIB  DD  DSN=SYS1.ACMDLIB,DISP=OLD                                01190000
//AGENLIB  DD  DSN=SYS1.AGENLIB,DISP=OLD                                01200000
//AHELP    DD  DSN=SYS1.AHELP,DISP=OLD                                  01210000
//AIMAGE   DD  DSN=SYS1.AIMAGE,DISP=OLD                                 01220000
//ALPALIB  DD  DSN=SYS1.ALPALIB,DISP=OLD                                01230000
//AMACLIB  DD  DSN=SYS1.AMACLIB,DISP=OLD                                01240000
//AMODGEN  DD  DSN=SYS1.AMODGEN,DISP=OLD                                01250000
//AOS00    DD  DSN=SYS1.AOS00,DISP=OLD                                  01260000
//AOS03    DD  DSN=SYS1.AOS03,DISP=OLD                                  01270000
//AOS04    DD  DSN=SYS1.AOS04,DISP=OLD                                  01280000
//AOS05    DD  DSN=SYS1.AOS05,DISP=OLD                                  01290000
//AOS06    DD  DSN=SYS1.AOS06,DISP=OLD                                  01300000
//AOS07    DD  DSN=SYS1.AOS07,DISP=OLD                                  01310000
//AOS11    DD  DSN=SYS1.AOS11,DISP=OLD                                  01320000
//AOS12    DD  DSN=SYS1.AOS12,DISP=OLD                                  01330000
//AOS20    DD  DSN=SYS1.AOS20,DISP=OLD                                  01340000
//AOS21    DD  DSN=SYS1.AOS21,DISP=OLD                                  01350000
//AOS24    DD  DSN=SYS1.AOS24,DISP=OLD                                  01360000
//AOS26    DD  DSN=SYS1.AOS26,DISP=OLD                                  01370000
//AOS29    DD  DSN=SYS1.AOS29,DISP=OLD                                  01380000
//AOS32    DD  DSN=SYS1.AOS32,DISP=OLD                                  01390000
//AOSA0    DD  DSN=SYS1.AOSA0,DISP=OLD                                  01400000
//AOSA1    DD  DSN=SYS1.AOSA1,DISP=OLD                                  01410000
//AOSB0    DD  DSN=SYS1.AOSB0,DISP=OLD                                  01420000
//AOSB3    DD  DSN=SYS1.AOSB3,DISP=OLD                                  01430000
//AOSBN    DD  DSN=SYS1.AOSBN,DISP=OLD                                  01440000
//AOSC2    DD  DSN=SYS1.AOSC2,DISP=OLD                                  01450000
//AOSC5    DD  DSN=SYS1.AOSC5,DISP=OLD                                  01460000
//AOSC6    DD  DSN=SYS1.AOSC6,DISP=OLD                                  01470000
//AOSCA    DD  DSN=SYS1.AOSCA,DISP=OLD                                  01480000
//AOSCD    DD  DSN=SYS1.AOSCD,DISP=OLD                                  01490000
//AOSCE    DD  DSN=SYS1.AOSCE,DISP=OLD                                  01500000
//AOSD0    DD  DSN=SYS1.AOSD0,DISP=OLD                                  01510000
//AOSD7    DD  DSN=SYS1.AOSD7,DISP=OLD                                  01520000
//AOSD8    DD  DSN=SYS1.AOSD8,DISP=OLD                                  01530000
//AOSG0    DD  DSN=SYS1.AOSG0,DISP=OLD                                  01540000
//AOSH1    DD  DSN=SYS1.AOSH1,DISP=OLD                                  01550000
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=OLD                                  01560000
//AOST3    DD  DSN=SYS1.AOST3,DISP=OLD                                  01570000
//AOST4    DD  DSN=SYS1.AOST4,DISP=OLD                                  01580000
//AOSU0    DD  DSN=SYS1.AOSU0,DISP=OLD                                  01590000
//APARMLIB DD  DSN=SYS1.APARMLIB,DISP=OLD                               01600000
//APROCLIB DD  DSN=SYS1.APROCLIB,DISP=OLD                               01610000
//ASAMPLIB DD  DSN=SYS1.ASAMPLIB,DISP=OLD                               01620000
//ATCAMMAC DD  DSN=SYS1.ATCAMMAC,DISP=OLD                               01630000
//ATSOMAC  DD  DSN=SYS1.ATSOMAC,DISP=OLD                                01640000
//AUADS    DD  DSN=SYS1.AUADS,DISP=OLD                                  01650000
//HASPSRC  DD  DSN=SYS1.HASPSRC,DISP=OLD                                01660000
//********************************************************************* 01670000
><                                                                      01680000
//*                                                                     01690000
//S5       EXEC  PGM=IEHDASDR,REGION=1024K                              01700000
//SYSPRINT DD  SYSOUT=A                                                 01710000
//SYSIN    DD  *                                                        01720000
   ANALYZE TODD=148,VTOC=1,EXTENT=15,NEWVOLID=SMP000                    01730000
   ANALYZE TODD=149,VTOC=1,EXTENT=15,NEWVOLID=WORK00                    01740000
   ANALYZE TODD=14A,VTOC=1,EXTENT=15,NEWVOLID=WORK01                    01750000
//                                                                      01760000
