//SYSGEN00 JOB 'INITIALIZE DASD',                                       00000100
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00000200
//*                                                                     00000300
//IEHPROGM EXEC PGM=IEHPROGM                                            00000400
//* ***************************************************************** * 00000500
//* Scratch datasets on WORK01 from prior run (if any).               * 00000600
//* ***************************************************************** * 00000700
//WORK01   DD  UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD                       00000800
//SYSPRINT DD  SYSOUT=*                                                 00000900
//SYSIN    DD  *                                                        00001000
  SCRATCH DSNAME=SYS1.OBJPDS01,VOL=SYSDA=WORK01,PURGE                   00001100
  SCRATCH DSNAME=SYS1.OBJPDS02,VOL=SYSDA=WORK01,PURGE                   00001200
  SCRATCH DSNAME=SYS1.OBJPDS03,VOL=SYSDA=WORK01,PURGE                   00001300
  SCRATCH DSNAME=SYS2.LOCAL.LPALIB,VOL=SYSDA=WORK01,PURGE               00001400
//*                                                                     00001500
//*                                                                     00001600
//ICKDSF   EXEC PGM=ICKDSF,REGION=4096K                                 00001700
//* ***************************************************************** * 00001800
//* Initialize 3350 DASD on 149, 14A, 14B, 14C to receive MVS3.8J     * 00001900
//* system datasets.                                                  * 00002000
//* ***************************************************************** * 00002100
//IPLTEXT  DD  DSN=SYS1.ASAMPLIB(IEAIPL00),DISP=SHR,                    00002200
//             UNIT=3350,VOL=SER=SMP000                                 00002300
//SYSPRINT DD  SYSOUT=*                                                 00002400
//SYSIN    DD  *                                                        00002500
  INIT UNIT(149) -                                                      00002600
               VERIFY(111111) -                                         00002700
               OWNER(HERCULES) -                                        00002800
               VOLID(MVSRES) -                                          00002900
               IPLDD(IPLTEXT) -                                         00003000
               VTOC(0,1,30)                                             00003100
  INIT UNIT(14A) -                                                      00003200
               VERIFY(222222) -                                         00003300
               OWNER(HERCULES) -                                        00003400
               VOLID(MVS000) -                                          00003500
               VTOC(0,1,30)                                             00003600
  INIT UNIT(14B) -                                                      00003700
               VERIFY(333333) -                                         00003800
               OWNER(HERCULES) -                                        00003900
               VOLID(SPOOL1) -                                          00004000
               VTOC(0,1,1)                                              00004100
  INIT UNIT(14C) -                                                      00004200
               VERIFY(444444) -                                         00004300
               OWNER(HERCULES) -                                        00004400
               VOLID(PAGE00) -                                          00004500
               VTOC(0,1,1)                                              00004600
//*                                                                     00004700
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE,ICKDSF)                         00004800
//* ***************************************************************** * 00004900
//* IF ICKDSF RC=0000, submit continuation job to internal reader.    * 00005000
//* ***************************************************************** * 00005100
//SYSPRINT DD  DUMMY                                                    00005200
//SYSIN    DD  DUMMY                                                    00005300
//SYSUT1   DD  DATA,DLM='><'                                            00005400
//SYSGEN00 JOB 'MOUNT DASD',                                            00005500
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00005600
//*                                                                     00005700
//********************************************************************* 00005800
//* Present VARY and MOUNT commands to the operator for confirmation. * 00005900
//* The DASD volumes initialized in the prior JOB will be placed      * 00006000
//* online and mounted with storage class of PRIVATE.  These volumes  * 00006100
//* will receive the generated MVS 3.8j system datasets.              * 00006200
//********************************************************************* 00006300
// V (149,14A,14B,14C),ONLINE                                           00006400
// M 149,VOL=(SL,MVSRES),USE=PRIVATE                                    00006500
// M 14A,VOL=(SL,MVS000),USE=PRIVATE                                    00006600
// M 14B,VOL=(SL,SPOOL1),USE=PRIVATE                                    00006700
// M 14C,VOL=(SL,PAGE00),USE=PRIVATE                                    00006800
//IEFBR14  EXEC PGM=IEFBR14                                             00006900
//*         END OF SUBMITTED SYSGEN00 (1 OF 2)                          00007000
//SYSGEN00 JOB 'SETUP FOR SYSGEN',                                      00007100
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A,COND=(0,NE)            00007200
//*                                                                     00007300
//DEFCAT   EXEC PGM=IDCAMS,REGION=4096K                                 00007400
//* ***************************************************************** * 00007500
//* DEFINE A USER CATALOG ON MVSRES, CATALOGED IN THE STARTER SYSTEM  * 00007600
//* MASTER CATALOG.  IT WILL BE USED DURING THE SYSTEM GENERATION AND * 00007700
//* WILL BECOME THE MASTER CATALOG ON THE TARGET MVS 3.8 SYSTEM.      * 00007800
//* ***************************************************************** * 00007900
//SYSPRINT DD  SYSOUT=*                                                 00008000
//SYSUT1   DD  UNIT=3350,VOL=SER=MVSRES,DISP=OLD                        00008100
//SYSIN    DD  *                                                        00008200
                                                                        00008300
  EXPORT SYS1.VSAM.MASTER.CATALOG DISCONNECT                            00008400
                                                                        00008500
  SET LASTCC = 0                                                        00008600
  SET  MAXCC = 0                                                        00008700
                                                                        00008800
  DEFINE USERCATALOG (                 -                                00008900
               NAME(SYS1.VSAM.MASTER.CATALOG) -                         00009000
               FILE(SYSUT1)            -                                00009100
               VOLUME(MVSRES)          -                                00009200
               CYLINDERS(30)           -                                00009300
               BUFFERSPACE(8192) )                                      00009400
                                                                        00009500
//*                                                                     00009600
//ALLOC    EXEC PGM=IEFBR14                                             00009700
//* ***************************************************************** * 00009800
//* ALLOCATE AND CATALOG WORK DATASETS USED IN STAGE 2 DECK           * 00009900
//* ***************************************************************** * 00010000
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00010100
//OBJPDS01 DD  DSN=SYS1.OBJPDS01,DISP=(NEW,CATLG,DELETE),               00010200
//             UNIT=3350,VOL=SER=WORK01,                                00010300
//             SPACE=(CYL,(20,10,50)),                                  00010400
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00010500
//OBJPDS02 DD  DSN=SYS1.OBJPDS02,DISP=(NEW,CATLG,DELETE),               00010600
//             UNIT=3350,VOL=SER=WORK01,                                00010700
//             SPACE=(CYL,(20,10,50)),                                  00010800
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00010900
//OBJPDS03 DD  DSN=SYS1.OBJPDS03,DISP=(NEW,CATLG,DELETE),               00011000
//             UNIT=3350,VOL=SER=WORK01,                                00011100
//             SPACE=(CYL,(20,10,50)),                                  00011200
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00011300
//*                                                                     00011400
//ASM      EXEC PGM=IEUASM,REGION=1024K,                                00011500
//             PARM='LIST,OBJECT,NODECK,NOXREF'                         00011600
//* ***************************************************************** * 00011700
//* ASSEMBLE MODULE FOR LOCAL SVC 244 (AUTHORIZIATION)                * 00011800
//* ***************************************************************** * 00011900
//SYSLIB   DD  DSN=SYS1.AMACLIB,UNIT=SYSDA,VOL=SER=SMP000,DISP=SHR      00012000
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        00012100
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        00012200
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        00012300
//SYSPRINT DD  SYSOUT=*                                                 00012400
//SYSGO    DD  UNIT=SYSDA,DISP=(,PASS),DSN=&&SYSGO,                     00012500
//             SPACE=(TRK,45),DCB=BLKSIZE=80                            00012600
//SYSIN    DD  *                                                        00012700
* SOURCE: CBT249-FILE0360                                               00012800
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 00012900
*                                                                     * 00013000
*  ----------------------S V C   2 4 4 -----------------------------  * 00013100
*  SETJSCB - A NON-AUTHORIZED TYPE 4 SVC THAT TURNS THE JSCB          * 00013200
*  AUTHORIZATION ON OR OFF BASED ON THE ENTRY CODE IN REGISTER 1.     * 00013300
*                                                                     * 00013400
*  ENTRY CODE = 0 - TURN JSCB AUTHORIZATION ON                        * 00013500
*  ENTRY CODE = 4 - TURN JSCB AUTHORIZATION OFF                       * 00013600
*                                                                     * 00013700
*  CODED 3/18/76 BY J. W. RICH (FROM R. MARKEL).                      * 00013800
*  MODED 3/31/81 BY J. A. MARTIN - W.S.R.C.C                          * 00013900
*        ADDED CHECK FOR R1=0 OR R1=4 => SDUMP IF NOT GOOD R1         * 00014000
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 00014100
IGC0024D CSECT                                                          00014200
         USING *,R6                BASE REGISTER                        00014300
         L     R12,PSATOLD         LOAD CURRENT TCB PTR                 00014400
         L     R12,TCBJSCB(0,R12)  LOAD JSCB PTR                        00014500
         LTR   R1,R1                    CHECK IF ZERO                   00014600
         BZ    DOIT                     R1=0 IS OK - GO DO IT           00014700
         C     R1,F4                    CHECK IF FOUR                   00014800
         BE    DOIT                     R1=4 IS OK - GO DO IT           00014900
         SDUMP HDR='IGC0024D ERROR R1 ^= 0 OR 4 - WSRCC LOCAL SVC'      00015000
         SR    R1,R1                    SET OFF AUTH                    00015100
DOIT     EX    R0,APFON(R1)        "EX"ECUTE APFON OR APFOFF BASED      00015200
*                                       ON REGISTER 1 ENTRY CODE        00015300
         BR    R14                 RETURN                               00015400
         EJECT                                                          00015500
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 00015600
*  VARIOUS AND SUNDRY "EX"ECUTED INSTRUCTIONS                         * 00015700
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 00015800
APFON    OI    JSCBOPTS(R12),JSCBAUTH        TURN ON AUTHORIZATION      00015900
APFOFF   NI    JSCBOPTS(R12),X'FF'-JSCBAUTH  TURN OFF AUTHORIZATION     00016000
         EJECT                                                          00016100
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 00016200
*  VARIOUS AND SUNDRY EQUATES                                           00016300
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* 00016400
F4       DC    F'04'               USED TO COMPARE TO R1                00016500
JSCBAUTH EQU   X'01'               JSCB AUTHORIZATION BIT               00016600
PSATOLD  EQU   X'21C'              OFFSET TO TCBOLD PTR                 00016700
TCBJSCB  EQU   X'B4'               OFFSET TO JSCB PTR                   00016800
JSCBOPTS EQU   X'EC'               OFFSET TO JSCB AUTHORIZATION FIELD   00016900
R0       EQU   0                   REGISTER 0                           00017000
R1       EQU   1                   REGISTER 1                           00017100
R6       EQU   6                   REGISTER 6                           00017200
R12      EQU   12                  REGISTER 12                          00017300
R14      EQU   14                  REGISTER 14                          00017400
         END                                                            00017500
//*                                                                     00017600
//LINK    EXEC PGM=IEWL,PARM=(LIST,MAP,XREF)                            00017700
//* ***************************************************************** * 00017800
//* CREATE SYS2.LOCAL.LPALIB AND INSERT MODULES FOR SVCS              * 00017900
//* ***************************************************************** * 00018000
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00018100
//SYSPRINT DD  SYSOUT=*                                                 00018200
//SYSLIB   DD  DISP=SHR,DSN=SYS1.LINKLIB                                00018300
//SYSLMOD  DD  DSN=SYS2.LOCAL.LPALIB,DISP=(NEW,CATLG,DELETE),           00018400
//             UNIT=3350,VOL=SER=WORK01,                                00018500
//             SPACE=(CYL,(2,1,10)),                                    00018600
//             DCB=SYS1.LPALIB                                          00018700
//SYSGO    DD  DSN=&&SYSGO,DISP=(OLD,DELETE)                            00018800
//SYSUT1   DD  UNIT=3330,SPACE=(CYL,(5,1))                              00018900
//SYSLIN   DD  *                                                        00019000
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 240                          00019100
 NAME IGC0024{(R)                                                       00019200
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 241                          00019300
 NAME IGC0024A(R)                                                       00019400
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 242                          00019500
 NAME IGC0024B(R)                                                       00019600
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 243                          00019700
 NAME IGC0024C(R)                                                       00019800
 INCLUDE SYSGO                   ASSEMBLED SVC 244                      00019900
 NAME IGC0024D(R)                                                       00020000
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 245                          00020100
 NAME IGC0024E(R)                                                       00020200
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 246                          00020300
 NAME IGC0024F(R)                                                       00020400
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 247                          00020500
 NAME IGC0024G(R)                                                       00020600
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 248                          00020700
 NAME IGC0024H(R)                                                       00020800
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 249                          00020900
 NAME IGC0024I(R)                                                       00021000
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 250                          00021100
 NAME IGC0025{(R)                                                       00021200
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 251                          00021300
 NAME IGC0025A(R)                                                       00021400
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 252                          00021500
 NAME IGC0025B(R)                                                       00021600
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 253                          00021700
 NAME IGC0025C(R)                                                       00021800
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 254                          00021900
 NAME IGC0025D(R)                                                       00022000
 INCLUDE SYSLIB(IEFBR14)         DUMMY SVC 255                          00022100
 NAME IGC0025E(R)                                                       00022200
//*                                                                     00022300
//RECATLG  EXEC PGM=IDCAMS,REGION=1024K,COND=(0,NE,DEFCAT)              00022400
//* ***************************************************************** * 00022500
//* CATALOG SMP DATASETS                                              * 00022600
//* ***************************************************************** * 00022700
//STEPCAT  DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00022800
//SYSPRINT DD  SYSOUT=*                                                 00022900
//SYSIN    DD  *                                                        00023000
                                                                        00023100
 DEFINE NONVSAM (NAME (SYS1.ACMDLIB) DEVT(3350) VOLUME(SMP000) )        00023200
 DEFINE NONVSAM (NAME (SYS1.AGENLIB) DEVT(3350) VOLUME(SMP000) )        00023300
 DEFINE NONVSAM (NAME (SYS1.AHELP) DEVT(3350) VOLUME(SMP000) )          00023400
 DEFINE NONVSAM (NAME (SYS1.AIMAGE) DEVT(3350) VOLUME(SMP000) )         00023500
 DEFINE NONVSAM (NAME (SYS1.ALPALIB) DEVT(3350) VOLUME(SMP000) )        00023600
 DEFINE NONVSAM (NAME (SYS1.AMACLIB) DEVT(3350) VOLUME(SMP000) )        00023700
 DEFINE NONVSAM (NAME (SYS1.AMODGEN) DEVT(3350) VOLUME(SMP000) )        00023800
 DEFINE NONVSAM (NAME (SYS1.AOS00) DEVT(3350) VOLUME(SMP000) )          00023900
 DEFINE NONVSAM (NAME (SYS1.AOS03) DEVT(3350) VOLUME(SMP000) )          00024000
 DEFINE NONVSAM (NAME (SYS1.AOS04) DEVT(3350) VOLUME(SMP000) )          00024100
 DEFINE NONVSAM (NAME (SYS1.AOS05) DEVT(3350) VOLUME(SMP000) )          00024200
 DEFINE NONVSAM (NAME (SYS1.AOS06) DEVT(3350) VOLUME(SMP000) )          00024300
 DEFINE NONVSAM (NAME (SYS1.AOS07) DEVT(3350) VOLUME(SMP000) )          00024400
 DEFINE NONVSAM (NAME (SYS1.AOS11) DEVT(3350) VOLUME(SMP000) )          00024500
 DEFINE NONVSAM (NAME (SYS1.AOS12) DEVT(3350) VOLUME(SMP000) )          00024600
 DEFINE NONVSAM (NAME (SYS1.AOS20) DEVT(3350) VOLUME(SMP000) )          00024700
 DEFINE NONVSAM (NAME (SYS1.AOS21) DEVT(3350) VOLUME(SMP000) )          00024800
 DEFINE NONVSAM (NAME (SYS1.AOS24) DEVT(3350) VOLUME(SMP000) )          00024900
 DEFINE NONVSAM (NAME (SYS1.AOS26) DEVT(3350) VOLUME(SMP000) )          00025000
 DEFINE NONVSAM (NAME (SYS1.AOS29) DEVT(3350) VOLUME(SMP000) )          00025100
 DEFINE NONVSAM (NAME (SYS1.AOS32) DEVT(3350) VOLUME(SMP000) )          00025200
 DEFINE NONVSAM (NAME (SYS1.AOSA0) DEVT(3350) VOLUME(SMP000) )          00025300
 DEFINE NONVSAM (NAME (SYS1.AOSA1) DEVT(3350) VOLUME(SMP000) )          00025400
 DEFINE NONVSAM (NAME (SYS1.AOSB0) DEVT(3350) VOLUME(SMP000) )          00025500
 DEFINE NONVSAM (NAME (SYS1.AOSB3) DEVT(3350) VOLUME(SMP000) )          00025600
 DEFINE NONVSAM (NAME (SYS1.AOSBN) DEVT(3350) VOLUME(SMP000) )          00025700
 DEFINE NONVSAM (NAME (SYS1.AOSC2) DEVT(3350) VOLUME(SMP000) )          00025800
 DEFINE NONVSAM (NAME (SYS1.AOSC5) DEVT(3350) VOLUME(SMP000) )          00025900
 DEFINE NONVSAM (NAME (SYS1.AOSC6) DEVT(3350) VOLUME(SMP000) )          00026000
 DEFINE NONVSAM (NAME (SYS1.AOSCA) DEVT(3350) VOLUME(SMP000) )          00026100
 DEFINE NONVSAM (NAME (SYS1.AOSCD) DEVT(3350) VOLUME(SMP000) )          00026200
 DEFINE NONVSAM (NAME (SYS1.AOSCE) DEVT(3350) VOLUME(SMP000) )          00026300
 DEFINE NONVSAM (NAME (SYS1.AOSD0) DEVT(3350) VOLUME(SMP000) )          00026400
 DEFINE NONVSAM (NAME (SYS1.AOSD7) DEVT(3350) VOLUME(SMP000) )          00026500
 DEFINE NONVSAM (NAME (SYS1.AOSD8) DEVT(3350) VOLUME(SMP000) )          00026600
 DEFINE NONVSAM (NAME (SYS1.AOSG0) DEVT(3350) VOLUME(SMP000) )          00026700
 DEFINE NONVSAM (NAME (SYS1.AOSH1) DEVT(3350) VOLUME(SMP000) )          00026800
 DEFINE NONVSAM (NAME (SYS1.AOSH3) DEVT(3350) VOLUME(SMP000) )          00026900
 DEFINE NONVSAM (NAME (SYS1.AOST3) DEVT(3350) VOLUME(SMP000) )          00027000
 DEFINE NONVSAM (NAME (SYS1.AOST4) DEVT(3350) VOLUME(SMP000) )          00027100
 DEFINE NONVSAM (NAME (SYS1.AOSU0) DEVT(3350) VOLUME(SMP000) )          00027200
 DEFINE NONVSAM (NAME (SYS1.APARMLIB) DEVT(3350) VOLUME(SMP000) )       00027300
 DEFINE NONVSAM (NAME (SYS1.APROCLIB) DEVT(3350) VOLUME(SMP000) )       00027400
 DEFINE NONVSAM (NAME (SYS1.ASAMPLIB) DEVT(3350) VOLUME(SMP000) )       00027500
 DEFINE NONVSAM (NAME (SYS1.ATCAMMAC) DEVT(3350) VOLUME(SMP000) )       00027600
 DEFINE NONVSAM (NAME (SYS1.ATSOMAC) DEVT(3350) VOLUME(SMP000) )        00027700
 DEFINE NONVSAM (NAME (SYS1.AUADS) DEVT(3350) VOLUME(SMP000) )          00027800
 DEFINE NONVSAM (NAME (SYS1.HASPSRC) DEVT(3350) VOLUME(SMP000) )        00027900
 DEFINE NONVSAM (NAME (SYS1.SMPACDS) DEVT(3350) VOLUME(SMP000) )        00028000
 DEFINE NONVSAM (NAME (SYS1.SMPACRQ) DEVT(3350) VOLUME(SMP000) )        00028100
 DEFINE NONVSAM (NAME (SYS1.SMPMTS) DEVT(3350) VOLUME(SMP000) )         00028200
 DEFINE NONVSAM (NAME (SYS1.SMPPTS) DEVT(3350) VOLUME(SMP000) )         00028300
 DEFINE NONVSAM (NAME (SYS1.SMPSTS) DEVT(3350) VOLUME(SMP000) )         00028400
                                                                        00028500
//*        END OF SUBMITTED SYSGEN00 (2 OF 2)                           00028600
><                                                                      00028700
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        00028800
//*        END OF PRIMARY SYSGEN00                                      00028900
