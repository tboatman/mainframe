//SYSGEN02 JOB 'LINK JES2',                                             00010001
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        00020001
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00030001
//*                                                                     00040001
//JES2    EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL'                       00050001
//* ***************************************************************** * 00060001
//* LINK JES2 FROM DISTRIBUTION LIBRARY                               * 00070001
//* ***************************************************************** * 00080001
//SYSPRINT DD  SYSOUT=*                                                 00090001
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))             00100001
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                00110001
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=SHR                                  00120001
//SYSLIN   DD  *                                                        00130001
       ORDER HASPNUC                                                    00140001
       ORDER HASPRDR                                                    00150001
       ORDER HASPRDRO                                                   00160001
       ORDER HASPRSCN                                                   00170001
       ORDER HASPXEQ                                                    00180001
       ORDER HASPPRPU                                                   00190001
       ORDER HASPACCT                                                   00200001
       ORDER HASPMISC                                                   00210001
       ORDER HASPCON                                                    00220001
       ORDER HASPRTAM                                                   00230001
       ORDER HASPCOMM                                                   00240001
       ORDER HASPCOMA                                                   00250001
       ORDER HASPINIT(P)                                                00260001
       INCLUDE AOSH3(HASPNUC)                                           00270001
       INCLUDE AOSH3(HASPRDR)                                           00280001
       INCLUDE AOSH3(HASPXEQ)                                           00290001
       INCLUDE AOSH3(HASPPRPU)                                          00300001
       INCLUDE AOSH3(HASPACCT)                                          00310001
       INCLUDE AOSH3(HASPMISC)                                          00320001
       INCLUDE AOSH3(HASPCON)                                           00330001
       INCLUDE AOSH3(HASPRTAM)                                          00340001
       INCLUDE AOSH3(HASPCOMM)                                          00350001
       INCLUDE AOSH3(HASPINIT)                                          00360001
       PAGE    HASPINIT                                                 00370001
       NAME    HASJES20(R)                                              00380001
       INCLUDE AOSH3(HASPBLKS)                                          00390001
       NAME    HASPBLKS(R)                                              00400001
       INCLUDE AOSH3(HASPFMT0)                                          00410001
       NAME    HASPFMT0(R)                                              00420001
       INCLUDE AOSH3(HASPFMT1)                                          00430001
       NAME    HASPFMT1(R)                                              00440001
       INCLUDE AOSH3(HASPFMT2)                                          00450001
       NAME    HASPFMT2(R)                                              00460001
       INCLUDE AOSH3(HASPFMT3)                                          00470001
       NAME    HASPFMT3(R)                                              00480001
       INCLUDE AOSH3(HASPFMT4)                                          00490001
       NAME    HASPFMT4(R)                                              00500001
       INCLUDE AOSH3(HASPFMT5)                                          00510001
       NAME    HASPFMT5(R)                                              00520001
//*                                                                     00530001
//SSSM     EXEC PGM=IEWL,PARM='XREF,LIST,LET,NCAL'                      00540001
//SYSPRINT DD  SYSOUT=*                                                 00550001
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))             00560001
//SYSLMOD  DD  DSN=SYS1.LPALIB,DISP=SHR                                 00570001
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=SHR                                  00580001
//SYSLIN   DD  *                                                        00590001
       INCLUDE AOSH3(HASPSSSM)                                          00600001
       NAME    HASPSSSM(R)                                              00610001
//*                                                                     00620001
//JES2PARM EXEC PGM=IEBUPDTE,PARM=NEW                                   00630001
//* ***************************************************************** * 00640001
//* CREATE JES2 PARAMETERS IN SYS1.PARMLIB                            * 00650001
//* ***************************************************************** * 00660001
//SYSPRINT DD  SYSOUT=*                                                 00670001
//SYSUT2   DD  DISP=MOD,DSN=SYS1.PARMLIB                                00680001
//SYSIN    DD  *                                                        00690001
./ ADD NAME=JES2PM00,LIST=ALL                                           00700001
./ NUMBER NEW1=10,INCR=10                                               00710001
&BUFSIZE=3664                  BUFFER SIZE (5 PER TRACK ON 3350)        00720001
&CCOMCHR=$                     OPERATOR COMMAND IDENTIFIER              00730001
&CHKPT=MVS000                  CHECKPOINT DATASET ON MVS000             00740001
&CKPTIME=60                    CHECKPOINT EVERY 60 SECONDS              00750001
&DMNDSET=NO                    DEMAND SETUP OPTION                      00760001
&DSNPRFX=SYS1                  DATA SET PREFIX (SPOOL AND CKPT)         00770001
&ESTIME=0035                   ESTIMATED JOB TIME 35 MINUTES            00780001
&ESTLNCT=50                    ESTIMATED LINE COUNT 50,000 LINES        00790001
&ESTPUN=500                    ESTIMATED CARD COUNT 500 CARDS           00800001
*********************************************************************** 00810001
*                              INITIATORS                             * 00820001
*********************************************************************** 00830001
I1 START,CLASS=A               STANDARD                                 00840001
I2 START,CLASS=BA              QUEUED STANDARD                          00850001
I3 START,CLASS=S               SYSTEMS                                  00860001
I4 DRAIN,CLASS=DCBA                                                     00870001
I5 DRAIN,CLASS=ECBA                                                     00880001
I6 DRAIN,CLASS=FECBA                                                    00890001
INTRDR CLASS=A,NOHOLD,AUTH=0,PRIOINC=0,PRIOLIM=15                       00900001
&JCOPYLM=10                    MAX JOB OUTPUT COPIES                    00910001
&LINECT=61                     LINE PER PAGE LIMIT                      00920001
&MAXCLAS=5                     MAX JOB CLASSES PER INITIATOR            00930001
&MAXJOBS=128                   MAX JOBS IN JOB QUEUE                    00940001
&MAXPART=6                     MAX NUMBER OF BATCH INITIATORS           00950001
&MINJOES=100                   MIN NUMBER OF FREE JOB OUTPUT ELEMENTS   00960001
&MLBFSIZ=400                   MULTI-LEAVE BUFFER SIZE                  00970001
&MSGID=YES                     MSG ID OPTION                            00980001
&NOPRCCW=30                    PRINTER CCW LIMIT                        00990001
&NOPUCCW=45                    PUNCH CCW LIMIT                          01000001
&NUMACE=50                     AUTO COMMAND LIMIT                       01010001
&NUMBUF=128                    I/O BUFFER COUNT                         01020001
&NUMCLAS=3                     PRINTER SYSOUT CLASS LIMIT               01030001
&NUMCMBS=256                   NUMBER OF CONSOLE MESSAGE BUFFERS        01040001
&NUMDA=1                       NUMBER OF SPOOL VOLUMES                  01050001
&NUMINRS=5                     NUMBER OF INTERNAL READERS               01060001
&NUMJOES=512                   NUMBER OF JOB OUTPUT ELEMENTS            01070001
&NUMPRTS=3                     MAX NUMBER OF LOCAL PRINTERS             01080001
&NUMPUNS=1                     MAX NUMBER OF LOCAL PUNCHES              01090001
&NUMRDRS=1                     MAX NUMBER OF LOCAL READERS              01100001
&NUMSMFB=96                    NUMBER OF SMF BUFFERS                    01110001
&NUMTGV=3330                   TRACK GROUPS PER SPOOL VOLUME (3350)     01120001
&OUTPOPT=0                     ACTION FOR JOBS EXCEEDING OUTPUT         01130001
&OUTXS=5000                    MSG INTERVAL FOR EST OUTPUT              01140001
&PRIDCT=30                     PRINT LINES PER SEPARATOR PAGE           01150001
&PRIHIGH=10                    MAX PRIORITY AGING LIMIT                 01160001
&PRILOW=3                      MIN PRIORITY AGING LIMIT                 01170001
*********************************************************************** 01180001
*                              LOCAL PRINTERS                         * 01190001
*********************************************************************** 01200001
PRINTER1       CLASS=A,SEP,UNIT=00E,START                    1403       01210001
PRINTER2       CLASS=M,SEP,UCS=QN,FCB=6,UNIT=00F,START       3211       01220001
*********************************************************************** 01230001
&PRIOOPT=YES                   SUPPORT /*PRIORITY CARD                  01240001
&PRIRATE=24                    PRIORITY INCREMENT INTERVAL              01250001
&PRTBOPT=YES                   DOUBLE BUFFER LOCAL PRINTERS             01260001
&PRTFCB=6                      FCB DEFAULT                              01270001
&PRTRANS=NO                    DON'T TRANSLATE LOWER->UPPER CASE        01280001
&PRTYOPT=YES                   PRTY SUPPORT OPTION                      01300001
&PUNBOPT=YES                   DOUBLE BUFFER LOCAL PUNCHES              01310001
*********************************************************************** 01320001
*                              LOCAL PUNCHES                          * 01330001
*********************************************************************** 01340001
PUNCH1         CLASS=B,SEP,AUTO,NOPAUSE,UNIT=00D,START       2540P      01350001
*********************************************************************** 01360001
&RCOMCHR=$                     INSTREAM COMMAND IDENTIFIER              01370001
&RDROPSL=00000300051221E00011  TSU LOGON                                01380001
&RDROPST=00014395951221E00011  STARTED TASK                             01390001
&RDROPSU=30000013051221E00011  BATCH                                    01400001
******** BPPMMMMSSCCCRLAAAAEF      DEFAULT CONVERSION PARAMETER FIELDS  01410001
******** !! !   ! !  !!!   !+-F--- DEFAULT ALLOCATION MESSAGE LEVEL     01420001
******** !! !   ! !  !!!   !         0: None listed unless abend        01430001
******** !! !   ! !  !!!   !         1: Allocation/Termination          01440001
******** !! !   ! !  !!!   !            messages listed                 01450001
******** !! !   ! !  !!!   +-E---  DEFAULT MSGLEVEL parameter           01460001
******** !! !   ! !  !!!             0: JOB statement only              01470001
******** !! !   ! !  !!!             1: input, catalogued procedure     01480001
******** !! !   ! !  !!!                statements, substitutions       01490001
******** !! !   ! !  !!!             2: input statements only           01500001
******** !! !   ! !  !!+-AAAA----  Command Group Authorization          01510001
******** !! !   ! !  !!              E000 = allow all commands          01520001
******** !! !   ! !  !+-L--------  BLP Authorization                    01530001
******** !! !   ! !  !               0: disallow (BLP as NL)            01540001
******** !! !   ! !  !               1: allow (BLP as BLP)              01550001
******** !! !   ! !  +-R---------  Operator Commands in jobstream       01560001
******** !! !   ! !                0=execute without display            01570001
******** !! !   ! !                1=display and execute w/o confirm    01580001
******** !! !   ! !                2=display, require confirm           01590001
******** !! !   ! !                3=ignore                             01600001
******** !! !   ! +-CCC----------  Default step REGION= parameter       01610001
******** !! +---+----------------  Default step TIME= parameter MMMMSS  01620001
******** !+-PP-------------------  Default job priority                 01630001
******** !                           Ignored, specify 00                01640001
******** +-B---------------------  Account/programmer required          01650001
********                           0 = none required                    01660001
********                           1 = Account required                 01670001
********                           2 = Programmer name required         01680001
********                           3 = Both required                    01690001
*********************************************************************** 01700001
*                              LOCAL READERS                          * 01710001
*********************************************************************** 01720001
READER1        AUTH=0,CLASS=A,NOHOLD,MSGCLASS=A,UNIT=00C     2540R      01730001
&RECINCR=2                     RECORD ALTERNATION                       01740001
&RJOBOPT=5                     JOB CARD SCAN OPTION                     01750001
&RPRI(1)=6                     PRTY FOR ESTIMATED TIME                  01760001
&RPRI(2)=5                     PRTY FOR ESTIMATED TIME                  01770001
&RPRI(3)=4                     PRTY FOR ESTIMATED TIME                  01780001
&RPRI(4)=3                     PRTY FOR ESTIMATED TIME                  01790001
&RPRI(5)=2                     PRTY FOR ESTIMATED TIME                  01800001
&RPRI(6)=1                     PRTY FOR ESTIMATED TIME                  01810001
&RPRT(1)=5                     ESTIMATED TIME TABLE ENTRY               01820001
&RPRT(2)=10                    ESTIMATED TIME TABLE ENTRY               01830001
&RPRT(3)=59                    ESTIMATED TIME TABLE ENTRY               01840001
&RPRT(4)=120                   ESTIMATED TIME TABLE ENTRY               01850001
&RPRT(5)=279620                ESTIMATED TIME TABLE ENTRY               01860001
&RPRT(6)=279620                ESTIMATED TIME TABLE ENTRY               01870001
&RPS=YES                       RPS SUPPORT                              01880001
&SPOOL=SPOOL1                  SPOOL VOLUME SERIAL                      01890001
*********************************************************************** 01900001
*                              STC / TSU / BATCH JOB CLASSES          * 01910001
*********************************************************************** 01920001
&STC     NOJOURN,LOG,OUTPUT,NOTYPE6,NOTYPE26,NOUJP,NOUSO,PROCLIB=00,   C01930001
         PERFORM=1                                                      01940001
&TSU     NOJOURN,LOG,OUTPUT,NOTYPE6,NOTYPE26,NOUJP,NOUSO,PROCLIB=00,   C01950001
         PERFORM=2                                                      01960001
&A       NOJOURN,LOG,OUTPUT,PROCLIB=00,PERFORM=4  Low priority batch    01970001
&B       NOJOURN,LOG,OUTPUT,PROCLIB=00,PERFORM=1  Standard batch        01980001
&S       NOJOURN,LOG,OUTPUT,PROCLIB=00,PERFORM=3, Sysprog              C01990001
         CONVPARM=30000013051211E00011                                  02000001
STCMCLAS=Y                     STARTED TASK MESSAGE CLASS               02010001
&STDFORM=STD.                  DEFAULT FORMS ID                         02020001
&TGWARN=80                     THRESHOLD SPOOL UTILIZATION WARNING      02030001
&TIMEOPT=YES                   EXCEEDED EXEC TIME WARN                  02040001
&TIMEXS=1                      EXCEEDED EXEC TIME WARNING INTERVAL      02050001
&XLIN(1)=16777215              OUTPUT SELECT PRIORITY CATEGORY          02060001
&XLIN(2)=1000                                                           02070001
&XLIN(3)=2000                                                           02080001
&XLIN(4)=2500                                                           02090001
&XLIN(5)=6500                                                           02100001
&XLIN(6)=1000000                                                        02110001
&XLIN(7)=16777215                                                       02120001
TSUMCLAS=Y                     TSO USER MESSAGE CLASS                   02130001
*********************************************************************** 02140001
*                            OUTPUT CLASSES                           * 02150001
*********************************************************************** 02160001
$$A  PRINT,SYSOUT,NOHOLD,TRKCEL     STANDARD OUTPUT CLASS               02170001
$$B  PRINT,SYSOUT,NOHOLD,TRKCEL     STANDARD PUNCH OUTPUT CLASS         02180001
$$L  PRINT,SYSOUT,NOHOLD,TRKCEL     SYSLOG                              02190001
$$M  PRINT,SYSOUT,NOHOLD,TRKCEL     DOCUMENTATION                       02200001
$$X  PRINT,SYSOUT,HOLD              HELD BATCH                          02210001
$$Y  PRINT,SYSOUT,HOLD              HELD STC/TSU                        02220001
$$Z  PRINT,DUMMY,NOHOLD             TRASH (DISCARDED)                   02230001
*********************************************************************** 02240001
*                     AUTOMATIC OPERATOR COMMANDS                     * 02250001
*********************************************************************** 02260001
$T OSC3,D=J                                                             02270001
$T OSC3,D=T                                                             02280001
$T PRT1,F=STD.,C=6,F=AUTOM                                              02290001
$T PRT2,F=STD.,C=6,F=AUTOM                                              02300001
$VS,'MN JOBNAMES,T'                                                     02310001
$VS,'MN SESS,T'                                                         02320001
$VS,'V (100-104),ONLINE'                                                02330001
$VS,'$SPRT1'                                                            02340001
$VS,'$SPRT2'                                                            02350001
./ ENDUP                                                                02360001
//*                                                                     02370001
//JES2PRC  EXEC PGM=IEBUPDTE,PARM=NEW                                   02380001
//* ***************************************************************** * 02390001
//* CREATE JES2 EXECUTION PROCEDURE IN SYS1.PROCLIB                   * 02400001
//* ***************************************************************** * 02410001
//SYSPRINT DD  SYSOUT=*                                                 02420001
//SYSUT2   DD  DISP=MOD,DSN=SYS1.PROCLIB                                02430001
//SYSIN    DD  DATA,DLM='><'                                            02440001
./ ADD NAME=JES2,LIST=ALL                                               02450001
./ NUMBER NEW1=10,INCR=10                                               02460001
//JES2     PROC M=JES2PM00,                                             02470001
//             N=SYS1,                                                  02480001
//             L=LINKLIB,                                               02490001
//             U=3350,                                                  02500001
//             N1=SYS1,                                                 02510001
//             P=PARMLIB                                                02520001
//IEFPROC  EXEC PGM=HASJES20,                                           02530001
//             TIME=1440,                                               02540001
//             DPRTY=(15,15)                                            02550001
//STEPLIB  DD  UNIT=&U,DISP=SHR,DSN=&N..&L                              02560001
//PROC00   DD  DSN=&N..PROCLIB,DISP=SHR                                 02570001
//HASPPARM DD  DSN=&N1..&P(&M),DISP=SHR                                 02580001
//HASPLIST DD  DDNAME=IEFRDER                                           02590001
./ ENDUP                                                                02600001
><                                                                      02610001
//*                                                                     02620001
//IEFBR14  EXEC PGM=IEFBR14                                             02630001
//* ***************************************************************** * 02640001
//* Allocate and catalog JES2 checkpoint & spool datasets             * 02650001
//* ***************************************************************** * 02660001
//HASPCKPT DD  DSN=SYS1.HASPCKPT,                                       02670001
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  02680001
//             SPACE=(CYL,(50))                                         02690001
//HASPACE  DD  DSN=SYS1.HASPACE,                                        02700001
//             UNIT=3350,VOL=SER=SPOOL1,DISP=(,CATLG),                  02710001
//             SPACE=(CYL,(554))                                        02720001
//                                                                      02730001
