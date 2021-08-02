//SYSGEN03 JOB 'ADD PARMS/PROCS/PGMS',                                  00010001
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        00020001
/*JOBPARM LINES=100                                                     00030001
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00040001
//*                                                                     00050001
//* ***************************************************************** * 00060001
//* Some updates/additions are necessary to make using the new system * 00070001
//* easier/more user-friendly. This job begins making those changes.  * 00080001
//* ***************************************************************** * 00090001
//*                                                                     00100001
//ASMFCL   PROC MAC='SYS1.MACLIB',MAC1='SYS1.MACLIB'                    00110001
//ASM      EXEC PGM=IFOX00,PARM='OBJ,LIST,NOXREF',REGION=1024K          00120001
//SYSLIB   DD  DSN=&MAC,DISP=SHR                                        00130001
//         DD  DSN=&MAC1,DISP=SHR                                       00140001
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))                        00150001
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(300,50))                         00160001
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(300,50))                         00170001
//SYSPRINT DD  SYSOUT=*                                                 00180001
//SYSTERM  DD  SYSOUT=*                                                 00190001
//SYSPUNCH DD  DUMMY                                                    00200001
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(200,50)),             00210001
//            DISP=(NEW,PASS)                                           00220001
//LKED     EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL',REGION=128K,         00230001
//            COND=(8,LT,ASM)                                           00240001
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,DELETE)                           00250001
//         DD  DDNAME=SYSIN                                             00260001
//SYSLMOD  DD  DSN=&&GOSET(GO),UNIT=SYSDA,SPACE=(1024,(50,20,1)),       00270001
//            DISP=(MOD,PASS)                                           00280001
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))                          00290001
//SYSPRINT DD  SYSOUT=*                                                 00300001
//         PEND                                                         00310001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 00320001
//*                                                                     00330001
//BACKUP01 EXEC PGM=IEHPROGM                                            00340001
//* ***************************************************************** * 00350001
//* SYS1.PARMLIB: BACKUP IEASYS00 TO IEASYS99                         * 00360001
//*               BACKUP IEAAPF00 TO IEAAPF99                         * 00370001
//*               BACKUP SMFPRM00 TO SMFPRM99                         * 00380001
//*               BACKUP LNKLST00 TO LNKLST99                         * 00390001
//* ***************************************************************** * 00400001
//SYSPRINT DD  SYSOUT=*                                                 00410001
//MVSRES   DD  UNIT=3350,VOL=SER=MVSRES,DISP=OLD                        00420001
//SYSIN    DD  *                                                        00430001
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C00440001
               MEMBER=IEASYS00,NEWNAME=IEASYS99                         00450001
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C00460001
               MEMBER=IEAAPF00,NEWNAME=IEAAPF99                         00470001
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C00480001
               MEMBER=SMFPRM00,NEWNAME=SMFPRM99                         00490001
  RENAME DSNAME=SYS1.PARMLIB,VOL=3350=MVSRES,                          C00500001
               MEMBER=LNKLST00,NEWNAME=LNKLST99                         00510001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -BACKUP01 00520001
//*                                                                     00530001
//UPDATE02 EXEC PGM=IEBUPDTE,PARM=NEW                                   00540001
//* ***************************************************************** * 00550001
//* SYS1.PARMLIB: CREATE IEASYS00 (IPL PARMAMETERS)                   * 00560001
//*               CREATE IEAAPF00 (AUTHORIZED LOAD LIBRARIES)         * 00570001
//*               CREATE SMFPRM00 (SMF PARAMETERS)                    * 00580001
//*               CREATE IEALOD00 (LPA MODULES FIXED IN REAL STORAGE) * 00590001
//*               CREATE COMMND00 (AUTOMATIC COMMANDS AT IPL)         * 00600001
//*               CREATE SETPFK00 (SET PFKEYS ON CONSOLES AT IPL)     * 00610001
//*               CREATE LNKLST00 (LINKLIST CONCATENATION)            * 00620001
//*               CREATE VATLST00 (VOLUMES MOUNTED AT IPL TIME)       * 00630001
//*               CREATE PARMTZ (TIME OFFSET FROM GMT)                * 00640001
//* ***************************************************************** * 00650001
//SYSUT2   DD  DSN=SYS1.PARMLIB,DISP=OLD                                00660001
//SYSPRINT DD  SYSOUT=*                                                 00670001
//SYSIN    DD  *                                                        00680001
./ ADD NAME=IEASYS00,LIST=ALL                                           00690001
./ NUMBER NEW1=10,INCR=10                                               00700001
APF=00,             IEAAPF00 FOR AUTHORIZED PROGRAM LIBRARIES           00710001
BLDLF=00,           IEABLD00 FOR BLDL MODULES                           00720001
CMD=00,             COMMND00 FOR AUTO COMMANDS                          00730001
CSA=3072,           3072*1K BLOCKS FOR COMMON SERVICE AREA              00740001
CVIO,               CLEAR VIO AT IPL                                    00750001
HARDCPY=(015,       SYSLOG HARDCOPY DEVICE ADDRESS,                     00760001
         ALL,         RECORD ALL WTO/WTOR WITH ROUTE CODES,             00770001
         CMDS),       RECORD ALL COMMANDS AND RESPONSES                 00780001
LNK=00,             LNKLST00 FOR LINKLIST CONCATENATION                 00790001
MAXUSER=64,         SYS TASKS+INITIATORS+TSO USERS                      00800001
PAGNUM=(3,2),       ALLOW ADDITION OF 3 PAGE D/S & 2 SWAP D/S           00810001
PAGE=(SYS1.PAGELPA,                      PAGE DATASETS                 C00820001
      SYS1.PAGECSA,                                                    C00830001
      SYS1.PAGEL00),                                                    00840001
REAL=128,           128*1K BLOCKS OF VIRTUAL=REAL SPACE                 00850001
SMF=00,             SMFPRM00 FOR SMP PARAMETERS                         00860001
SQA=5,              5*64K SEGMENTS RESERVED FOR SYSTEM QUEUE SPACE      00870001
VAL=00,             VATLST00 FOR VOLUME ATTRIBUTE LIST                  00880001
VRREGN=64           64 BLOCKS OF V=R ALLOCATED TO VRREGN                00890001
./ ADD NAME=IEAAPF00,LIST=ALL                                           00900001
./ NUMBER NEW1=10,INCR=10                                               00910001
 SYS1.VTAMLIB MVSRES,         REQUIRED BY MVS                           00920001
 SYS1.INDMAC MVSRES,          REQUIRED BY MVS                           00930001
 SYS2.LINKLIB MVS000,         USER BATCH LINKLIB                        00940001
 SYSC.LINKLIB SYSCPK          COMPILER/TOOLS VOLUME                     00950001
./ ADD NAME=SMFPRM00,LIST=ALL                                           00960001
./ NUMBER NEW1=10,INCR=10                                               00970001
    OPT=2,      SYSTEM, JOB AND STEP DATA COLLECTION                    00980001
    EXT=YES,    USER EXITS ARE TO BE TAKEN                              00990001
    JWT=10,     MAXIMUM CONTINUOUS WAIT TIME IS 10 MINS                 01000001
    BUF=2000,   A 2000 BYTE BUFFER IS DEFINED                           01010001
    SID=HMVS,   SYSTEM ID IS HMVS                                       01020001
    OPI=NO,     DO NOT PERMIT OPERATOR INTERVENTION AT IPL              01030001
    MAN=ALL     USER AND SYSTEM RECORDS PERMITTED                       01040001
./ ADD NAME=IEALOD00,LIST=ALL                                           01050001
./ NUMBER NEW1=10,INCR=10                                               01060001
HASPSSSM,                        JES2                                   01070001
IEFW21SD                         IEFACTRT                               01080001
./ ADD NAME=COMMND00,LIST=ALL                                           01090001
./ NUMBER NEW1=10,INCR=10                                               01100001
COM='SEND 'AUTO COMMANDS IN COMMND00 BEING PROCESSED',CN=01'            01110001
COM='START SETPFKEY,M=00'                                               01120001
COM='START ZTIMER'                                                      01130001
./ ADD NAME=SETPFK00,LIST=ALL                                           01140001
./ NUMBER NEW1=10,INCR=10                                               01150001
* ------------------------------------------------------------------- * 01160001
* PROGRAM FUNCTION KEYS FOR CONSOLE 1                                 * 01170001
* ------------------------------------------------------------------- * 01180001
0101N D PFK                                                             01190001
0102                                                                    01200001
0103                                                                    01210001
0104                                                                    01220001
0105                                                                    01230001
0106                                                                    01240001
0107                                                                    01250001
0108                                                                    01260001
0109N D U,TAPE,ONLINE                                                   01270001
0110N D U,DASD,ONLINE                                                   01280001
0111Y V NET,INACT,ID=CUU0C_0;V NET,ACT,ID=CUU0C_0                       01290001
0112N K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,10                   01300001
* ------------------------------------------------------------------- * 01310001
* PROGRAM FUNCTION KEYS FOR CONSOLE 2                                 * 01320001
* ------------------------------------------------------------------- * 01330001
0201N D PFK                                                             01340001
0202                                                                    01350001
0203                                                                    01360001
0204                                                                    01370001
0205                                                                    01380001
0206                                                                    01390001
0207                                                                    01400001
0208                                                                    01410001
0209N D U,TAPE,ONLINE                                                   01420001
0210N D U,DASD,ONLINE                                                   01430001
0211Y V NET,INACT,ID=CUU0C_0;V NET,ACT,ID=CUU0C_0                       01440001
0212N K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,10                   01450001
* ------------------------------------------------------------------- * 01460001
* END OF SETPFK00 MEMBER                                              * 01470001
* ------------------------------------------------------------------- * 01480001
./ ADD NAME=LNKLST00,LIST=ALL                                           01490001
./ NUMBER NEW1=10,INCR=10                                               01500001
SYS1.LINKLIB,               IBM LOAD MODULE LIBRARY                     01510001
SYS2.LINKLIB,               USER LOAD MODULE LIBRARY                    01520001
SYS1.CMDLIB,                IBM TSO COMMAND LIBRARY                     01530001
SYS2.CMDLIB                 USER TSO COMMAND LIBRARY                    01540001
./ ADD NAME=VATLST00,LIST=ALL                                           01550001
./ NUMBER NEW1=10,INCR=10                                               01560001
MVSRES,0,2,3350    ,Y        SYSTEM RESIDENCE (PRIVATE)                 01570001
MVS000,0,2,3350    ,Y        SYSTEM DATASETS (PRIVATE)                  01580001
PAGE00,0,2,3350    ,Y        PAGE DATASETS (PRIVATE)                    01590001
PUB000,1,2,3380    ,N        PUBLIC DATASETS (PRIVATE)                  01600001
PUB001,1,2,3390    ,N        PUBLIC DATASETS (PRIVATE)                  01610001
SMP000,1,2,3350    ,N        DISTRIBUTION LIBRARIES (PRIVATE)           01620001
SORTW1,1,1,2314    ,N        SORT WORK (PUBLIC)                         01630001
SORTW2,1,1,2314    ,N        SORT WORK (PUBLIC)                         01640001
SORTW3,1,1,2314    ,N        SORT WORK (PUBLIC)                         01650001
SORTW4,1,1,2314    ,N        SORT WORK (PUBLIC)                         01660001
SORTW5,1,1,2314    ,N        SORT WORK (PUBLIC)                         01670001
SORTW6,1,1,2314    ,N        SORT WORK (PUBLIC)                         01680001
SPOOL1,0,2,3350    ,Y        JES2 QUEUES (PRIVATE)                      01690001
SYSCPK,1,2,3350    ,N        COMPILER/TOOLS (PRIVATE)                   01700001
WORK00,1,0,3350    ,N        WORK PACK (STORAGE)                        01710001
WORK01,1,0,3350    ,N        WORK PACK (STORAGE)                        01720001
./ ADD NAME=PARMTZ,LIST=ALL                                             01730001
./ NUMBER NEW1=10,INCR=10                                               01740001
W,05                   UNITED STATES, CENTRAL TIME ZONE                 01750001
./ ENDUP                                                                01760001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE02 01770001
//*                                                                     01780001
//ALLOC03  EXEC PGM=IEFBR14                                             01790001
//* ***************************************************************** * 01800001
//* DELETE SYS2.LPALIB FROM SMP000                                    * 01810001
//* ALLOCATE USER LINKLIB (SYS2.LINKLIB),                             * 01820001
//*          USER PROCLIB (SYS2.PROCLIB),                             * 01830001
//*          USER TSO COMMAND LIBRARY (SYS2.CMDLIB),  AND             * 01840001
//*          USER TSO HELP LIBRARY (SYS2.HELP) ON MVS000              * 01850001
//* ***************************************************************** * 01860001
/*                                                                      01870001
//LPALIB2  DD  DSN=SYS2.LOCAL.LPALIB,DISP=(OLD,DELETE)                  01880001
//LINKLIB  DD  DSN=SYS2.LINKLIB,                                        01890001
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  01900001
//             SPACE=(CYL,(20,1,323)),                                  01910001
//             DCB=(SYS1.LINKLIB)                                       01920001
//PROCLIB  DD  DSN=SYS2.PROCLIB,DISP=(,CATLG,),                         01930001
//             UNIT=3350,VOL=SER=MVS000,                                01940001
//             SPACE=(CYL,(10,5,50)),                                   01950001
//             DCB=(SYS1.PROCLIB)                                       01960001
//CMDLIB2  DD  DSN=SYS2.CMDLIB,                                         01970001
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  01980001
//             SPACE=(CYL,(20,,100)),                                   01990001
//             DCB=SYS1.CMDLIB                                          02000001
//HELP2    DD  DSN=SYS2.HELP,                                           02010001
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  02020001
//             SPACE=(CYL,(2,1,20)),                                    02030001
//             DCB=SYS1.HELP                                            02040001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ALLOC03 02050001
//*                                                                     02060001
//BACKUP04 EXEC PGM=IEBGENER                                            02070001
//* ***************************************************************** * 02080001
//* SYS1.PROCLIB: BACKUP JES2 TO JES20099                             * 02090001
//* ***************************************************************** * 02100001
//SYSPRINT DD  SYSOUT=*                                                 02110001
//SYSUT1   DD  DSN=SYS1.PROCLIB(JES2),DISP=SHR                          02120001
//SYSUT2   DD  DSN=SYS1.PROCLIB(JES20099),DISP=SHR                      02130001
//SYSIN    DD  DUMMY                                                    02140001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -BACKUP04 02150001
//*                                                                     02160001
//UPDATE05 EXEC PGM=IEBUPDTE,PARM=NEW                                   02170001
//* ***************************************************************** * 02180001
//* SYS1.PROCLIB: CREATE JES2 (JES2 STARTUP PROC) TO ADD SYS2.PROCLIB * 02190001
//* ***************************************************************** * 02200001
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD                                02210001
//SYSPRINT DD  SYSOUT=*                                                 02220001
//SYSIN    DD  DATA,DLM='><'                                            02230001
./ ADD NAME=JES2,LIST=ALL                                               02240001
./ NUMBER NEW1=10,INCR=10                                               02250001
//JES2     PROC M=JES2PM00,                                             02260001
//             N1=SYS1,                                                 02270001
//             N2=SYS2,                                                 02280001
//             L=LINKLIB,                                               02290001
//             U=3350,                                                  02300001
//             P=PARMLIB                                                02310001
//IEFPROC  EXEC PGM=HASJES20,                                           02320001
//             TIME=1440,                                               02330001
//             DPRTY=(15,15)                                            02340001
//STEPLIB  DD  UNIT=&U,DISP=SHR,DSN=&N1..&L                             02350001
//PROC00   DD  DSN=&N1..PROCLIB,DISP=SHR                                02360001
//         DD  DSN=&N2..PROCLIB,DISP=SHR                                02370001
//         DD  DSN=&N1..PROCLIB,DISP=SHR                                02380001
//HASPPARM DD  DSN=&N1..&P(&M),DISP=SHR                                 02390001
//HASPLIST DD  DDNAME=IEFRDER                                           02400001
./ ENDUP                                                                02410001
><                                                                      02420001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE05 02430001
//*                                                                     02440001
//UPDATE06 EXEC PGM=IEBUPDTE,PARM=NEW                                   02450001
//* ***************************************************************** * 02460001
//* SYS2.PROCLIB: ADD CLEARDMP (CLEAR DUMP DATASETS)                  * 02470001
//*                   CLEARERP (CLEAR HARDWARE ERROR DATASET)         * 02480001
//*                   COMPRESS (COMPRESS PDS)                         * 02490001
//*                   SMPASM (ASSEMBLE USERMODS)                      * 02500001
//*                   SMPASML (ASSEMBLE/LINK USERMODS)                * 02510001
//*                   SMPAPP (REJECT/APPLY USERMODS)                  * 02520001
//*                   SMPREC (RECEIVE USERMODS)                       * 02530001
//* ***************************************************************** * 02540001
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=MOD                                02550001
//SYSPRINT DD  SYSOUT=*                                                 02560001
//SYSIN    DD  DATA,DLM='><'                                            02570001
./ ADD NAME=CLEARDMP,LIST=ALL                                           02580001
./ NUMBER NEW1=10,INCR=10                                               02590001
//CLEARDMP PROC DD=00         SPECIFY DD={00|01|02}                     02600001
//* ***************************************************************** * 02610001
//* CLEAR DUMP DATASET                                                * 02620001
//* ***************************************************************** * 02630001
//EMPTY    EXEC PGM=IEBGENER                                            02640001
//SYSPRINT DD  SYSOUT=*                                                 02650001
//SYSIN    DD  DUMMY                                                    02660001
//SYSUT1   DD  DUMMY,DCB=(RECFM=U,LRECL=10,BLKSIZE=10)                  02670001
//SYSUT2   DD  DISP=SHR,DSN=SYS1.DUMP&DD                                02680001
./ ADD NAME=CLEARERP,LIST=ALL                                           02690001
./ NUMBER NEW1=10,INCR=10                                               02700001
//CLEARERP PROC                                                         02710001
//* ***************************************************************** * 02720001
//* CLEAR ENVIRONMENTAL ERROR RECORDER DATASET                        * 02730001
//* ***************************************************************** * 02740001
//EREP     EXEC PGM=IFCDIP00                                            02750001
//SERERDS  DD  DISP=SHR,DSN=SYS1.LOGREC                                 02760001
./ ADD NAME=COMPRESS,LIST=ALL                                           02770001
./ NUMBER NEW1=10,INCR=10                                               02780001
//COMPRESS PROC LIB='SYS2.LINKLIB',SOUT='*'                             02790001
//* ***************************************************************** * 02800001
//* COMPRESS LIBRARY IN PLACE                                         * 02810001
//* ***************************************************************** * 02820001
//COPY     EXEC PGM=IEBCOPY                                             02830001
//SYSPRINT DD  SYSOUT=&SOUT                                             02840001
//SYSIN    DD  DUMMY                                                    02850001
//SYSUT1   DD  DISP=SHR,DSN=&LIB                                        02860001
//SYSUT2   DD  DISP=SHR,DSN=&LIB                                        02870001
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(5,5))                             02880001
./ ADD NAME=SMPASM,LIST=ALL                                             02890001
./ NUMBER NEW1=10,INCR=10                                               02900001
//SMPASM   PROC M=MISSING                                               02910001
//* ***************************************************************** * 02920001
//* ASSEMBLE USER MOD                                                 * 02930001
//* ***************************************************************** * 02940001
//ASM      EXEC PGM=IFOX00,                                             02950001
//             REGION=4096K,                                            02960001
//             PARM='LIST,XREF(SHORT),DECK,NOOBJECT'                    02970001
//SYSPRINT DD  SYSOUT=*                                                 02980001
//SYSTERM  DD  SYSOUT=*                                                 02990001
//SYSPUNCH DD  DISP=SHR,DSN=SYS1.UMODOBJ(&M)                            03000001
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=32720               03010001
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                03020001
//         DD  DISP=SHR,DSN=SYS1.UMODMAC                                03030001
//         DD  DISP=SHR,DSN=SYS1.UMODSRC                                03040001
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             03050001
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             03060001
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             03070001
//SYSIN    DD  DISP=SHR,DSN=SYS1.UMODSRC(&M)                            03080001
./ ADD NAME=SMPASML,LIST=ALL                                            03090001
./ NUMBER NEW1=10,INCR=10                                               03100001
//SMPASML  PROC M=MISSING                                               03110001
//* ***************************************************************** * 03120001
//* ASSEMBLE AND LINK USER MOD                                        * 03130001
//* ***************************************************************** * 03140001
//ASM      EXEC PGM=IFOX00,                                             03150001
//             REGION=4096K,                                            03160001
//             PARM='LIST,XREF(SHORT),DECK,NOOBJECT'                    03170001
//SYSPRINT DD  SYSOUT=*                                                 03180001
//SYSTERM  DD  SYSOUT=*                                                 03190001
//SYSPUNCH DD  DISP=(,PASS),                                            03200001
//             UNIT=3350,                                               03210001
//             SPACE=(CYL,(1,1)),                                       03220001
//             DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB)                     03230001
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB,DCB=BLKSIZE=32720               03240001
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                03250001
//         DD  DISP=SHR,DSN=SYS1.UMODMAC                                03260001
//         DD  DISP=SHR,DSN=SYS1.UMODSRC                                03270001
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             03280001
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             03290001
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(2,1))                             03300001
//SYSIN    DD  DISP=SHR,DSN=SYS1.UMODSRC(&M)                            03310001
//LINK     EXEC PGM=IEWL,PARM='LIST,MAP,XREF',REGION=512K               03320001
//SYSPRINT DD  SYSOUT=*                                                 03330001
//SYSLIN   DD  DISP=(OLD,PASS),DSN=*.ASM.SYSPUNCH                       03340001
//         DD  DDNAME=SYSIN                                             03350001
//SYSLIB   DD  DISP=SHR,DSN=SYS1.UMODOBJ                                03360001
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.UMODLIB(&M)                            03370001
./ ADD NAME=SMPAPP                                                      03380001
./ NUMBER NEW1=10,INCR=10                                               03390001
//* ***************************************************************** * 03400001
//* APPLY/RESTORE USER MOD                                            * 03410001
//* ***************************************************************** * 03420001
//SMPAPP  PROC WORK=3350,              WORK UNIT                        03430001
//             TUNIT=3350,             TLIB UNIT                        03440001
//             TVOL=WORK00             TLIB VOLUME                      03450001
//HMASMP  EXEC PGM=HMASMP,PARM='DATE=U',REGION=5120K,TIME=1439          03460001
//SYSUT1   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        03470001
//SYSUT2   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        03480001
//SYSUT3   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        03490001
//SYSUT4   DD  UNIT=&WORK,SPACE=(80,(2,2))                              03500001
//SYSPRINT DD  SYSOUT=*                                                 03510001
//ASMPRINT DD  SYSOUT=*                                                 03520001
//CMPPRINT DD  SYSOUT=*                                                 03530001
//COPPRINT DD  SYSOUT=*                                                 03540001
//LKDPRINT DD  SYSOUT=*                                                 03550001
//E37PRINT DD  SYSOUT=*                                                 03560001
//UPDPRINT DD  SYSOUT=*                                                 03570001
//ZAPPRINT DD  SYSOUT=*                                                 03580001
//*************************** SMP DATASETS *********************        03590001
//SMPOUT   DD  SYSOUT=*                                                 03600001
//SMPLOG   DD  DUMMY                                                    03610001
//SMPTLIB  DD  DISP=OLD,UNIT=&TUNIT,VOL=SER=&TVOL                       03620001
//SYSLIB   DD  DISP=SHR,DSN=SYS1.SMPMTS,DCB=BLKSIZE=32720               03630001
//         DD  DISP=SHR,DSN=SYS1.SMPSTS                                 03640001
//         DD  DISP=SHR,DSN=SYS1.MACLIB                                 03650001
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                03660001
//         DD  DISP=SHR,DSN=SYS1.AMACLIB                                03670001
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS                                03680001
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ                                03690001
//SMPCDS   DD  DISP=SHR,DSN=SYS1.SMPCDS                                 03700001
//SMPCRQ   DD  DISP=SHR,DSN=SYS1.SMPCRQ                                 03710001
//SMPMTS   DD  DISP=SHR,DSN=SYS1.SMPMTS                                 03720001
//SMPPTS   DD  DISP=SHR,DSN=SYS1.SMPPTS                                 03730001
//SMPSTS   DD  DISP=SHR,DSN=SYS1.SMPSTS                                 03740001
//SMPSCDS  DD  DISP=SHR,DSN=SYS1.SMPSCDS                                03750001
//SMPWRK1  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      03760001
//             LRECL=80)                                                03770001
//SMPWRK2  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      03780001
//             LRECL=80)                                                03790001
//SMPWRK3  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      03800001
//             LRECL=80)                                                03810001
//SMPWRK4  DD  UNIT=&WORK,SPACE=(CYL,(1,10,84)),DCB=(BLKSIZE=3120,      03820001
//             LRECL=80)                                                03830001
//SMPWRK5  DD  UNIT=&WORK,SPACE=(CYL,(30,10,250))                       03840001
//*************************** DLIB DATASETS *********************       03850001
//************************* NEEDED ON RESTORE *******************       03860001
//ACMDLIB  DD  DISP=SHR,DSN=SYS1.ACMDLIB                                03870001
//AGENLIB  DD  DISP=SHR,DSN=SYS1.AGENLIB                                03880001
//AHELP    DD  DISP=SHR,DSN=SYS1.AHELP                                  03890001
//AIMAGE   DD  DISP=SHR,DSN=SYS1.AIMAGE                                 03900001
//ALPALIB  DD  DISP=SHR,DSN=SYS1.ALPALIB                                03910001
//AMACLIB  DD  DISP=SHR,DSN=SYS1.AMACLIB                                03920001
//AMODGEN  DD  DISP=SHR,DSN=SYS1.AMODGEN                                03930001
//AOS00    DD  DISP=SHR,DSN=SYS1.AOS00                                  03940001
//AOS03    DD  DISP=SHR,DSN=SYS1.AOS03                                  03950001
//AOS04    DD  DISP=SHR,DSN=SYS1.AOS04                                  03960001
//AOS05    DD  DISP=SHR,DSN=SYS1.AOS05                                  03970001
//AOS06    DD  DISP=SHR,DSN=SYS1.AOS06                                  03980001
//AOS07    DD  DISP=SHR,DSN=SYS1.AOS07                                  03990001
//AOS11    DD  DISP=SHR,DSN=SYS1.AOS11                                  04000001
//AOS12    DD  DISP=SHR,DSN=SYS1.AOS12                                  04010001
//AOS20    DD  DISP=SHR,DSN=SYS1.AOS20                                  04020001
//AOS21    DD  DISP=SHR,DSN=SYS1.AOS21                                  04030001
//AOS24    DD  DISP=SHR,DSN=SYS1.AOS24                                  04040001
//AOS26    DD  DISP=SHR,DSN=SYS1.AOS26                                  04050001
//AOS29    DD  DISP=SHR,DSN=SYS1.AOS29                                  04060001
//AOS32    DD  DISP=SHR,DSN=SYS1.AOS32                                  04070001
//AOSA0    DD  DISP=SHR,DSN=SYS1.AOSA0                                  04080001
//AOSA1    DD  DISP=SHR,DSN=SYS1.AOSA1                                  04090001
//AOSB0    DD  DISP=SHR,DSN=SYS1.AOSB0                                  04100001
//AOSB3    DD  DISP=SHR,DSN=SYS1.AOSB3                                  04110001
//AOSBN    DD  DISP=SHR,DSN=SYS1.AOSBN                                  04120001
//AOSC2    DD  DISP=SHR,DSN=SYS1.AOSC2                                  04130001
//AOSC5    DD  DISP=SHR,DSN=SYS1.AOSC5                                  04140001
//AOSC6    DD  DISP=SHR,DSN=SYS1.AOSC6                                  04150001
//AOSCA    DD  DISP=SHR,DSN=SYS1.AOSCA                                  04160001
//AOSCD    DD  DISP=SHR,DSN=SYS1.AOSCD                                  04170001
//AOSCE    DD  DISP=SHR,DSN=SYS1.AOSCE                                  04180001
//AOSD0    DD  DISP=SHR,DSN=SYS1.AOSD0                                  04190001
//AOSD7    DD  DISP=SHR,DSN=SYS1.AOSD7                                  04200001
//AOSD8    DD  DISP=SHR,DSN=SYS1.AOSD8                                  04210001
//AOSG0    DD  DISP=SHR,DSN=SYS1.AOSG0                                  04220001
//AOSH1    DD  DISP=SHR,DSN=SYS1.AOSH1                                  04230001
//AOSH3    DD  DISP=SHR,DSN=SYS1.AOSH3                                  04240001
//AOST3    DD  DISP=SHR,DSN=SYS1.AOST3                                  04250001
//AOST4    DD  DISP=SHR,DSN=SYS1.AOST4                                  04260001
//AOSU0    DD  DISP=SHR,DSN=SYS1.AOSU0                                  04270001
//APARMLIB DD  DISP=SHR,DSN=SYS1.APARMLIB                               04280001
//APROCLIB DD  DISP=SHR,DSN=SYS1.APROCLIB                               04290001
//ASAMPLIB DD  DISP=SHR,DSN=SYS1.ASAMPLIB                               04300001
//ATCAMMAC DD  DISP=SHR,DSN=SYS1.ATCAMMAC                               04310001
//ATSOMAC  DD  DISP=SHR,DSN=SYS1.ATSOMAC                                04320001
//AUADS    DD  DISP=SHR,DSN=SYS1.AUADS                                  04330001
//HASPSRC  DD  DISP=SHR,DSN=SYS1.HASPSRC                                04340001
//*************************** TARGET DATASETS *******************       04350001
//*************************** NEEDED FOR APPLY ******************       04360001
//CMDLIB   DD  DISP=SHR,DSN=SYS1.CMDLIB                                 04370001
//HELP     DD  DISP=SHR,DSN=SYS1.HELP                                   04380001
//IMAGELIB DD  DISP=SHR,DSN=SYS1.IMAGELIB                               04390001
//IMAGE    DD  DISP=SHR,DSN=SYS1.IMAGELIB                               04400001
//LPALIB   DD  DISP=SHR,DSN=SYS1.LPALIB                                 04410001
//LINKLIB  DD  DISP=SHR,DSN=SYS1.LINKLIB                                04420001
//NUCLEUS  DD  DISP=SHR,DSN=SYS1.NUCLEUS                                04430001
//MACLIB   DD  DISP=SHR,DSN=SYS1.MACLIB                                 04440001
//PARMLIB  DD  DISP=SHR,DSN=SYS1.PARMLIB                                04450001
//PROCLIB  DD  DISP=SHR,DSN=SYS1.PROCLIB                                04460001
//SAMPLIB  DD  DISP=SHR,DSN=SYS1.SAMPLIB                                04470001
//SVCLIB   DD  DISP=SHR,DSN=SYS1.SVCLIB                                 04480001
//TCOMMAC  DD  DISP=SHR,DSN=SYS1.TCOMMAC                                04490001
//TELCMLIB DD  DISP=SHR,DSN=SYS1.TELCMLIB                               04500001
//UADS     DD  DISP=SHR,DSN=SYS1.UADS                                   04510001
//UMODLIB  DD  DISP=SHR,DSN=SYS1.UMODLIB                                04520001
//UMODOBJ  DD  DISP=SHR,DSN=SYS1.UMODOBJ                                04530001
//VTAMLIB  DD  DISP=SHR,DSN=SYS1.VTAMLIB                                04540001
./ ADD NAME=SMPREC                                                      04550001
./ NUMBER NEW1=10,INCR=10                                               04560001
//SMPREC  PROC WORK=3350,              WORK UNIT                        04570001
//             TUNIT=3350,             TLIB UNIT                        04580001
//             TVOL=WORK00             TLIB VOLUME                      04590001
//* ***************************************************************** * 04600001
//* RECEIVE USER MOD                                                  * 04610001
//* ***************************************************************** * 04620001
//HMASMP  EXEC PGM=HMASMP,PARM='DATE=U',REGION=5120K,TIME=1440          04630001
//SYSUT1   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        04640001
//SYSUT2   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        04650001
//SYSUT3   DD  UNIT=&WORK,SPACE=(1700,(600,100))                        04660001
//SYSUT4   DD  UNIT=&WORK,SPACE=(80,(2,2))                              04670001
//SYSPRINT DD  SYSOUT=*                                                 04680001
//ASMPRINT DD  SYSOUT=*                                                 04690001
//CMPPRINT DD  SYSOUT=*                                                 04700001
//COPPRINT DD  SYSOUT=*                                                 04710001
//LKDPRINT DD  SYSOUT=*                                                 04720001
//E37PRINT DD  SYSOUT=*                                                 04730001
//UPDPRINT DD  SYSOUT=*                                                 04740001
//ZAPPRINT DD  SYSOUT=*                                                 04750001
//*************************** SMP DATASETS *********************        04760001
//SMPOUT   DD  SYSOUT=*                                                 04770001
//SMPLOG   DD  DUMMY                                                    04780001
//SMPTLIB  DD  DISP=OLD,UNIT=&TUNIT,VOL=SER=&TVOL                       04790001
//SYSLIB   DD  DISP=SHR,DSN=SYS1.SMPMTS,DCB=BLKSIZE=32720               04800001
//         DD  DISP=SHR,DSN=SYS1.SMPSTS                                 04810001
//         DD  DISP=SHR,DSN=SYS1.MACLIB                                 04820001
//         DD  DISP=SHR,DSN=SYS1.AMODGEN                                04830001
//         DD  DISP=SHR,DSN=SYS1.AMACLIB                                04840001
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS                                04850001
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ                                04860001
//SMPCDS   DD  DISP=SHR,DSN=SYS1.SMPCDS                                 04870001
//SMPCRQ   DD  DISP=SHR,DSN=SYS1.SMPCRQ                                 04880001
//SMPMTS   DD  DISP=SHR,DSN=SYS1.SMPMTS                                 04890001
//SMPPTS   DD  DISP=SHR,DSN=SYS1.SMPPTS                                 04900001
//SMPSTS   DD  DISP=SHR,DSN=SYS1.SMPSTS                                 04910001
//SMPSCDS  DD  DISP=SHR,DSN=SYS1.SMPSCDS                                04920001
//SMPWRK1  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      04930001
//             LRECL=80)                                                04940001
//SMPWRK2  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      04950001
//             LRECL=80)                                                04960001
//SMPWRK3  DD  UNIT=&WORK,SPACE=(CYL,(5,10,84)),DCB=(BLKSIZE=3120,      04970001
//             LRECL=80)                                                04980001
//SMPWRK4  DD  UNIT=&WORK,SPACE=(CYL,(1,10,84)),DCB=(BLKSIZE=3120,      04990001
//             LRECL=80)                                                05000001
//SMPWRK5  DD  UNIT=&WORK,SPACE=(CYL,(30,10,250))                       05010001
./ ENDUP                                                                05020001
><                                                                      05030001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE06 05040001
//*                                                                     05050001
//COPY07   EXEC PGM=IEBCOPY,PARM=NEW                                    05060001
//* ***************************************************************** * 05070001
//* ALLOCATE DATASETS FOR ADDING USER MODS                            * 05080001
//* COPY SMP DATASETS CREATING ENVIRONMENT FOR TARGET SYSTEM          * 05090001
//* ***************************************************************** * 05100001
//UMODMAC  DD  DISP=(NEW,CATLG,DELETE),                                 05110001
//             DSN=SYS1.UMODMAC,                                        05120001
//             UNIT=3350,VOL=SER=SMP000,                                05130001
//             SPACE=(CYL,(1,1,10)),                                    05140001
//             DCB=SYS1.MACLIB                                          05150001
//UMODCNTL DD  DISP=(NEW,CATLG,DELETE),                                 05160001
//             DSN=SYS1.UMODCNTL,                                       05170001
//             UNIT=3350,VOL=SER=SMP000,                                05180001
//             SPACE=(CYL,(1,1,10)),                                    05190001
//             DCB=SYS1.MACLIB                                          05200001
//UMODSRC  DD  DISP=(NEW,CATLG,DELETE),                                 05210001
//             DSN=SYS1.UMODSRC,                                        05220001
//             UNIT=3350,VOL=SER=SMP000,                                05230001
//             SPACE=(CYL,(1,1,10)),                                    05240001
//             DCB=SYS1.MACLIB                                          05250001
//UMODOBJ  DD  DISP=(NEW,CATLG,DELETE),                                 05260001
//             DSN=SYS1.UMODOBJ,                                        05270001
//             UNIT=3350,VOL=SER=SMP000,                                05280001
//             SPACE=(CYL,(1,1,10)),                                    05290001
//             DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB)                     05300001
//UMODLIB  DD  DISP=(NEW,CATLG,DELETE),                                 05310001
//             DSN=SYS1.UMODLIB,                                        05320001
//             UNIT=3350,VOL=SER=SMP000,                                05330001
//             SPACE=(CYL,(1,1,10)),                                    05340001
//             DCB=SYS1.LINKLIB                                         05350001
//SMPACDS  DD  DISP=SHR,DSN=SYS1.SMPACDS                                05360001
//SMPACRQ  DD  DISP=SHR,DSN=SYS1.SMPACRQ                                05370001
//SMPCDS   DD  UNIT=3350,DSN=SYS1.SMPCDS,DISP=(,CATLG),                 05380001
//             VOL=(,RETAIN,SER=MVS000),SPACE=(CYL,(90,5,5000)),        05390001
//             DCB=SYS1.SMPACDS                                         05400001
//SMPCRQ   DD  UNIT=3350,DSN=SYS1.SMPCRQ,DISP=(,CATLG),                 05410001
//             VOL=(,RETAIN,SER=MVS000),SPACE=(1680,(400,5,184)),       05420001
//             DCB=SYS1.SMPACRQ                                         05430001
//SMPSCDS  DD  UNIT=3350,DSN=SYS1.SMPSCDS,DISP=(,CATLG),                05440001
//             VOL=(,RETAIN,SER=MVS000),SPACE=(CYL,(1,1,71)),           05450001
//             DCB=(RECFM=FB,BLKSIZE=3120,LRECL=80)                     05460001
//SYSUT3   DD  UNIT=3350,SPACE=(CYL,(20,10))                            05470001
//SYSUT4   DD  UNIT=3350,SPACE=(CYL,(20,10))                            05480001
//SYSPRINT DD  SYSOUT=*                                                 05490001
//SYSIN    DD  *                                                        05500001
 COPY INDD=SMPACDS,OUTDD=SMPCDS,LIST=NO                                 05510001
 COPY INDD=SMPACRQ,OUTDD=SMPCRQ,LIST=NO                                 05520001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -COPY07 05530001
//*                                                                     05540001
//UPDATE08 EXEC PGM=IEBUPDTE,PARM=NEW                                   05550001
//* ***************************************************************** * 05560001
//* ADD SMPASM, SMPASML, SMPAPP, AND SMPREC (THAT WERE JUST ADDED TO  * 05570001
//* SYS2.PROCLIB ABOVE) INTO SYS1.PROCLIB ON THE STARTER SYSTEM.      * 05580001
//* ***************************************************************** * 05590001
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD,                               05600001
//             UNIT=3330,VOL=SER=START1                                 05610001
//SYSPRINT DD  SYSOUT=*                                                 05620001
//SYSIN    DD  *                                                        05630001
./ ADD NAME=SMPASM,LIST=ALL                                             05640001
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPASM)                        05650001
//         DD  *                                                        05660001
./ ADD NAME=SMPASML,LIST=ALL                                            05670001
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPASML)                       05680001
//         DD  *                                                        05690001
./ ADD NAME=SMPAPP,LIST=ALL                                             05700001
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPAPP)                        05710001
//         DD  *                                                        05720001
./ ADD NAME=SMPREC,LIST=ALL                                             05730001
//         DD  DISP=SHR,DSN=SYS2.PROCLIB(SMPREC)                        05740001
//         DD  *                                                        05750001
./ ENDUP                                                                05760001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE08 05770001
//*                                                                     05780001
//SUBMIT09 EXEC PGM=IEBGENER,COND=(0,NE)                                05790001
//* ***************************************************************** * 05800001
//* SUBMIT JOB TO ESTABLISH MVS3.8j SYSTEM CONTENTS IN SMP.           * 05810001
//* ***************************************************************** * 05820001
//SYSIN    DD  DUMMY                                                    05830001
//SYSPRINT DD  DUMMY                                                    05840001
//SYSUT1   DD  DATA,DLM='@@'                                            05850001
//SYSGEN03 JOB 'JCLIN-STAGE1/JES2',                                     05860001
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        05870001
/*JOBPARM LINES=100                                                     05880001
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    05890001
//*                                                                     05900001
//SMPAPP   EXEC SMPAPP                                                  05910001
//* ***************************************************************** * 05920001
//* RUN JCLIN WITH STAGE1 JOBSTREAM + JES2 LINK JOBSTREAM.            * 05930001
//* ***************************************************************** * 05940001
//SMPCNTL  DD  *                                                        05950001
  JCLIN .                                                               05960001
  LIST CDS SYS XREF .                                                   05970001
//SMPJCLIN DD  DISP=SHR,DSN=SYS1.STAGE1.OUTPUT,                         05980001
//             UNIT=3350,VOL=SER=WORK01                                 05990001
//         DD  DATA,DLM='><'                                            06000001
//JES2    EXEC PGM=IEWL,PARM='XREF,LET,LIST,NCAL'                       06010001
//SYSPRINT DD  SYSOUT=*                                                 06020001
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))             06030001
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                06040001
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=SHR                                  06050001
//SYSLIN   DD  *                                                        06060001
       ORDER HASPNUC                                                    06070001
       ORDER HASPRDR                                                    06080001
       ORDER HASPRDRO                                                   06090001
       ORDER HASPRSCN                                                   06100001
       ORDER HASPXEQ                                                    06110001
       ORDER HASPPRPU                                                   06120001
       ORDER HASPACCT                                                   06130001
       ORDER HASPMISC                                                   06140001
       ORDER HASPCON                                                    06150001
       ORDER HASPRTAM                                                   06160001
       ORDER HASPCOMM                                                   06170001
       ORDER HASPCOMA                                                   06180001
       ORDER HASPINIT(P)                                                06190001
       INCLUDE AOSH3(HASPNUC)                                           06200001
       INCLUDE AOSH3(HASPRDR)                                           06210001
       INCLUDE AOSH3(HASPXEQ)                                           06220001
       INCLUDE AOSH3(HASPPRPU)                                          06230001
       INCLUDE AOSH3(HASPACCT)                                          06240001
       INCLUDE AOSH3(HASPMISC)                                          06250001
       INCLUDE AOSH3(HASPCON)                                           06260001
       INCLUDE AOSH3(HASPRTAM)                                          06270001
       INCLUDE AOSH3(HASPCOMM)                                          06280001
       INCLUDE AOSH3(HASPINIT)                                          06290001
       PAGE    HASPINIT                                                 06300001
       NAME    HASJES20(R)                                              06310001
       INCLUDE AOSH3(HASPBLKS)                                          06320001
       NAME    HASPBLKS(R)                                              06330001
       INCLUDE AOSH3(HASPFMT0)                                          06340001
       NAME    HASPFMT0(R)                                              06350001
       INCLUDE AOSH3(HASPFMT1)                                          06360001
       NAME    HASPFMT1(R)                                              06370001
       INCLUDE AOSH3(HASPFMT2)                                          06380001
       NAME    HASPFMT2(R)                                              06390001
       INCLUDE AOSH3(HASPFMT3)                                          06400001
       NAME    HASPFMT3(R)                                              06410001
       INCLUDE AOSH3(HASPFMT4)                                          06420001
       NAME    HASPFMT4(R)                                              06430001
       INCLUDE AOSH3(HASPFMT5)                                          06440001
       NAME    HASPFMT5(R)                                              06450001
//*                                                                     06460001
//SSSM     EXEC PGM=IEWL,PARM='XREF,LIST,LET,NCAL'                      06470001
//SYSPRINT DD  SYSOUT=*                                                 06480001
//SYSUT1   DD  DSN=&&SYSUT1,UNIT=SYSDA,SPACE=(1024,(50,20))             06490001
//SYSLMOD  DD  DSN=SYS1.LPALIB,DISP=SHR                                 06500001
//AOSH3    DD  DSN=SYS1.AOSH3,DISP=SHR                                  06510001
//SYSLIN   DD  *                                                        06520001
       INCLUDE AOSH3(HASPSSSM)                                          06530001
       NAME    HASPSSSM(R)                                              06540001
><                                                                      06550001
//*                                                                     06560001
//*        END OF SUBMITTED SYSGEN03                                    06570001
@@                                                                      06580001
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        06590001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -SUBMIT09 06600001
//*                                                                     06610001
//UPDATE10 EXEC PGM=IEBUPDTE,PARM=NEW                                   06620001
//* ***************************************************************** * 06630001
//* Installs 'HELP' for TSO command CLS into SYS2.HELP                * 06640001
//* ***************************************************************** * 06650001
//SYSPRINT DD  SYSOUT=*                                                 06660001
//SYSUT2   DD  DISP=SHR,DSN=SYS2.HELP                                   06670001
//SYSIN    DD  *                                                        06680001
./ ADD NAME=CLS,LIST=ALL                                                06690001
./ NUMBER NEW1=10,INCR=10                                               06700001
)F CLS  FUNCTIONS -                                                     06710001
  CLEAR THE SCREEN ON A 3270 TYPE TERMINAL                              06720001
)X SYNTAX -                                                             06730001
         CLS                                                            06740001
)O OPERANDS -                                                           06750001
  NO OPERANDS ON CLS COMMAND.                                           06760001
./ ENDUP                                                                06770001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE10 06780001
//*                                                                     06790001
//ASMFCL11 EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM,NOXREF'           06800001
//* ***************************************************************** * 06810001
//* Installs TSO command CLS into SYS2.CMDLIB                         * 06820001
//* ***************************************************************** * 06830001
//ASM.SYSIN DD *                                                        06840001
*                              SOURCE: CBT V430 FILE 300                06850001
         TITLE 'CLRSCRN'                                                06860001
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 06870001
         SPACE 1                                                        06880001
CLRSCRN  CSECT                                                          06890001
         USING CLRSCRN,R15                                              06900001
         ST    R14,RETURN              SAVE                             06910001
         LR    R14,R15                 BASE REGISTER                    06920001
         DROP  R15                                                      06930001
         USING CLRSCRN,R14                                              06940001
         GTSIZE                                                         06950001
         LA    R15,20                  RETURN CODE 20 - NOT 3270 DEVICE 06960001
         LTR   R0,R0                   Q. 3270 TERMINAL                 06970001
         BZ    EXIT                    ...NO                            06980001
         MR    R0,R0                   SCREEN END = LINES * LINE LENGTH 06990001
         BCTR  R1,0                    ADJUST RELATIVE                  07000001
         D     R0,=F'64'               MODULO 64                        07010001
         N     R1,=X'0000003F'         INSURANCE                        07020001
         STC   R0,SCREND+1             SCREEN OFFSET MODULO 64 COLUMN   07030001
         STC   R1,SCREND               SCREEN OFFSET MODULO 64 ROW      07040001
         TR    SCREND,SCRPOS           TRANSLATE TO 3270 ADDRESS        07050001
         TPUT  SCRCNTL,LSCRCNTL,FULLSCR,,HOLD                           07060001
EXIT     DS    0H                                                       07070001
         L     R14,RETURN                                               07080001
         BR    14                      RETURN                           07090001
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 07100001
         TITLE 'CLRSCRN - DATA AREA'                                    07110001
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 07120001
         SPACE 1                                                        07130001
RETURN   DS    F                                                        07140001
SCRCNTL  DS    0CL11                   3270 FULL SCREEN CONTROL         07150001
         DC    AL1(WCC)                3270 WRITE CONTROL               07160001
         DC    AL1(SBA)                3270 SET BUFFER ADDRESS COMMAND  07170001
         DC    X'4040'                 3270 ADDRESS 0                   07180001
         DC    AL1(IC)                 INSERT CURSOR                    07190001
         DC    AL1(RA)                 3270 REPEAT TO ADDRESS COMMAND   07200001
SCREND   DC    X'5D7F'                 3270 ADDRESS - REPEAT END        07210001
         DC    X'00'                   REPEAT CHARACTER                 07220001
LSCRCNTL EQU   *-SCRCNTL                                                07230001
SCRPOS   DS    0H                      MODULO 64 TRANSLATE TABLE        07240001
         DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'                      07250001
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'                      07260001
         DC    X'6061E2E3E4E5E6E7E8E96A6B6C6D6E6F'                      07270001
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'                      07280001
         LTORG                                                          07290001
R0       EQU   0                                                        07300001
R1       EQU   1                                                        07310001
R2       EQU   2                                                        07320001
R3       EQU   3                                                        07330001
R4       EQU   4                                                        07340001
R5       EQU   5                                                        07350001
R6       EQU   6                                                        07360001
R7       EQU   7                                                        07370001
R8       EQU   8                                                        07380001
R9       EQU   9                                                        07390001
R10      EQU   10                                                       07400001
R11      EQU   11                                                       07410001
R12      EQU   12                                                       07420001
R13      EQU   13                                                       07430001
R14      EQU   14                                                       07440001
R15      EQU   15                                                       07450001
WCC      EQU   X'C3'                   KEYBOARD RESTORE,                07460001
*                                          RESET MODIFIED DATA TAG      07470001
SF       EQU   X'1D'                   START FIELD                      07480001
SBA      EQU   X'11'                   SET BUFFER ADDRESS               07490001
IC       EQU   X'13'                   INSERT CURSOR                    07500001
PT       EQU   X'05'                   PROGRAM TAB                      07510001
RA       EQU   X'3C'                   REPEATE TO ADDRESS               07520001
EUA      EQU   X'12'                   ERASE UNPROTECTED TO ADDRESS     07530001
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 07540001
         END                                                            07550001
//*                                                                     07560001
//LKED.SYSLMOD DD DSN=SYS2.CMDLIB,DISP=SHR                              07570001
//LKED.SYSIN   DD *                                                     07580001
  ALIAS CLS                            12/2014 JLM                      07590001
  NAME CLRSCRN(R)                      12/2014 JLM                      07600001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL11 07610001
//*                                                                     07620001
//ASMFCL12 EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM,NOXREF',          07630001
//             MAC1='SYS1.AMODGEN'                                      07640001
//* ***************************************************************** * 07650001
//* Installs SETPFKEY into SYS2.LINKLIB                               * 07660001
//* ***************************************************************** * 07670001
//ASM.SYSIN DD *                                                        07680001
*                              SOURCE: CBT V249 FILE 295                07690001
*          DATA SET CBT1115    AT LEVEL 005 AS OF 10/23/80              07700001
         PRINT OFF                                                      07710001
         MACRO                                                          07720001
&NAME    INIT  &RG=1,&PC=,&REQ=,&PATCH=,&SAVE=,&PARM=,&PGM=,&ENTRY      07730001
         GBLB  &ACSC1(8)                                                07740001
         GBLA  &ACSC2(4)                                                07750001
         GBLC  &ACSC3(4)                                                07760001
         LCLA  &A1,&A2,&A3,&A4                                          07770001
         SPACE 2                                                        07780001
*********************************************************************** 07790001
*        INIT MACRO - PROGRAM ENTRY AND HOUSEKEEPING                  * 07800001
*********************************************************************** 07810001
         SPACE 2                                                        07820001
         AIF   (T'&REQ EQ 'O').REGS                                     07830001
         AIF   ('&REQ' EQ 'NO').EREGS                                   07840001
         MNOTE 0,'INVALID ''REQ'' PARAM, NO ASSUMED'                    07850001
         AGO   .EREGS                                                   07860001
.REGS    ANOP                                                           07870001
.* ISSUE EQUR MACRO FOR REGISTER EQUATED SYMBOLS                        07880001
         EQUATE                                                         07890001
.EREGS   ANOP                                                           07900001
.* CHECK PC AND SET APPROPRIATE SWITCH                                  07910001
         AIF   (T'&PC EQ 'O').NOPCX                                     07920001
         AIF   ('&PC' NE 'ARM' AND '&PC' NE 'YES').NOPC                 07930001
&ACSC1(1) SETB 1                                                        07940001
         AGO   .NOPCX                                                   07950001
.NOPC    ANOP                                                           07960001
         MNOTE 0,'INVALID PC, IGNORED'                                  07970001
.NOPCX   AIF   (T'&ENTRY EQ 'O').NOENTRY                                07980001
         AIF   ('&ENTRY' EQ 'ENTRY').ENTOK                              07990001
         MNOTE 4,'INVALID POSITIONAL OPERAND, NO ENTRY GENERATED'       08000001
         AGO   .NOENTRY                                                 08010001
.ENTOK   ANOP                                                           08020001
         ENTRY &NAME                                                    08030001
.NOENTRY ANOP                                                           08040001
&NAME    DS    0H                                                       08050001
*---------------------------------------------------------------------* 08060001
*        ISSUE SAVE MACRO                                             * 08070001
*---------------------------------------------------------------------* 08080001
         SAVE  (14,12),,&SYSECT-&SYSDATE-&SYSTIME                       08090001
         SPACE 2                                                        08100001
*---------------------------------------------------------------------* 08110001
*        SAVE PARM, GET SAVE AREA, SET UP BASE REGS                   * 08120001
*---------------------------------------------------------------------* 08130001
         SPACE 2                                                        08140001
         LR    2,1                      SAVE PASSED PARAMS              08150001
.NPARM1  AIF   (T'&SAVE EQ 'O').NOSAVE                                  08160001
         USING &NAME,15                 SET UP BASE                     08170001
&ACSC3(1) SETC '&SAVE'                  SAVE LENGTH                     08180001
         LA    0,&SAVE+72+&ACSC1(1)*4   SET GETMAIN LENGTH              08190001
* ISSUE GETMAIN FOR SAVE AREA AND WORK SPACE                            08200001
         GETMAIN R,LV=(0)                                               08210001
.CHAIN   ST    13,4(1)                  SAVE BACKWARD POINTER           08220001
         ST    1,8(13)                  SAVE FORWARD POINTER            08230001
         LR    13,1                     SET SAVE AREA                   08240001
         DROP  15                                                       08250001
         AGO   .ADDRS                                                   08260001
.NOSAVE  USING &NAME,15                 SET UP BASE                     08270001
         CNOP  0,4                      SET ON BOUNDRY                  08280001
         BAL   1,*+76+&ACSC1(1)*4       SET REG SAVE PLUS WORK AREA     08290001
         USING *,13                                                     08300001
         DS    18F                      SAVE AREA                       08310001
         AIF   (NOT &ACSC1(1)).CHAIN                                    08320001
         DS    F                        SPIE SAVE AREA                  08330001
         AGO   .CHAIN                                                   08340001
.ADDRS   AIF   (T'&SAVE EQ 'O').NSAV1                                   08350001
         AIF   (T'&RG NE 'O').OKBASE                                    08360001
         MNOTE 4,'YOU REQUESTED NO BASE REGISTERS, WILL GIVE YOU ONE'   08370001
&A1      SETA  1                                                        08380001
         AGO   .NOBASE                                                  08390001
.OKBASE  ANOP                                                           08400001
&A1      SETA  &RG                                                      08410001
.NOBASE  ANOP                                                           08420001
&A2      SETA  11                                                       08430001
&A3      SETA  1                                                        08440001
&A4      SETA  0                                                        08450001
         BALR  12,0                     SET BASE REG                    08460001
         USING *,12                                                     08470001
.ADRLP   ANOP                                                           08480001
&A1      SETA  &A1-1                                                    08490001
         AIF   (&A1 EQ 0).EADDR                                         08500001
         LA    &A2,4095(&A2+1)          SET MORE BASES                  08510001
         USING *+4095*&A3-&A4-4*&A3-&ACSC1(1)*4,&A2                     08520001
&A3      SETA  &A3+1                                                    08530001
&A2      SETA  &A2-1                                                    08540001
         AGO   .ADRLP                                                   08550001
.NSAV1   ANOP                                                           08560001
         AIF   (T'&RG EQ 'O').OKBASE1                                   08570001
         AIF   (T'&RG NE 'O').OKBASE1                                   08580001
         MNOTE 4,'YOU REQUESTED NO BASE REGISTERS, WILL GIVE YOU ONE'   08590001
&A1      SETA  1                                                        08600001
         AGO   .NOBASE1                                                 08610001
.OKBASE1 ANOP                                                           08620001
&A1      SETA  &RG                                                      08630001
.NOBASE1 ANOP                                                           08640001
&A2      SETA  12                                                       08650001
&A3      SETA  1                                                        08660001
&A4      SETA  82                                                       08670001
         AGO   .ADRLP                                                   08680001
.EADDR   AIF   (T'&PARM EQ 'O').PATCHS                                  08690001
         SPACE 2                                                        08700001
*---------------------------------------------------------------------* 08710001
*        SAVE PARM ADDRESS OR INFO                                    * 08720001
*---------------------------------------------------------------------* 08730001
         SPACE 2                                                        08740001
         AIF   ('&PARM(2)' NE 'ADDR').NPARM2                            08750001
         ST    2,&PARM(1)               SAVE PARM ADDRESS POINTER       08760001
         AGO   .PATCHS                                                  08770001
.NPARM2  AIF   ('&PARM(2)' NE 'DATA').NPARM3                            08780001
         L     1,0(0,2)                 GET PARM ADDRESS                08790001
         LH    3,0(0,1)                 GET LENGTH                      08800001
         LA    3,1(0,3)                 SET FOR MVC                     08810001
         EX    3,*+8                    DO THE MOVE                     08820001
         B     *+10                     GO AROUND                       08830001
         MVC   &PARM(1).(0),0(1)        EXECUTED MOVE                   08840001
         AGO   .PATCHS                                                  08850001
.NPARM3  MNOTE 4,'INVALID ''PARM'' PARAM, NO INFO SAVED'                08860001
.PATCHS  AIF   ('&PATCH' EQ 'NO').LEAVE                                 08870001
         AIF   ('&PATCH' NE 'YES').LEAVE                                08880001
         SPACE 2                                                        08890001
*---------------------------------------------------------------------* 08900001
*        PATCH AREA                                                   * 08910001
*---------------------------------------------------------------------* 08920001
         B     *+104                    GO AROUND                       08930001
         NOP   *                        SET UP ADDRESS INDICATOR        08940001
         DC    96X'00'                  CLEAR PATCH AREA                08950001
.LEAVE   AIF   ('&PC' NE 'YES').NPCYES                                  08960001
         SPACE 2                                                        08970001
*---------------------------------------------------------------------* 08980001
*        ISSUE SPIEPC MACRO                                           * 08990001
*---------------------------------------------------------------------* 09000001
         SPACE 2                                                        09010001
         SPIEPC                                                         09020001
.NPCYES ANOP                                                            09030001
         SPACE 2                                                        09040001
         AIF   (T'&PGM EQ 'O').OUT                                      09050001
*        ISSUE WTO FOR PROGRAM NAME                                     09060001
         SPACE                                                          09070001
         WTO   '&PGM EXECUTING',ROUTCDE=2                               09080001
         SPACE                                                          09090001
.OUT     ANOP                                                           09100001
         LR 1,2                         RESTORE PARM INFO               09110001
         SPACE 2                                                        09120001
         MEND                                                           09130001
         MACRO                                                          09140001
&NAME    LEAVE &SAVE=,&RC=0,&RECREG=                                    09150001
*        COPY  ACSCGBLS                                                 09160001
         GBLB  &ACSC1(8)                                                09170001
         GBLA  &ACSC2(4)                                                09180001
         GBLC  &ACSC3(4)                                                09190001
         SPACE 2                                                        09200001
*********************************************************************** 09210001
*        LEAVE MACRO                                                  * 09220001
*********************************************************************** 09230001
         SPACE 2                                                        09240001
&NAME    DS    0H                                                       09250001
         AIF   (NOT &ACSC1(2)).NOSP1                                    09260001
*---------------------------------------------------------------------* 09270001
*        RESET SPIE ROUTINE                                           * 09280001
*---------------------------------------------------------------------* 09290001
         SPACE 2                                                        09300001
         L     1,&ACSC3(1)+72(13)       GET SAVED ADDRESS               09310001
* ISSUE SPIE MACRO                                                      09320001
         SPIE  MF=(E,(1))                                               09330001
         SPACE 2                                                        09340001
.NOSP1   ANOP                                                           09350001
*---------------------------------------------------------------------* 09360001
*        RESET SAVE AREA AND EXIT                                     * 09370001
*---------------------------------------------------------------------* 09380001
         SPACE 2                                                        09390001
         AIF   (T'&SAVE EQ 'O').NSAV                                    09400001
         LR    1,13                     SET FOR RELEASE                 09410001
.NSAV    L     13,4(13)                 UNCHAIN SAVE AREA               09420001
         AIF   (T'&SAVE EQ 'O').NSAV1                                   09430001
* ISSUE FREEMAIN MACRO                                                  09440001
         LA    0,&SAVE+72+&ACSC1(1)*4   GET LENGTH OF AREA              09450001
         FREEMAIN R,LV=(0),A=(1)                                        09460001
.NSAV1   AIF   (T'&RECREG EQ 'O').RET                                   09470001
         AIF   ('&RECREG'(1,1) EQ '(').RECOK                            09480001
         MNOTE 4,'INVALID RECREG, NOT REGISTER NOTATION, IGNORED'       09490001
         AGO   .RET                                                     09500001
.RECOK   ST    &RECREG(1),24(13)        SAVE RECREG IN R1 AREA          09510001
.RET     RETURN (14,12),T,RC=&RC                                        09520001
         SPACE 2                                                        09530001
         MEND                                                           09540001
         MACRO                                                          09550001
         EQUATE                                                         09560001
**                           EQUATES FOR SYMBOLIC REG USAGE             09570001
R0       EQU   0                                                        09580001
R1       EQU   1                                                        09590001
R2       EQU   2                                                        09600001
R3       EQU   3                                                        09610001
R4       EQU   4                                                        09620001
R5       EQU   5                                                        09630001
R6       EQU   6                                                        09640001
R7       EQU   7                                                        09650001
R8       EQU   8                                                        09660001
R9       EQU   9                                                        09670001
R10      EQU   10                                                       09680001
R11      EQU   11                                                       09690001
R12      EQU   12                                                       09700001
R13      EQU   13                                                       09710001
R14      EQU   14                                                       09720001
R15      EQU   15                                                       09730001
RA       EQU   10                                                       09740001
RB       EQU   11                                                       09750001
RC       EQU   12                                                       09760001
RD       EQU   13                                                       09770001
RE       EQU   14                                                       09780001
RF       EQU   15                                                       09790001
         MEND                                                           09800001
         PRINT ON                                                       09810001
PFK      TITLE 'PFK CREATION - MODIFICATION PROGRAM'                    09820001
****DATE - 10/24/78**************************************************** 09830001
*  PROGRAMMER NAME -                                                  * 09840001
*                                                                     * 09850001
*         BARRY GOLDBERG                                              * 09860001
*         SYSTEMS PROG.                                               * 09870001
*         TECHNICAL SUPPORT                                           * 09880001
*         213-741-4875                                                * 09890001
*         AUTO CLUB OF SOUTHERN CALIFORNIA                            * 09900001
*         2601 S. FIGUEROA                                            * 09910001
*         LOS ANGELES, CA. 90007                                      * 09920001
*  REQUESTED BY -                                                     * 09930001
*                                                                     * 09940001
*         BARRY GOLDBERG                                              * 09950001
*  EFFECTIVE DATE -                                                   * 09960001
*                                                                     * 09970001
*        SOON                                                         * 09980001
*  PARM USAGE -                                                       * 09990001
*                                                                     * 10000001
*        NA                                                           * 10010001
*  REGISTER USAGE -                                                   * 10020001
*                                                                     * 10030001
*        SEE TEXT                                                     * 10040001
*  SWITCHES -                                                         * 10050001
*                                                                     * 10060001
*        NA                                                           * 10070001
*  INPUT FILES -                                                      * 10080001
*                                                                     * 10090001
*        SYSIN - PFK CONTROL                                          * 10100001
*  OUTPUT FILES -                                                     * 10110001
*                                                                     * 10120001
*        NEW PFK AREA IN STORAGE                                      * 10130001
*  MODULES USED -                                                     * 10140001
*                                                                     * 10150001
*        NA                                                           * 10160001
*  ENVIROMENTS  -                                                     * 10170001
*                                                                     * 10180001
*        MVS                                                          * 10190001
*        VS1                                                          * 10200001
*  DESCRIPTION -                                                      * 10210001
*                                                                     * 10220001
*        THIS ROUTINE WILL MODIFY THE PFK AREAS IN MAIN               * 10230001
*        STORAGE.  IN ORDER TO MAKE THEM PERMANENT,                   * 10240001
*        THE OPERATOR WILL HAVE TO MAKE A                             * 10250001
*        REAL PFK DEFINITION.                                         * 10260001
*        NOTE -- THIS PROGRAM MUST RUN AUTHORIZED OR KEY ZERO.        * 10270001
*                                                                     * 10280001
*   PROGRAM DESCRIPTION -                                             * 10290001
*         SSP79PFK WILL LOAD THE RESIDENT PFK AREA FROM CARD IMAGE    * 10300001
*         INPUT.                                                      * 10310001
*         THE OPERATORS MAY MAKE THE CHANGES PERMANENT AND UPDATE     * 10320001
*         SYS1.DCMLIB BY MEARLY UPDATING ANY PFK.                     * 10330001
*         IT IS SUGGESTED THAT THE SIMPLEST ONE BE UPDATED.  THIS WILL* 10340001
*         CAUSE THE IEEPFKEY MEMBER TO BE REWRITTEN WITH ALL PFK'S.   * 10350001
*                                                                     * 10360001
*INPUT RECORD FORMAT:                                                 * 10370001
*                                                                     * 10380001
*   COL               FUNCTION                                        * 10390001
*                                                                     * 10400001
*  1 - 2             CONSOLE ID FOR PFK                               * 10410001
*  3 - 4             PFK #                                            * 10420001
*    5               PFK CONTROL                                      * 10430001
*                    BLANK  =  NULL PFK ENTRY                         * 10440001
*                       N   =  NON CONVERSATIONAL                     * 10450001
*                       Y   =  CONVERSATIONAL                         * 10460001
*  7 - 71            THE COMMAND AS IT WOULD BE ISSUED AND IF         * 10470001
*                     MULTIPLE, SEPARATED WITH A SEMICOLON. ALSO      * 10480001
*                     UNDERSCORE ALLOWED (TAKES A POSITION).          * 10490001
*   72               CONTINUATION COLUMN IF COMMAND(S) REQUIRE        * 10500001
*                     ADDITIONAL SPACE ON ANOTHER CARD.               * 10510001
*   CARD # 2 -                                                        * 10520001
*   16-55            CONTINUATION OF COMMAND IF PREVIOUS CARD         * 10530001
*                     IS NONBLANK IN COL 72.                          * 10540001
*                                                                     * 10550001
*NOTE :                                                               * 10560001
*         1) MAXIMUM LENGTH OF ALL COMMANDS INCLUDING SPECIAL         * 10570001
*               CHARACTERS AND BLANKS IS 105 CHARACTERS.              * 10580001
*         2) THIS PROGRAM MUST RUN AUTHORIZED OR AS A SYSTEM TASK.    * 10590001
*                                                                     * 10600001
*EXAMPLE:                                                             * 10610001
*                                                                     * 10620001
*  //TSGBLGAC JOB 269,GOLDBERG,CLASS=X                                  10630001
*  //PFK   EXEC  PGM=SSP79PFK                                           10640001
*  //SYSIN DD    *                                                      10650001
*  0101N K E,1                                                          10660001
*  0102Y V 020,CONSOLE,ROUT=(1,2,3,4,5,6,8,9,10,11,12,14,15)            10670001
*  0103Y S SSP98LBL.P3;S SSP97LBL.P3                                    10680001
*  0104Y V 520,CONSOLE,ROUT=(1,2,3,4,5)                                 10690001
*  0105Y S LBLREADC.P3;S LBLREAD.P3,ID=_X                               10700001
*  0106Y V 301,CONSOLE,ROUT=7,AUTH=ALL;V 300,CONSOLE,ROUT=3,AUTH=INFO   10710001
*  0107                                                                 10720001
*  0108Y S MSC1010P.P3,RUN=_X                                           10730001
*  0109Y CENOUT C=A,J=_                                                 10740001
*  0110Y S MSC9010P.P3,RUN=_                                            10750001
*  0111Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N                       10760001
*  0112Y S SSP93CPY.P3,NEWVOL='SER=_CAPS'                               10770001
*  0201N K E,1                                                          10780001
*  0202                                                                 10790001
*  0203                                                                 10800001
*  0204Y V 020,MSTCONS                                                  10810001
*  0205                                                                 10820001
*  0206                                                                 10830001
*  0207Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              10840001
*  0208N D A                                                            10850001
*  0209N D N=SOUT                                                       10860001
*  0210N D R                                                            10870001
*  0211N D N                                                            10880001
*  0212N K                                                              10890001
*  0301N K E,1                                                          10900001
*  0302Y S DUMWTR.P                                                     10910001
*  0303Y S IRPT.P3;S IRPTJ.P3                                           10920001
*  0304Y V 002,ONLINE;V 00E,ONLINE;V 011,ONLINE;V 010,ONLINE            10930001
*  0305Y SF PRINT,002,,_;SF PRINT,00E;SF PUNCH,011                      10940001
*  0306Y SF RDR,010                                                     10950001
*  0307Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              10960001
*  0308N D A                                                            10970001
*  0309N D N                                                            10980001
*  0310Y D Q;D R                                                        10990001
*  0311N D N=SOUT                                                       11000001
*  0312N K                                                              11010001
*  0401N K E,1                                                          11020001
*  0402Y V 017,CONSOLE,ROUT=(1,2,3,4,5,6,8,9,10,11,12,14,15)            11030001
*  0403Y S SSP98LBL.P3;S SSP97LBL.P3                                    11040001
*  0404                                                                 11050001
*  0405Y S LBLREADC.P3;S LBLREAD.P3,ID=_X                               11060001
*  0406Y V 301,CONSOLE,ROUT=7,AUTH=ALL;V 300,CONSOLE,ROUT=3,AUTH=INFO   11070001
*  0407                                                                 11080001
*  0408Y S MSC1010P.P3,RUN=_X                                           11090001
*  0409Y CENOUT C=A,J=_                                                 11100001
*  0410Y S MSC9010P.P3,RUN=_                                            11110001
*  0411Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              11120001
*  0412Y S SSP93CPY.P3,NEWVOL='SER=_CAPS'                               11130001
*  0501N K E,1                                                          11140001
*  0502                                                                 11150001
*  0503                                                                 11160001
*  0504                                                                 11170001
*  0505                                                                 11180001
*  0506                                                                 11190001
*  0507Y K S,DEL=RD,SEG=19,RTME=001,RNUM=19,CON=N;K A,NONE              11200001
*  0508                                                                 11210001
*  0509                                                                 11220001
*  0510                                                                 11230001
*  0511                                                                 11240001
*  0512                                                                 11250001
*                                                                       11260001
****DATE - 10/24/78**************************************************** 11270001
         EJECT                                                          11280001
SETPFKEY START 0                                                  *JLM* 11290001
XX       INIT  PGM=SETPFKEY                                       *JLM* 11300001
         SPACE                                                          11310001
         OPEN  SYSIN                                                    11320001
         SPACE 2                                                        11330001
         MODESET KEY=ZERO                                               11340001
         SPACE                                                          11350001
G1       EQU   *                                                        11360001
         SPACE                                                          11370001
**********************************************************************  11380001
*        THIS IS THE LOOP START                                       * 11390001
*        INITIALIZE THE PFK AREA AND READ/FILL WITH DATA              * 11400001
**********************************************************************  11410001
         SPACE                                                          11420001
         MVI   CTLLINE,C' '             BLANK OUT IMAGE                 11430001
         MVC   CTLLINE+1(L'CTLLINE-1),CTLLINE                           11440001
         XC    CTLIND,CTLIND            AND INDICATORS                  11450001
G1A      EQU   *                                                  *JLM* 11460001
         GET   SYSIN                    GET A RECORD                    11470001
         CLI   0(R1),C'*'               IS THIS A COMMENT         *JLM* 11480001
         BE    G1A                        YES, IGNORE             *JLM* 11490001
         LR    R3,R1                    POINT TO IT                     11500001
         PACK  DBL,0(2,R3)              GET CONSOLE ID                  11510001
         CVB   R1,DBL                                                   11520001
         STH   R1,CONID                 AND SAVE IT                     11530001
         PACK  DBL,2(2,R3)              GET PFK NUMBER                  11540001
         CVB   R1,DBL                                                   11550001
         STH   R1,PFKNO                 SAVE IT FOR LATER               11560001
         CLI   4(R3),C' '               NULL ENTRY?                     11570001
         BE    SETX                     YES, WE ARE FINISHED            11580001
         OI    CTLIND,ACTIVE            INDICATE ACTIVE ENTRY           11590001
         CLI   4(R3),C'N'               IS IT NON CONVERSATIONAL        11600001
         BE    SET1                     YES                             11610001
         CLI   4(R3),C'Y'               COONVERSATIONAL                 11620001
         BNE   SET1                     TOO BAD, IGNORE                 11630001
         OI    CTLIND,CONY              SET CONVERSATIONAL BIT          11640001
         SPACE 2                                                        11650001
SET1     EQU   *                                                        11660001
         MVC   CTLLINE1,6(R3)           SET COMMAND                     11670001
         CLI   71(R3),C' '              ANY CONTINUATION?               11680001
         BE    SETX                     NO                              11690001
G2A      EQU   *                                                  *JLM* 11700001
         GET   SYSIN                                                    11710001
         CLI   0(R1),C'*'               IS THIS A COMMENT         *JLM* 11720001
         BE    G2A                        YES, IGNORE             *JLM* 11730001
         MVC   CTLLINE2,15(R1)          GET REST OF THE COMMAND         11740001
         SPACE 2                                                        11750001
**********************************************************************  11760001
*        DATA AREA NOW COMPLETE, CHECK AND SET PFK                    * 11770001
**********************************************************************  11780001
         SPACE                                                          11790001
SETX     EQU   *                                                        11800001
         L     R15,CVTPTR               POINT TO CVT                    11810001
         USING CVT,R15                                                  11820001
         L     R15,CVTCUCB                                              11830001
         USING UCM,R15                                                  11840001
         LM    R3,R5,UCMVDATA           SET TO SEARCH                   11850001
         DROP  R15                                                      11860001
         LH    R6,CONID                 GET CON NUMBER                  11870001
SRCH1    EQU   *                                                        11880001
         BCT   R6,SRCH2                 NEXT ONE?                       11890001
         B     SRCH3                    CONTINUE TO LOOK                11900001
SRCH2    EQU   *                                                        11910001
         BXLE  R3,R4,SRCH1              GO GET MORE                     11920001
         B     G1                       NOT HERE, IGNORE                11930001
         SPACE 2                                                        11940001
SRCH3    EQU   *                                                        11950001
**********************************************************************  11960001
*        HAVE FOUND THE ENTRY, NOW MUST GO TO                         * 11970001
*        GET THE PFK AREA                                             * 11980001
**********************************************************************  11990001
         SPACE 2                                                        12000001
         USING UCMLIST,R3                                               12010001
         ICM   R15,B'1111',UCMXB        DCM ADDRESS                     12020001
         BZ    G1                       NOT HERE                        12030001
         DROP  R3                                                       12040001
         USING DCM,R15                                                  12050001
         ICM   R15,B'1111',DCMADPFK                                     12060001
         BZ    G1                       NOT HERE, IGNORE                12070001
         SPACE 2                                                        12080001
**********************************************************************  12090001
*        WE ARE NOW POINTING TO THE PFK AREA                          * 12100001
**********************************************************************  12110001
         SPACE                                                          12120001
         LH    R4,PFKNO                 GET PFK NUMBER                  12130001
PFK1     EQU   *                                                        12140001
         BCT   R4,NXTPFK                GET ANOTHER                     12150001
         CLC   0(1,R15),PFKNO+1         IS IT THE SAME?                 12160001
         BNE   G1                       NO, ERROR                       12170001
         MVC   1(PFKLEN-1,R15),CTLIND   SET UP THE PFK                  12180001
         B     G1                                                       12190001
         SPACE                                                          12200001
NXTPFK   EQU   *                                                        12210001
         LA    R15,PFKLEN(R15)          POINT TO NEXT ONE               12220001
         CLI   0(R15),100               END?                            12230001
         BE    G1                       YES, NOT HERE                   12240001
         B     PFK1                     GO LOOK AGAIN                   12250001
         SPACE 2                                                        12260001
EOFIN    EQU   *                                                        12270001
         MODESET KEY=NZERO                                        *JLM* 12280001
         CLOSE SYSIN                                                    12290001
         LEAVE                                                          12300001
         SPACE 2                                                        12310001
         LTORG                                                          12320001
         SPACE                                                          12330001
DBL      DS    D                        A WORK AREA                     12340001
CONID    DS    H                        REQUESTED CONSOLE               12350001
PFKNO    DS    H                        REQUESTED PFK NUMBER            12360001
         DS    0D                                                       12370001
CTLIND   DS    X                        CONTROL BYTE                    12380001
ACTIVE   EQU   X'80'                                                    12390001
CONY     EQU   X'20'                    PFK IS CONVERSATIONAL           12400001
CTLLINE  DS    0CL108                   LENGTH OF DATA                  12410001
CTLLINE1 DS    CL65                     FIRST CARD IMAGE AMOUNT         12420001
CTLLINE2 DS    CL40                     NEXT AMOUNT (ALLOW ONLY 105)    12430001
         DS    CL3                      THE REST                        12440001
         SPACE 2                                                        12450001
PFKLEN   EQU   110                      LENGTH OF A PFK ANTRY           12460001
         SPACE 2                                                        12470001
SYSIN    DCB   DDNAME=SYSIN,MACRF=GL,EODAD=EOFIN,DSORG=PS               12480001
         SPACE 2                                                        12490001
         CVT   SYS=AOS1,DSECT=YES                                       12500001
         IEECUCM SYS=AOS1,FORMAT=NEW                                    12510001
DCM      DSECT                                                          12520001
         IEECRDCM DSECT=YES                                             12530001
         SPACE 2                                                        12540001
         END                                                            12550001
//*                                                                     12560001
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             12570001
//LKED.SYSIN   DD *                                                     12580001
  SETCODE AC(1)                        12/2014 JLM                      12590001
  NAME SETPFKEY(R)                     12/2014 JLM                      12600001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL12 12610001
//*                                                                     12620001
//UPDATE13 EXEC PGM=IEBUPDTE,PARM=NEW                                   12630001
//* ***************************************************************** * 12640001
//* Installs SETPFKEY PROC into SYS2.PROCLIB                          * 12650001
//* ***************************************************************** * 12660001
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=MOD                                12670001
//SYSPRINT DD  SYSOUT=*                                                 12680001
//SYSIN    DD  DATA,DLM='><'                                            12690001
./ ADD    LIST=ALL,NAME=SETPFKEY                                        12700001
//*-------------------------------------------------------------------* 12710001
//*            SET CONSOLE PFKEYS FROM SYS1.PARMLIB MEMBER            * 12720001
//*-------------------------------------------------------------------* 12730001
//SETPFKEY PROC M=                                                      12740001
//SETPFKEY EXEC PGM=SETPFKEY                                            12750001
//SYSIN     DD DSN=SYS1.PARMLIB(SETPFK&M),DISP=SHR                      12760001
./ ENDUP                                                                12770001
><                                                                      12780001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE13 12790001
//*                                                                     12800001
//ASMFCL14 EXEC ASMFCL,PARM.ASM='LIST,NODECK,OBJ,TERM,NOXREF',          12810001
//             MAC1='SYS1.AMODGEN',                                     12820001
//             PARM.LKED='XREF,LET,LIST,NCAL'                           12830001
//* ***************************************************************** * 12840001
//* Installs ZTIMER into SYS2.LINKLIB                                 * 12850001
//* ***************************************************************** * 12860001
//ASM.SYSIN DD *                                                        12870001
         PRINT OFF                                                      12880001
         MACRO                                                          12890001
&LABEL   $PROLOG &LV=,&RENT=NO,&ERRCODE=,&C=,&SP=,&GM=,&LIST=NO         12900001
.********************************************************************** 12910001
.*                                                                    * 12920001
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 12930001
.*                                                                    * 12940001
.*       THIS MACRO PROVIDES STANDARD LINKAGE AND BASE REGISTER       * 12950001
.*       SPECIFICATIONS FOR MOST MEMBERS OF THE CSS TOOL KIT.         * 12960001
.*                                                                    * 12970001
.*       THE FIRST OPERERAND IS A POSITIONAL LIST OF REGISTERS        * 12980001
.*       TO BE USED AS BASE(S) FOR THE CSECT BEING DEFINED AND        * 12990001
.*       THE LABEL BECOMES THE CSECT NAME.                            * 13000001
.*                                                                    * 13010001
.*       LV=NNN    SPECIFIES AN ADDITIONAL AMOUNT OF STORAGE TO BE    * 13020001
.*                 GOTTEN FOLLOWING THE DYNAMIC SAVE AREA.  THIS      * 13030001
.*                 ADDITIONAL STORAGE IS ADDRESSED VIA REG 13         * 13040001
.*                 (JUST FOLLOWING THE 18 FULLWORD SAVEAREA) AND      * 13050001
.*                 IS LIMITED TO 4023.                                * 13060001
.*                                                                    * 13070001
.*       RENT=YES  IF USED, SPECIFIES THAT THE LENGTH OF ADDITIONAL   * 13080001
.*                 STORAGE IS DETERMINED BASED UPON THE SIZE OF       * 13090001
.*                 THE DYNAMIC STORAGE DEFINED BETWEEN TWO            * 13100001
.*                 LABELS:  "SAVEAREA" AND "WORKEND".  THE WORK       * 13110001
.*                 AREA IS ADDRESSABLE VIA REG 13 THE SAME AS THE     * 13120001
.*                 LV= OPERAND.  THE USER DEFINED CONSTANTS THAT      * 13130001
.*                 EXIST IN THE SOURCE ARE COPIED TO THE NEWLY        * 13140001
.*                 AQUIRED STORAGE AND ARE DIRECTLY ADDRESSABLE.      * 13150001
.*                 CAUTION: THE DYNAMIC AREA MUST BE DEFINED          * 13160001
.*                          "IMMEDIATELY PRIOR TO ANY DSECTS" AND     * 13170001
.*                          YOU MUST SPECIFY "LTORG" PRIOR TO THE     * 13180001
.*                          DEFINITION OF "SAVEAREA".                 * 13190001
.*                                                                    * 13200001
.*                 NOTE: LV= AND RENT=YES ARE MUTUALLY EXCLUSIVE.     * 13210001
.*                                                                    * 13220001
.*       ERRCODE=  SPECIFIES THE RETURN CODE TO BE RETURNED TO        * 13230001
.*                 THE CALLER IN THE EVENT THAT THE CONDITIONAL       * 13240001
.*                 GETMAIN FAILS.  IF SPECIFIED, THE GETMAIN THAT     * 13250001
.*                 IS ISSUED WILL BE CONDITIONAL AND IF IT DOES       * 13260001
.*                 NOT COMPLETE NORMALLY, THIS ERROR CODE WILL        * 13270001
.*                 BE RETURNED TO THE CALLER.  IF ERRCODE IS NOT      * 13280001
.*                 SPECIFIED, THE GETMAIN THAT IS ISSUED WILL BE      * 13290001
.*                 UNCONDITIONAL WITH A RELATED 80A ABEND IN THE      * 13300001
.*                 EVENT OF FAILURE.                                  * 13310001
.*                                                                    * 13320001
.*       SP=       IS USED TO CAUSE AN MVS SPLEVEL MACRO TO BE        * 13330001
.*                 EXECUTED AS PART OF THE ASSEMBLY.  POSSIBLE        * 13340001
.*                 OPTIONS ARE "1" (370) OR "2" (XA).  IF NOT         * 13350001
.*                 SPECIFIED, THE SPLEVEL MACRO IS NOT USED.          * 13360001
.*                                                                    * 13370001
.*       C=        IS A MEANS OF PROVIDING ADDITIONAL DATA IN THE     * 13380001
.*                 EYECATCHER.  IF USED, THE DATA SPECIFIED MUST      * 13390001
.*                 BE ENCLOSED WITHIN QUOTES AND IS LIMITED TO        * 13400001
.*                 46 CHARACTERS.                                     * 13410001
.*                                                                    * 13420001
.*       GM=NO     IS NOT SUPPORTED BY THIS MACRO BUT IS ALLOWED      * 13430001
.*                 FOR COMPATIBILITY OF OLDER VERSIONS.               * 13440001
.*                                                                    * 13450001
.*       LIST=NO   SUPPRESSES GENERATION OF LISTINGS FOR $PROLOG,     * 13460001
.*                 $EPILOG AND $REGS WHEN EXPANSION IS ACTIVE         * 13470001
.*                 (PRINT GEN).                                       * 13480001
.*                                                                    * 13490001
.*       EXAMPLES:                                                    * 13500001
.*                                                                    * 13510001
.*       SECTNAME $PROLOG ,        R12 IS BASE BY DEFAULT             * 13520001
.*       SECTNAME $PROLOG R12,R11  R12 IS 1ST BASE AND R11 IS SECOND  * 13530001
.*       SECTNAME $PROLOG R2,LV=8  R2 IS BASE AND AN ADDITIONAL       * 13540001
.*                                 8 BYTES ARE ADDED TO THE STORAGE   * 13550001
.*                                 GOTTEN FOR THE SAVEAREA.           * 13560001
.*                                                                    * 13570001
.*       SECTNAME $PROLOG RENT=YES R12 IS BASE AND THE ADDITIONAL     * 13580001
.*                                 STORAGE TO BE GOTTEN IS DEFINED    * 13590001
.*                                 BETWEEN "SAVEAREA" AND "WORKEND".  * 13600001
.*       SAVEAREA DS    9D         (SAVE AREA FOR $PROLOG GENERATION) * 13610001
.*       MYFIELD1 DC    CL8'DATA1' (PROGRAM CONSTANTS)                * 13620001
.*       MYFIELD2 DC    CL8'DATA2' (PROGRAM CONSTANTS)                * 13630001
.*       WORKEND  EQU   *          (END OF DYNAMIC WORK AREA)         * 13640001
.*                                                                    * 13650001
.********************************************************************** 13660001
         GBLA  &EPILOG             DEFINE GLOBAL FOR $EPILOG            13670001
         GBLB  &REGS,&LSAVE        DEFINE GLOBALS FOR $REGS/$EPILOG     13680001
         LCLA  &AA,&AB,&BUMP,&X    DEFINE LOCAL VARIABLES               13690001
         LCLC  &GMT,&BASE,&LISTOPT DEFINE LOCAL VARIABLES               13700001
&X       SETA  &SYSNDX             SET LABEL CONSTANT                   13710001
&EPILOG  SETA  0                   RESET LV= GLOBAL                     13720001
&BUMP    SETA  4096                SET FOR BASE REG BUMP                13730001
&LSAVE   SETB  0                   RESET RENT GLOBAL FOR $EPILOG        13740001
&GMT     SETC  'RU'                SET UNCONDITIONAL GETMAIN            13750001
         AIF   ('&LIST' EQ 'YES').LIST1                                 13760001
         PUSH PRINT                                                     13770001
         PRINT OFF                                                      13780001
.LIST1   ANOP                                                           13790001
&LABEL   CSECT                                                          13800001
         AIF   (T'&SP EQ 'O').GO1  IF NO SPLEVEL REQUIRED               13810001
         SPLEVEL SET=&SP           ISSUE USER REQUESTED SPLEVEL MACRO   13820001
.GO1     ANOP                                                           13830001
         USING *,R15               TEMPORARY BASE                       13840001
         B     $&X.A               BRANCH AROUND CONSTANTS              13850001
         DC    CL8'&LABEL'         PROVIDE EYECATCHER                   13860001
         AIF   (T'&C EQ 'O').GO2   COMMENTS ADDITION?                   13870001
         DC    CL46&C                                                   13880001
.GO2     ANOP                                                           13890001
         DC    C'&SYSDATE @ &SYSTIME' DATE/TIME STAMP OBJECT            13900001
         AIF   (T'&LV   EQ 'O').GO3 IF LV= NOT SPECIFIED                13910001
         AIF   ('&RENT' NE 'YES').GO3 RENT NOT ALSO SPECIFIED           13920001
         MNOTE 12,'$PROLOG - RENT=YES AND LV=&LV MUTUALLY EXCLUSIVE'    13930001
         MEXIT                                                          13940001
.GO3     AIF   ('&RENT' EQ 'YES').GO4   RENT=YES SPECIFIED              13950001
         AIF   (T'&LV   NE 'O').GO4   LV= SPECIFIED                     13960001
&LSAVE   SETB  1                   SET NORENT GLOBAL FOR $EPILOG        13970001
$AVE&X   DC    18F'0'              DEFINED SAVE AREA                    13980001
.GO4     ANOP                                                           13990001
         AIF   (T'&LABEL NE 'O').GO5 INSURE CSECT NAME PROVIDED         14000001
         MNOTE 8,'$PROLOG - CSECT NAME NOT SUPPLIED'                    14010001
.GO5     ANOP                                                           14020001
$&X.A    STM   R14,R12,12(R13)     SAVE CALLERS REGISTERS               14030001
&BASE    SETC  'R12'               ASSUME A BASE REGISTER               14040001
         AIF   (N'&SYSLIST EQ 0).GO6 USE DEFAULT IF NOT SPECIFIED       14050001
&BASE    SETC  '&SYSLIST(1)'       SET THE SPECIFIED BASE REGISTER      14060001
.GO6     ANOP                                                           14070001
         LR    &BASE,R15           SET FIRST BASE REGISTER              14080001
         DROP  R15                 FREE THE TEMPORARY BASE              14090001
         USING &LABEL,&BASE        INFORM ASSEMBLER                     14100001
         AIF   (N'&SYSLIST EQ 0).GO7                                    14110001
&AA      SETA  2                   NUMBER TO DEFINE +1                  14120001
.LOOP    ANOP                                                           14130001
         AIF   (&AA GT N'&SYSLIST).GO7                                  14140001
&AB      SETA  &AA-1               NUMBER OF LAST BASE REG DEFINED      14150001
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AB)) SET NEXT BASE REG      14160001
         LA    &SYSLIST(&AA),2048(&SYSLIST(&AA)) SET NEXT BASE REG      14170001
         USING &LABEL+&BUMP,&SYSLIST(&AA) INFORM THE ASSEMBLER          14180001
&BUMP    SETA  &BUMP+4096          BUMP INDEX                           14190001
&AA      SETA  &AA+1               BUMP CONTROL COUNT                   14200001
         AGO   .LOOP                                                    14210001
.GO7     AIF   (T'&ERRCODE EQ 'O').GO8 IF ERROR CODE NOT SPECIFIED      14220001
&GMT     SETC  'RC'                ERROR CODE WAS SPECIFIED             14230001
.GO8     AIF   (T'&LV   NE 'O').GO10 LV= SPECIFIED, DO GETMAIN          14240001
         AIF   ('&RENT' EQ 'YES').GO12 RENT SPECIFIED, DO GETMAIN       14250001
         AIF   (T'&ERRCODE EQ 'O').GO9 IF ERROR CODE NOT SPECIFIED      14260001
      MNOTE 8,'$PROLOG - ERRCODE=&ERRCODE INVALID WITHOUT RENT=YES/LV=' 14270001
.GO9     ANOP                                                           14280001
$&X.B    LA    R2,$AVE&X           ADDRESS OF SAVE AREA                 14290001
         AGO   .COMMON                                                  14300001
.GO10    ANOP                                                           14310001
&EPILOG  SETA  &LV+72              SET SIZE FOR $EPILOG FREEMAIN        14320001
         LA    R0,&LV+72           SET SIZE FOR GETMAIN                 14330001
         GETMAIN &GMT,LV=(0)       GET STORAGE                          14340001
         AIF   (T'&ERRCODE EQ 'O').GO11 IF UNCONDITIONAL                14350001
         LTR   R15,R15             STORAGE GOTTEN?                      14360001
         BZ    $&X.C               YES, CONTINUE                        14370001
         LA    R15,&ERRCODE        SET SPECIFIED ERROR CODE             14380001
         ST    R15,16(R13)         INTO SAVE AREA                       14390001
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 14400001
         BR    R14                 AND RETURN                           14410001
$&X.C    DS    0H                                                       14420001
.GO11    ANOP                                                           14430001
         LR    R2,R1               SAVE THE GOTTEN STORAGE ADDRESS      14440001
         LR    R14,R2               MVCL - TARGET ADDR                  14450001
         LA    R15,&LV+72           MVCL - TARGET SIZE                  14460001
         SR    R0,R0                MVCL - SOURCE ADDR (NONE)           14470001
         SR    R1,R1                MVCL - SOURCE SIZE (NONE)           14480001
         MVCL  R14,R0              ZERO GOTTEN STORAGE                  14490001
         AGO   .COMMON                                                  14500001
.GO12    ANOP                                                           14510001
$&X.B    GETMAIN &GMT,LV=WORKEND-SAVEAREA GET THE SAVE AREA STORAGE     14520001
         AIF   (T'&ERRCODE EQ 'O').GO13 IF UNCONDITIONAL                14530001
         LTR   R15,R15             STORAGE GOTTEN?                      14540001
         BZ    $&X.C               YES, CONTINUE                        14550001
         LA    R15,&ERRCODE        SET SPECIFIED ERROR CODE             14560001
         ST    R15,16(R13)         INTO SAVE AREA                       14570001
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 14580001
         BR    R14                 AND RETURN                           14590001
$&X.C    DS    0H                                                       14600001
.GO13    ANOP                                                           14610001
         LR    R2,R1               SAVE THE GOTTEN STORAGE ADDRESS      14620001
         LR    R14,R2               MVCL - TARGET ADDR                  14630001
         LA    R15,WORKEND-SAVEAREA MVCL - TARGET SIZE                  14640001
         LA    R0,SAVEAREA          MVCL - SOURCE ADDR                  14650001
         LR    R1,R15               MVCL - SOURCE SIZE                  14660001
         MVCL  R14,R0              COPY TO WORKING STORAGE              14670001
         USING SAVEAREA,R13        PROVIDE ADDRESSABILITY               14680001
.COMMON  ANOP                                                           14690001
         LR    R14,R13             COPY OLD SAVE AREA ADDRESS           14700001
         LR    R13,R2              SET NEW SAVEAREA ADDRESS             14710001
         ST    R14,4(R13)          CHAIN SAVEAREA - BACKWARD            14720001
         ST    R13,8(R14)          CHAIN SAVEAREA - FORWARD             14730001
         LM    R15,R2,16(R14)      RESTORE ENTRY REGISTERS              14740001
         SR    R14,R14             RESET RETURN ADDRESS                 14750001
         AIF   (&REGS).SKIPREG                                          14760001
&LISTOPT SETC  'LIST=&LIST'                                             14770001
         $REGS &LISTOPT            DEFINE STANDARD REG EQUATES          14780001
.SKIPREG AIF   ('&LIST' EQ 'YES').MEXIT                                 14790001
         POP  PRINT                                                     14800001
.MEXIT   ANOP                                                           14810001
         MEND                                                           14820001
         MACRO                                                          14830001
&LABEL   $EPILOG &RETCODE,&LIST=NO                                      14840001
.********************************************************************** 14850001
.*                                                                    * 14860001
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 14870001
.*                                                                    * 14880001
.*       THIS MACRO PROVIDES STANDARD DE-LINKAGE CONVENTIONS FOR      * 14890001
.*       MOST CSS TOOL KIT MEMBERS.                                   * 14900001
.*                                                                    * 14910001
.********************************************************************** 14920001
         GBLA  &EPILOG                                                  14930001
         GBLB  &LSAVE                                                   14940001
         AIF   ('&LIST' EQ 'YES').LIST1                                 14950001
         PUSH  PRINT                                                    14960001
         PRINT OFF                                                      14970001
.LIST1   ANOP                                                           14980001
&LABEL   DS    0H                                                       14990001
         AIF   (&LSAVE).GO3                                             15000001
         AIF   (&EPILOG EQ 0).GO1                                       15010001
         LA    R0,&EPILOG          GET SAVEAREA LENGTH                  15020001
         AGO   .GO2                                                     15030001
.GO1     LA    R0,WORKEND-SAVEAREA GET SAVEAREA LENGTH                  15040001
.GO2     LR    R1,R13              GET SAVEAREA ADDRESS FOR FREEMAIN    15050001
.GO3     ANOP                                                           15060001
         L     R13,4(R13)          GET BACK CHAIN POINTER               15070001
         ST    R15,16(R13)         SAVE REGISTER 15 (RETCODE)           15080001
         AIF   (&LSAVE).GO4                                             15090001
         FREEMAIN RU,LV=(0),A=(1)  FREE SAVEAREA                        15100001
.GO4     ANOP                                                           15110001
         LM    R14,R12,12(R13)     RESTORE CALLERS REGS                 15120001
         AIF   (T'&RETCODE EQ 'O').GO5                                  15130001
         LA    R15,&RETCODE        SET RETURN CODE                      15140001
.GO5     ANOP                                                           15150001
         BR    R14                 RETURN TO CALLER                     15160001
         AIF   ('&LIST' EQ 'YES').MEXIT                                 15170001
         POP   PRINT                                                    15180001
.MEXIT   ANOP                                                           15190001
         MEND                                                           15200001
         MACRO                                                          15210001
         $REGS &LIST=NO                                                 15220001
.********************************************************************** 15230001
.*                                                                    * 15240001
.*                                  CHANEY SYSTEMS SUPPORT, INC.      * 15250001
.*                                                                    * 15260001
.*       THIS MACRO PROVIDES STANDARD BASE REGISTER EQUATES FOR       * 15270001
.*       ALL "CSS" SUPPORT ROUTINES.                                  * 15280001
.*                                                                    * 15290001
.********************************************************************** 15300001
         GBLB  &REGS                                                    15310001
         AIF   (&REGS).MEXIT                                            15320001
&REGS    SETB  1                                                        15330001
         AIF   ('&LIST' NE 'YES').LIST1                                 15340001
         PUSH  PRINT               SAVE CURRENT PRINT SETTINGS          15350001
         PRINT OFF                 TEMPORARILY TURN OFF PRINT           15360001
.LIST1   ANOP                                                           15370001
R0       EQU   0                                                        15380001
R1       EQU   1                                                        15390001
R2       EQU   2                                                        15400001
R3       EQU   3                                                        15410001
R4       EQU   4                                                        15420001
R5       EQU   5                                                        15430001
R6       EQU   6                                                        15440001
R7       EQU   7                                                        15450001
R8       EQU   8                                                        15460001
R9       EQU   9                                                        15470001
R10      EQU   10                                                       15480001
R11      EQU   11                                                       15490001
R12      EQU   12                                                       15500001
R13      EQU   13                                                       15510001
R14      EQU   14                                                       15520001
R15      EQU   15                                                       15530001
         AIF   ('&LIST' NE 'YES').MEXIT                                 15540001
         POP   PRINT               RESTORE CURRENT PRINT SETTINGS       15550001
.MEXIT   ANOP                                                           15560001
         MEND                                                           15570001
*                              SOURCE: XEPHON MVS UDPATE 08/1987        15575001
         PRINT ON                                                       15580001
         TITLE '*** ZTIMER - TSG ACTION REMINDER.                 ***'  15590001
*---------------------------------------------------------------------* 15600001
*    THIS PROGRAM PROVIDES A 'REMINDER' FACILITY FOR EVENTS WHICH     * 15610001
*    OCCUR REGULARLY. THIS PROGRAM WILL ISSUE A WRITE-TO-OPERATOR     * 15620001
*    OR A CONSOLE COMMAND DEPENDING ON THE INPUT.                     * 15630001
*    INPUT:  VIA DDNAME SYSIN (CONTROL STATEMENTS)                    * 15640001
*            VIA DDNAME SMFIN (SMFPRMOO MEMBER IN SYS1.PARMLIB)       * 15650001
*    OUTPUT: TO CONSOLE VIA WTO OR MGCR MACRO.                        * 15660001
*---------------------------------------------------------------------* 15670001
* 12/2004 RETRIEVAL OF SYSTEM ID VIA CONTROL BLOCKS               *JLM* 15680001
*---------------------------------------------------------------------* 15690001
ZTIMER   $PROLOG R12,C='ZTIMER'                                         15700001
*---------------------------------------------------------------------* 15710001
*       GET CURRENT TIME AND DATE INTO CHARACTER FORMAT.              * 15720001
*       CHECK FOR LEAP YEAR AND ADJUST.                               * 15730001
*       EVALUATE DAY OF THE WEEK AND WHETHER WEEK-END OR NOT.         * 15740001
*---------------------------------------------------------------------* 15750001
         TIME  DEC                        GET SYSTEM TIME AND DATE      15760001
         ST    R0,PTIM                    STORE PACKED TIME             15770001
         ST    R1,PDAT                    STORE PACKED DATE             15780001
         STCM  R1,4,YEAREND+1             SAVE 'YY' PART OF DATE        15790001
         ED    CURTMSK(6),PTIM            UNPACK TIME                   15800001
         ED    CURDMSK(7),PDAT+1          UNPACK DATE                   15810001
         SLR   R2,R2                      R2 = 0                        15820001
         CVB   R3,PDATDBL                 R3 = YYDDD IN BINARY          15830001
         D     R2,FULL1000                R3 = YY,  R2 = DDD            15840001
         LR    R4,R3                      KEEP YEARS FOR LATER          15850001
         SRL   R4,2                       R4 = R4 / 4                   15860001
         SLL   R4,2                       R4 = R4 * 4                   15870001
         CR    R3,R4                      ARE YEARS EQUAL ?             15880001
         BNE   NOLEAP                     NO ->                         15890001
         MVC   YEAREND+2(2),LEAPDAYS      LEAP YEARS HAVE 366 DAYS      15900001
NOLEAP   EQU   *                                                        15910001
         ST    R3,YEARS                   SAVE BINARY YEARS             15920001
         ST    R2,DAYS                    SAVE BINARY DAYS              15930001
         SLR   R2,R2                      R2 = 0                        15940001
         LA    R3,365                     R3 = 365                      15950001
         M     R2,YEARS                   R3 = YEARS * 365              15960001
         L     R4,YEARS                   R4 = YEARS IN BINARY          15970001
         BCTR  R4,0                       R4 = R4 - 1                   15980001
         SRL   R4,2                       R4 = R4 / 4                   15990001
         AR    R3,R4                      R3 = R3 + R4                  16000001
         A     R3,DAYS                    R3 = R3 + DDD DAYS            16010001
         ST    R3,TEMP                    SAVE IT FOR NOW               16020001
         SLR   R2,R2                      R2 = 0                        16030001
         D     R2,FULL7                   R3 = INTEGER(R3 / 7)          16040001
         M     R2,FULL7                   R3 = R3 * 7                   16050001
         L     R2,TEMP                    GET BACK PREVIOUS VALUE       16060001
         SR    R2,R3                      R2 = 0 TO 6 (SUN TO SAT)      16070001
         SLL   R2,2                       R2 = R2 * 4                   16080001
         LA    R2,WKDAYS(R2)              R2 -> TABLE ENTRY             16090001
         MVC   WKDAY,0(R2)                EXTRACT WEEKDAY               16100001
         CLC   WKDAY,SATURDAY             IS IT A SATURDAY ?            16110001
         BE    WEEKEND                    YES ->                        16120001
         CLC   WKDAY,SUNDAY               IS IT A SUNDAY ?              16130001
         BNE   WEEKDAY                    NO ->                         16140001
WEEKEND  EQU   *                                                        16150001
         MVC   DAYTYPE,WKEND              SET DAYTYPE TO 'WEEK-END'     16160001
WEEKDAY  EQU   *                                                        16170001
*---------------------------------------------------------------------* 16180001
*        CALCULATE NEXT INVOCATION OF ZTIMER                          * 16190001
*        (2 HRS HENCE OR AT NEXT EVENT WHICHEVER COMES EARLIER)       * 16200001
*        GET SYSTEM-ID FROM SMF PARAMETERS                            * 16210001
*---------------------------------------------------------------------* 16220001
         OI    PTIM+3,X'0F'               GIVE A DECENT SIGN TO PTIM    16230001
         SRP   PTIM,64-1,0                SHIFT RIGHT 1 DIGIT POS       16240001
         AP    PTIM,HRS2                  TIME = TIME + 2 HOURS         16250001
         ED    NXTTMSK(7),PTIM            TIME FOR NEXT EXECUTION       16260001
         CP    PTIM,DAYEND                DID WE PASS MIDNIGHT ?        16270001
         BNH   SAMEDAY                    NO ->                         16280001
         MVC   NXTTIM+1(5),TWENTY4        NEXT EXECUTION IS AT MIDNIGHT 16290001
SAMEDAY  EQU   *                                                  *JLM* 16300001
*         OPEN  (SMFIN,(INPUT))            OPEN SMF PARMLIB MEMBER*JLM* 16310001
*SMFREAD  EQU   *                                                 *JLM* 16320001
*         GET   SMFIN,INAREA               READ A RECORD          *JLM* 16330001
*         LA    R2,INAREA                  R2 -> START OF SMF RECO*JLM* 16340001
*         LA    R4,1                       R4 =  INCREMENT FOR BXL*JLM* 16350001
*         LA    R5,INAREA+68               R5 -> END OF SMF RECORD*JLM* 16360001
*SIDSERCH EQU   *                                                 *JLM* 16370001
*         CLC   0(4,R2),SIDKEY             'SID(' KEYWORD FOUND ? *JLM* 16380001
*         BE    SIDFOUND                   YES ->                 *JLM* 16390001
*         BXLE  R2,R4,SIDSERCH             INCR, CHECK AND LOOP   *JLM* 16400001
*         B     SMFREAD                    NOT FOUND, TRY NEXT CAR*JLM* 16410001
*SMFEOF   EQU   *                                                 *JLM* 16420001
*         MVC   SYSID,BLANKS               NO SYSID FOUND.        *JLM* 16430001
*         B     SYSIDOK                                           *JLM* 16440001
*SIDFOUND EQU   *                                                 *JLM* 16450001
*         MVC   SYSID,4(R2)                SAVE CURRENT SYSTEM-ID *JLM* 16460001
*SYSIDOK  EQU   *                                                 *JLM* 16470001
*         CLOSE SMFIN                      CLOSE INPUT FILE       *JLM* 16480001
          USING PSA,R2                     PSA BASE               *JLM* 16490001
          USING CVT,R4                     CVT BASE               *JLM* 16500001
          USING SMCABASE,R5                SMCA BASE              *JLM* 16510001
          XR    R2,R2                      POINT TO PSA (0)       *JLM* 16520001
          L     R4,FLCCVT                  POINT TO CVT           *JLM* 16530001
          L     R5,CVTSMCA                 POINT TO SMCA          *JLM* 16540001
          MVC   SYSID,SMCASID              RETRIEVE SMF-ID        *JLM* 16550001
          DROP  R2,R4,R5                   RELEASE BASES          *JLM* 16560001
*---------------------------------------------------------------------* 16570001
*        READ EACH RECORD, AND CHECK FORMAT, DATE AND SYSTEM-ID       * 16580001
*---------------------------------------------------------------------* 16590001
         OPEN  (SYSIN,(INPUT))            OPEN INPUT FILE               16600001
READ     EQU   *                                                        16610001
         GET   SYSIN,INAREA               READ A RECORD                 16620001
         CLI   INAREA,C'*'                IS THIS A COMMENT CARD ?      16630001
         BE    READ                       YES ->                        16640001
         CLC   INAREA(3),ALL              TO BE DONE ON 'ALL' DAYS ?    16650001
         BE    CHKSYSID                   YES ->                        16660001
         CLC   INAREA(5),DAYTYPE          TO BE DONE ON THIS TYP OF DAY 16670001
         BE    CHKSYSID                   YES ->                        16680001
         CLC   INAREA(3),WKDAY            TO BE DONE ON THIS WEEKDAY ?  16690001
         BE    CHKSYSID                   YES ->                        16700001
         LA    R2,WKDAYS                  R2 -> WEEKDAY TABLE           16710001
         LA    R4,4                       R4 =  LENGTH OF TABLE ENTRIES 16720001
         LA    R5,WKDAYS+24               R5 -> END OF WEEKDAY TABLE    16730001
DAYSERCH EQU   *                                                        16740001
         CLC   INAREA(3),0(R2)            TO BE DONE ON ANOTHER DAY ?   16750001
         BE    READ                       YES ->                        16760001
         BXLE  R2,R4,DAYSERCH             INCR, CHECK AND LOOP          16770001
         CLC   INAREA(5),WKEND            TO BE DONE ON A WEEKEND ?     16780001
         BE    READ                       YES ->                        16790001
         CLC   INAREA(5),WORKDAY          TO BE DONE IN THE WEEK ?      16800001
         BE    READ                       YES ->                        16810001
         CLC   INAREA(5),CURDAT           TO BE DONE ON THIS JULDATE ?  16820001
         BE    CHKSYSID                   YES ->                        16830001
         CLI   INAREA+2,C'/'              VALID DATE SEPARATOR ?        16840001
         BNE   DUFFCARD                   NO ->                         16850001
         CLI   INAREA+5,C'/'              VALID DATE SEPARATOR ?        16860001
         BNE   DUFFCARD                   NO ->                         16870001
         BAL   R14,CALTOJUL               DDMMYY TO YYDDD CONVERSION    16880001
         CLC   JULDAT,CURDAT              TO BE DONE TODAY ?            16890001
         BNE   READ                       NO ->                         16900001
CHKSYSID EQU   *                                                        16910001
         CLC   INSYSID,BLANKS             WAS A SYSTEM-ID SPECIFIED ?   16920001
         BE    CHKTIME                    NO -> GO AND CHECK TIME       16930001
         CLC   INSYSID,SYSID              ACTION FOR THIS SYSTEM ?      16940001
         BNE   READ                       NO ->                         16950001
*---------------------------------------------------------------------* 16960001
*        CHECK TIME OR EXECUTION AND TYPE OF ACTION REQUIRED.         * 16970001
*        PERFORM THE REQUIRED FUNCTION.                               * 16980001
*---------------------------------------------------------------------* 16990001
CHKTIME  EQU   *                                                        17000001
         CLC   INTIME,BLANKS              WAS A TIME SPECIFIED ?        17010001
         BE    EXECUTE                    NO -> GO AND DO IT            17020001
         CLC   INTIME,CURTIM              WHEN IS THE EVENT FOR ?       17030001
         BE    EXECUTE                    NOW -> GO AND DO IT           17040001
         BL    READ                       BEFORE -> IGNORE IT           17050001
         CLC   INTIME,NXTTIM+1            BEFORE THE NEXT EXECUTION ?   17060001
         BH    READ                       NO -> IGNORE IT               17070001
         MVC   NXTTIM+1(5),INTIME         ENSURE WE CATCH HIM           17080001
         B     READ                       GET NEXT CARD                 17090001
EXECUTE  EQU   *                                                        17100001
         CLC   INTYPE,COMMAND             IS IT A COMMAND REQUEST ?     17110001
         BE    ISSUECOM                   YES ->                        17120001
* ADDED 2ND WTO AND COMPARE FOR 'DSH'/'DSN'                       *JLM* 17130001
         CLC   INTYPE,DISPLAYH            IS IT A DISPLAY REQUEST ?     17140001
         BE    ISSUEDSH                   YES ->                        17150001
         CLC   INTYPE,DISPLAYN            IS IT A DISPLAY REQUEST ?     17160001
         BNE   DUFFCARD                   NO ->                         17170001
ISSUEDSN MVC   MESSAGEN+8(49),INACTN      MOVE MESSAGE TO WTO AREA      17180001
MESSAGEN WTO   '....+....1....+....2....+....3....+....4....+....'      17190001
         B     READ                       GO BACK FOR MORE              17200001
ISSUEDSH MVC   MESSAGEH+8(49),INACTN      MOVE MESSAGE TO WTO AREA      17210001
MESSAGEH WTO   '....+....1....+....2....+....3....+....4....+....',    C17220001
               DESC=(2)                   TELL 'EM                      17230001
         B     READ                       GO BACK FOR MORE              17240001
ISSUECOM EQU   *                                                        17250001
         MVC   COMACTN(49),INACTN         MOVE COMMAND TO COM AREA      17260001
         MODESET KEY=ZERO,MODE=SUP        SWITCH TO SUPERVISOR STATE    17270001
         SLR   R0,R0                      PRETEND YOU'RE MVS            17280001
         MGCR  COMAREA                    ISSUE THE COMMAND             17290001
         MODESET KEY=NZERO,MODE=PROB      SWITCH BACK TO NORMAL         17300001
         B     READ                       GO BACK FOR MORE              17310001
*---------------------------------------------------------------------* 17320001
*        END-OF-FILE HANDLING.                                        * 17330001
*        RETURN TO CALLER.                                            * 17340001
*        DUFF INPUT HANDLING.                                         * 17350001
*---------------------------------------------------------------------* 17360001
EOF      EQU   *                                                        17370001
         CLOSE SYSIN                      CLOSE INPUT FILE              17380001
         MVC   AUTOTIME,NXTTIM+1          MOVE NEXT TIME TO AUTO-COMM   17390001
         MODESET KEY=ZERO,MODE=SUP        SWITCH TO SUPERVISOR STATE    17400001
         SLR   R0,R0                      PRETEND YOU'RE MVS            17410001
         MGCR  SCHEDULE                   ISSUE THE AUTOMATIC COMMAND   17420001
         MODESET KEY=NZERO,MODE=PROB      SWITCH BACK TO NORMAL         17430001
         $EPILOG                                                        17440001
DUFFCARD EQU   *                                                        17450001
         WTO   'ZTIMER: INVALID INPUT DETECTED - DUFF CARD READS:'      17460001
         MVC   DUFFMSG+8(54),INAREA                                     17470001
DUFFMSG  WTO   '....+....1....+....2....+....3....+....4....+....5....' 17480001
         B     READ                       IGNORE THIS ONE               17490001
*---------------------------------------------------------------------* 17500001
*        CALENDER DATE (DD/MM/YY) TO JULIAN DATE (YY.DDD) CONVERSION  * 17510001
*        INPUT AREA IS FIELD CALLED 'INDATE'                          * 17520001
*        OUTPUT AREA IS FIELD CALLED 'JULDAT'                         * 17530001
*---------------------------------------------------------------------* 17540001
CALTOJUL EQU   *                                                        17550001
*        PACK  DAY,INDATE(2)              EXTRACT DAY PORTION     *JLM* 17560001
*        PACK  MONTH,INDATE+3(2)          EXTRACT MONTH PORTION   *JLM* 17570001
         PACK  MONTH,INDATE(2)            EXTRACT MONTH PORTION   *JLM* 17580001
         PACK  DAY,INDATE+3(2)            EXTRACT DAY PORTION     *JLM* 17590001
         PACK  YEAR,INDATE+6(2)           EXTRACT YEAR PORTION          17600001
         CVB   R5,MONTHDBL                R5 = MONTH IN BINARY          17610001
         BCTR  R5,0                       R5 = R5 - 1                   17620001
         SLL   R5,1                       R5 = R5 * 2                   17630001
         LH    R2,TABLE(R5)               EXTRACT CUMULATIVE DAYS       17640001
         CVB   R3,YEARDBL                 R3 = YEAR IN BINARY           17650001
         LR    R5,R3                      R5 = YEAR IN BINARY           17660001
         SRL   R3,2                       R3 = R3 / 4                   17670001
         SLL   R3,2                       R3 = R3 * 4                   17680001
         CR    R3,R5                      WAS IT A LEAP YEAR ?          17690001
         BNE   MNTHREDY                   NO ->                         17700001
         CP    MONTH,PACK2                IS IT PAST FEBRUARY ?         17710001
         BNH   MNTHREDY                   NO ->                         17720001
         LA    R2,1(R2)                   ADD 1 FOR THE 29TH            17730001
MNTHREDY EQU   *                                                        17740001
         CVB   R3,DAYDBL                  R3 = DAY IN BINARY            17750001
         AR    R2,R3                      R2 = TOTAL DAYS               17760001
         M     R4,FULL1000                R5 = YEAR * 1000              17770001
         AR    R5,R2                      R5 = R5 + DAYS                17780001
         CVD   R5,DOUBLE                  CONVERT DATE TO PACKED        17790001
         MVC   JULDATF(7),JULDMSK         MOVE EDIT MASK TO FIELD       17800001
         ED    JULDATF(7),DOUBLE+5        FORMAT INTO CHAR FIELD        17810001
         BR    R14                        RETURN TO CALLER              17820001
*---------------------------------------------------------------------* 17830001
*        CONSTANT DATA AREAS.                                         * 17840001
*---------------------------------------------------------------------* 17850001
FULL7    DC    F'7'                       CONSTANT                      17860001
FULL1000 DC    F'1000'                    CONSTANT                      17870001
PACK2    DC    PL2'2'                     CONSTANT                      17880001
HRS2     DC    PL4'020000'                02 HOURS                      17890001
HRS24    DC    PL4'240000'                24 HOURS                      17900001
DAYEND   DC    PL4'235959'                END OF DAY (MIDNIGHT - 1 MIN) 17910001
YEAREND  DC    PL4'365'                   END OF YEAR                   17920001
LEAPDAYS DC    PL2'366'                   NO OF DAYS IN A LEAP YEAR     17930001
CURDMSK  DC    CL1'0'                     FILL CHAR                     17940001
CURDAT   DC    XL6'21204B202020'          CURRENT DATE   (YY.DDD)       17950001
CURTMSK  DC    CL1'0'                     FILL CHAR                     17960001
CURTIM   DC    XL5'21204B2020'            CURRENT TIME  (HH.MM)         17970001
NXTTMSK  DC    CL1'0'                     FILL CHAR                     17980001
NXTTIM   DC    XL6'2120204B2020'          TIME OF NEXT EXECUTION        17990001
JULDMSK  DC    XL7'F021204B202020'        JULIAN DATE EDIT MASK         18000001
JULDATF  DS    XL1                        FILL CHAR                     18010001
JULDAT   DS    XL6                        JULIAN DATE                   18020001
TABLE    DC    H'000',H'031',H'059',H'090',H'120',H'151'                18030001
         DC    H'181',H'212',H'243',H'273',H'304',H'334'                18040001
WKDAYS   DC    C'SUN MON TUE WED THU FRI SAT'  ALL THE WEEKDAYS         18050001
ALL      DC    CL3'ALL'                   CONSTANT                      18060001
TWENTY4  DC    CL5'24.00'                 CONSTANT                      18070001
BLANKS   DC    CL5'     '                 CONSTANT                      18080001
DISPLAYH DC    CL3'DSH'                   CONSTANT                      18090001
DISPLAYN DC    CL3'DSN'                   CONSTANT                      18100001
COMMAND  DC    CL3'COM'                   CONSTANT                      18110001
SATURDAY DC    CL3'SAT'                   CONSTANT                      18120001
SUNDAY   DC    CL3'SUN'                   CONSTANT                      18130001
SIDKEY   DC    CL4'SID('                  CONSTANT                      18140001
WKEND    DC    CL5'WKEND'                 CONSTANT                      18150001
WORKDAY  DC    CL5'WKDAY'                 CONSTANT                      18160001
DAYTYPE  DC    CL5'WKDAY'                 EITHER 'WKDAY' OR 'WKEND'     18170001
COMAREA  DS    0F                         COMMAND AREA                  18180001
         DC    H'70'                      LENGTH                        18190001
         DC    X'0000'                    FLAGS                         18200001
COMACTN  DC    CL80' '                    COMMAND FIELD                 18210001
SCHEDULE DS    0F                         AUTOMATIC COMMAND AREA        18220001
         DC    H'37'                      LENGTH                        18230001
         DC    X'0000'                    FLAGS                         18240001
         DC    CL08'$TA99,T='                                           18250001
AUTOTIME DC    CL05'00.00'                TIME FOR NEXT EXECUTION       18260001
         DC    CL80',''$VS,''''S ZTIMER'''''''                          18270001
*---------------------------------------------------------------------* 18280001
*        OTHER STORAGE REQUIREMENTS                                   * 18290001
*---------------------------------------------------------------------* 18300001
DOUBLE   DS    D                          WORK SPACE FOR CONVERSIONS    18310001
PTIMDBL  DC    XL4'0'                                                   18320001
PTIM     DC    PL4'0'                     PACKED TIME FORMAT HHMMSSCC   18330001
PDATDBL  DC    XL4'0'                                                   18340001
PDAT     DC    PL4'0'                     PACKED DATE FORMAT 00YYDDDF   18350001
DAYDBL   DC    XL5'0'                                                   18360001
DAY      DC    PL3'0'                     WORKSPACE - JULTOCAL          18370001
MONTHDBL DC    XL5'0'                                                   18380001
MONTH    DC    PL3'0'                     WORKSPACE - JULTOCAL          18390001
YEARDBL  DC    XL5'0'                                                   18400001
YEAR     DC    PL3'0'                     WORKSPACE - JULTOCAL          18410001
TEMP     DS    F                          WORKSPACE                     18420001
YEARS    DS    F                          YEARS IN BINARY               18430001
DAYS     DS    F                          DAYS IN BINARY                18440001
WKDAY    DS    CL3                        TODAY'S WEEK DAY              18450001
SYSID    DS    CL4                        SMF SYSID OF CURRENT SYSTEM   18460001
INAREA   DS    0CL80                      INPUT AREA                    18470001
INDATE   DS    CL08                       DATE FORMAT DD/MM/YY          18480001
         DS    CL01                                                     18490001
INTIME   DS    CL05                       TIME FORMAT HH.MM OR BLANK    18500001
         DS    CL01                                                     18510001
INTYPE   DS    CL03                       ACTION TYPE DISPLAY/COMMAND   18520001
INSYSID  DS    CL04                       SMF SYSTEM-ID OF TARGET SYS   18530001
         DS    CL01                                                     18540001
INACTN   DS    CL49                       COMMAND OR MESSAGE            18550001
         DS    CL08                                                     18560001
*---------------------------------------------------------------------* 18570001
*        DATA CONTROL BLOCKS.                                         * 18580001
*---------------------------------------------------------------------* 18590001
SYSIN    DCB   DDNAME=SYSIN,DSORG=PS,MACRF=(GM),EODAD=EOF               18600001
*SMFIN    DCB   DDNAME=SMFIN,DSORG=PS,MACRF=(GM),EODAD=SMFEOF     *JLM* 18610001
         PUSH PRINT                                                     18620001
         PRINT OFF                                                      18630001
         IHAPSA DSECT=YES              PREFIXED SAVE AREA         *JLM* 18640001
         CVT    DSECT=YES              COMM VECT TABLE            *JLM* 18650001
         IEESMCA                       SMF CONTROL TABLE          *JLM* 18660001
         POP PRINT                                                      18670001
*---------------------------------------------------------------------* 18680001
*           STEFAN NEUMANN                                            * 18690001
*           SYSTEMS PROGRAMMER                                        * 18700001
*           SUN ALLIANCE PLC (UK)      XEPHON 1987                    * 18710001
*---------------------------------------------------------------------* 18720001
         END                                                            18730001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 18740001
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             18750001
//LKED.SYSIN   DD *                                                     18760001
  SETCODE AC(1)                        12/2014 JLM                      18770001
  NAME ZTIMER(R)                       12/2014 JLM                      18780001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL14 18790001
//*                                                                     18800001
//UPDATE15 EXEC PGM=IEBUPDTE,PARM=NEW                                   18810001
//* ***************************************************************** * 18820001
//* Installs ZTIMER PROC itno SYS2.PROCLIB                            * 18830001
//* ***************************************************************** * 18840001
//SYSPRINT DD  SYSOUT=*                                                 18850001
//SYSUT2   DD  DISP=MOD,DSN=SYS2.PROCLIB                                18860001
//SYSIN    DD  DATA,DLM='><'                                            18870001
./ ADD NAME=ZTIMER                                                      18880001
./ NUMBER NEW1=10,INCR=10                                               18890001
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 18900001
//* INVOKE AUTOMATIC COMMAND/MESSAGE UTILITY                          * 18910001
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 18920001
//ZTIMER  PROC MEM=ZTIMER,SOUT=A                                        18930001
//REMINDR EXEC PGM=ZTIMER                                               18940001
//SYSUDUMP DD  SYSOUT=&SOUT                                             18950001
//SYSIN    DD  DISP=SHR,DSN=SYS2.CONTROL(&MEM)                          18960001
./ ENDUP                                                                18970001
><                                                                      18980001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE15 18990001
//*                                                                     19000001
//UPDATE16 EXEC PGM=IEBUPDTE,PARM=NEW                                   19010001
//* ***************************************************************** * 19020001
//* ALLOCATE USER CONTROL CARD LIBRARY (SYS2.CONTROL),                * 19030001
//* Installs ZTIMER commands into SYS2.CONTROL                        * 19040001
//* ***************************************************************** * 19050001
//SYSPRINT DD  SYSOUT=*                                                 19060001
//SYSUT2   DD  DSN=SYS2.CONTROL,                                        19070001
//             UNIT=3350,VOL=SER=MVS000,DISP=(,CATLG),                  19080001
//             SPACE=(TRK,(30,5,20)),                                   19090001
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     19100001
//SYSIN    DD  *                                                        19110001
./ ADD NAME=ZTIMER                                                      19120001
./ NUMBER NEW1=10,INCR=10                                               19130001
*----------------------------------------------------------------------*19140001
*             ZTIMER - AUTOMATIC REMINDER/COMMAND FACILITY             *19150001
* COL 1 TO 8:  EITHER DATE IN FORMAT 'DD/MM/YY'                        *19160001
*                 OR DATE IN FORMAT 'YY/DDD'                           *19170001
*                 OR 3-CHAR WEEKDAY 'MON TUE WED THU FRI SAT SUN'      *19180001
*                 OR WORD 'WKDAY' FOR ANY WEEKDAY (MON TO FRI)         *19190001
*                 OR WORD 'WKEND' FOR WEEKEND (SAT AND SUN)            *19200001
*                 OR WORD 'ALL' FOR EVERY DAY EXECUTION                *19210001
*                 OR '*' IN COL 1 FOR A COMMENT CARD.                  *19220001
* COL 10 TO 14:  EITHER TIME IN FORMAT 'HH.MM'                         *19230001
*                 OR BLANKS FOR EXECUTION EVERY TIME.                  *19240001
* COL 16 TO 18:  EITHER 'COM' TO ISSUE COMMAND                         *19250001
*                 OR 'DSH' TO ISSUE A BRIGHT (NON-ROLLABLE) MESSAGE    *19260001
*                 OR 'DSN' TO ISSUE A NORMAL (ROLLABLE) MESSAGE        *19270001
* COL 19 TO 22:  EITHER THE SYSTEM-ID WHERE THE ACTION IS TO BE DONE   *19280001
*                 OR BLANKS FOR ACTION ON ALL SYSTEMS                  *19290001
* COL 24 TO 72:  EITHER THE MESSAGE TO BE DISPLAYED                    *19300001
*                 OR THE COMMAND AS ENTERED AT THE MASTER CONSOLE.     *19310001
*----------------------------------------------------------------------*19320001
*...+....1....+....2....+....3....+....4....+....5....+....6....+....7..19330001
*--------PURGE SYSOUT 'Z' & 'Y' OLDER THAN N DAYS, ALL SYSTEMS          19340001
ALL      00.05 DSN     $OQ,Q=Z,CANCEL,D=5                               19350001
ALL      00.05 COM     $OQ,Q=Z,CANCEL,D=5                               19360001
ALL      00.05 DSN     $OQ,Q=Y,CANCEL,D=5                               19370001
ALL      00.05 COM     $OQ,Q=Y,CANCEL,D=5                               19380001
*                                                                       19390001
./ ENDUP                                                                19400001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE16 19410001
//*                                                                     19420001
//ASMFCL17 EXEC ASMFCL,PARM.ASM='LIST,LOAD,NODECK,NOXREF',              19430001
//             MAC1='SYS1.AMODGEN',                                     19440001
//             PARM.LKED='XREF,LET,LIST,NCAL'                           19450001
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 19460001
//* Installs CLIP into SYS2.LINKLIB                                   * 19470001
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 19480001
//ASM.SYSIN DD *                                                        19490001
         PRINT OFF                                                      19500001
         MACRO                                                          19510001
         YREGS                                                          19520001
         GBLA  &REGS                                                    19530001
&REGS    SETA  1                                                        19540001
         SPACE 1                                                        19550001
R0       EQU   0                                                        19560001
R1       EQU   1                                                        19570001
R2       EQU   2                                                        19580001
R3       EQU   3                                                        19590001
R4       EQU   4                                                        19600001
R5       EQU   5                                                        19610001
R6       EQU   6                                                        19620001
R7       EQU   7                                                        19630001
R8       EQU   8                                                        19640001
R9       EQU   9                                                        19650001
R10      EQU   10                                                       19660001
R11      EQU   11                                                       19670001
R12      EQU   12                                                       19680001
R13      EQU   13                                                       19690001
R14      EQU   14                                                       19700001
R15      EQU   15                                                       19710001
         SPACE 1                                                        19720001
         MEND                                                           19730001
         PRINT ON,NOGEN                                                 19740001
*                              SOURCE: XEPHON MVS UDPATE 12/1987        19750001
         TITLE 'CLIP - CONSOLE CLIP PROGRAM'                            19760001
*=====================================================================* 19770001
*  CLIP - CONSOLE CLIP PROGRAM                                        * 19780001
*     EXEC PGM=CLIP,PARM='UUU/VVVVVV'                                 * 19790001
*        UUU IS UNIT ADDRESS (WHICH MUST BE OFFLINE)                  * 19800001
*     VVVVVV IS NEW VOLUME SERIAL NUMBER                              * 19810001
*     RETURN CODE 28 MEANS: THE PARM WAS IN ERROR                     * 19820001
*                           THE SYSIN FILE DID NOT OPEN               * 19830001
*                           THE SYSPRINT FILE DID NOT OPEN            * 19840001
*     ANY OTHER RETURN CODE IS FROM ICKDSF                            * 19850001
*  THIS PROGRAM MUST BE LINK EDITED WITH AC=1 AND PLACED              * 19860001
*  IN AN APF AUTHORIZED LIBRARY.                                      * 19870001
*=====================================================================* 19880001
         PRINT OFF                                                      19890001
         DCBD  DSORG=PS,DEVD=DA                                         19900001
         PRINT ON,NOGEN                                                 19910001
*   ENTRY LINKAGE                                                       19920001
CLIP     CSECT                                                          19930001
         USING CLIP,R15            SET UP TEMPORARY BASE                19940001
         B     AROUNDID            BRANCH AROUND EYECATCHER             19950001
         DC    AL1(ENDID-*-1)      LENGTH OF EYECATCHER                 19960001
         DC    C'CLIP &SYSDATE. &SYSTIME. '                             19970001
ENDID    EQU   *                                                        19980001
AROUNDID DS    0H                                                       19990001
         STM   R14,R12,12(R13)     SAVE REGISTERS                       20000001
         LR    R12,R15             SET UP NEW BASE                      20010001
         USING CLIP,R12            ...                                  20020001
         DROP  R15                 ...                                  20030001
         LA    R15,SAVEAREA        LINK SAVE AREAS                      20040001
         ST    R15,8(R13)          HSA -> LSA                           20050001
         ST    R13,4(R15)          LSA -> HSA                           20060001
         LR    R13,R15             POINT TO OUR SAVE AREA               20070001
         LR    R2,R1               SAVE PARMLIST POINTER                20080001
*   PARSE THE JCL PARAMETER:  UUU/VVVVVV                                20090001
         L     R4,0(R2)            R4 -> JCL PARM LENGTH                20100001
         LH    R5,0(R4)            R5 = LENGTH OF JCL PARM              20110001
         LA    R4,2(R4)            R4 -> JCL PARM DATA                  20120001
*                                                                       20130001
*   GET THE UNIT ADDRESS                                                20140001
         LR    R1,R4               SAVE START OF PARM                   20150001
UNITLOOP LTR   R5,R5               ARE WE OUT OF PARM?                  20160001
         BNP   UNITLEND            YES...QUIT THE LOOP                  20170001
         CLI   0(R4),C'/'          IS THIS A SLASH?                     20180001
         BE    UNITLEND            YES...QUIT THE LOOP                  20190001
         LA    R4,1(R4)            NO...POINT TO THE NEXT CHARACTER     20200001
         BCTR  R5,0                DOCK THE LENGTH                      20210001
         B     UNITLOOP            AND TRY AGAIN                        20220001
UNITLEND DS    0H                  -                                    20230001
         LR    R15,R4              GET THE LENGTH                       20240001
         SR    R15,R1              OF THE FIRST FIELD                   20250001
         C     R15,=F'3'           IS IT RIGHT?                         20260001
         BNE   BADPARM             NO...BAG IT                          20270001
         MVC   UNIT,0(R1)          SAVE THE UNIT                        20280001
         LA    R4,1(R4)            POINT PAST THE SLASH                 20290001
         BCTR  R5,0                AND TRIM THE LENGTH                  20300001
*   GET THE VOLUME SERIAL NUMBER                                        20310001
         LR    R1,R4               SAVE THE START OF THE FIELD          20320001
VOLLOOP  LTR   R5,R5               ARE WE OUT OF PARM?                  20330001
         BNP   VOLLEND             YES...QUIT THE LOOP                  20340001
         LA    R4,1(R4)            NO...POINT TO THE NEXT CHARACTER     20350001
         BCTR  R5,0                DOCK THE LENGTH                      20360001
         B     VOLLOOP             AND TRY AGAIN                        20370001
VOLLEND  DS    0H                  -                                    20380001
         LR    R15,R4              GET THE LENGTH                       20390001
         SR    R15,R1              OF THE FIRST FIELD                   20400001
         BNP   BADPARM             ZERO LENGTH IS BAD                   20410001
         C     R15,=F'6'           IS IT TOO BIG?                       20420001
         BH    BADPARM             YES...BAG IT                         20430001
         BCTR  R15,0               DECREMENT LENGTH FOR EXECUTE         20440001
         MVI   VOLUME,C' '         BLANK OUT THE VOLSER SLOT            20450001
         MVC   VOLUME+1(L'VOLUME-1),VOLUME  ...                         20460001
MVCINS1  MVC   VOLUME(*-*),0(R1)   *** EXECUTED INSTRUCTION ***         20470001
         EX    R15,MVCINS1         MOVE THE VOLUME SERIAL NUMBER        20480001
         B     CREATEDS            GO CREATE THE DATASET                20490001
*   FLAG  BAD JCL PARAMETER                                             20500001
BADPARM  WTO   'CLIP: THE FORMAT OF THE JCL PARAMETER IS INCORRECT',   X20510001
               ROUTCDE=2                                                20520001
         LA    R15,28              SET BAD RETURN CODE                  20530001
         B     EXIT                GO BAIL OUT                          20540001
CREATEDS DS    0H                  -                                    20550001
         OPEN  (SYSIN,(OUTPUT))    OPEN THE DATASET                     20560001
         TM    SYSIN+(DCBOFLGS-IHADCB),DCBOFOPN  DID IT OPEN?           20570001
         BZ    NOSYSIN             NO...ERROR                           20580001
*   PUT THE ICKDSF CONTROL CARD                                         20590001
         PUT   SYSIN,DSFCARD       PUT THE CARD                         20600001
*   CLOSE THE FILE                                                      20610001
         CLOSE (SYSIN,)            CLOSE IT                             20620001
         B     CALLPROG            GO CALL ICKDSF                       20630001
*   FLAG OPEN ERROR ON SYSIN FILE                                       20640001
NOSYSIN  WTO   'CLIP: THE SYSIN FILE DID NOT OPEN',                    X20650001
               ROUTCDE=2                                                20660001
         LA    R15,28              SET BAD RETURN CODE                  20670001
         B     EXIT                GO BAIL OUT                          20680001
CALLPROG DS    0H                  -                                    20690001
         LA    R1,DUMMYPRM         POINT TO DUMMY PARM                  20700001
         LINK  EP=ICKDSF           CALL THE PROGRAM                     20710001
         LR    R3,R15              SAVE THE RETURN CODE                 20720001
         CVD   R3,DWORK            CONVERT THE RETURN CODE TO DECIMAL   20730001
         MVC   RETCODE,=XL4'40202120'  SET UP RETURN CODE EDIT MASK     20740001
         ED    RETCODE,DWORK+6     EDIT IN THE RETURN CODE              20750001
RCMSG    WTO   'CLIP: ICKDSF RETURN CODE ISXXXX',                      X20760001
               ROUTCDE=2                                                20770001
RETCODE  EQU   RCMSG+8+27,4,C'C'   MESSAGE INSERT FOR RETURN CODE       20780001
SCANSYSP DS    0H                  -                                    20790001
*                                                                       20800001
         OPEN  (SYSPRINT,(INPUT))     OPEN THE DATASET                  20810001
         TM    SYSPRINT+(DCBOFLGS-IHADCB),DCBOFOPN  DID IT OPEN?        20820001
         BZ    NOSYSPR             NO...ERROR                           20830001
*   READ ALL THE SYSPRINT RECORDS                                       20840001
         GET   SYSPRINT            GET THE FIRST RECORD                 20850001
READLOOP DS    0H                  -                                    20860001
         LH    R15,0(R1)           R15 = LENGTH OF RECORD               20870001
         C     R15,=F'9'           IS IT LONG ENOUGH TO BE A MESSAGE?   20880001
         BL    SKIPREC             NO...SKIP IT                         20890001
         LA    R1,5(R1)            POINT PAST THE RDW AND CARRIAGE CTL  20900001
         S     R15,=F'5'           AND ADJUST LENGTH                    20910001
         CLC   =CL3'ICK',0(R1)     IS THIS A POTENTIAL ICK MESSAGE?     20920001
         BNE   SKIPREC             NO...SKIP IT                         20930001
         CLI   3(R1),C'0'          IS IT ICKN, WHERE N IS NUMERIC?      20940001
         BL    SKIPREC             NO...SKIP IT                         20950001
         MVI   DSFMSG,C' '         CLEAR OUT THE MESSAGE SLOT           20960001
         MVC   DSFMSG+1(L'DSFMSG-1),DSFMSG  ...                         20970001
         C     R15,=A(L'DSFMSG)    IS THE LENGTH TOO BIG?               20980001
         BNH   OKLEN               NO...LEAVE IT                        20990001
         L     R15,=A(L'DSFMSG)    YES...MAKE IT RIGHT                  21000001
OKLEN    DS    0H                  -                                    21010001
         BCTR  R15,0               DECREMENT FOR EXECUTE                21020001
MVCINS3  MVC   DSFMSG(*-*),0(R1)   *** EXECUTED INSTRUCTION ***         21030001
         EX    R15,MVCINS3         MOVE THE MESSAGE TO THE WTO          21040001
DSFWTO   WTO   'CLIP: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX21050001
               XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX',ROUTCDE=2               21060001
DSFMSG   EQU   DSFWTO+8+6,80,C'C'  MESSAGE INSERT FOR ICKDSF MESSAGE    21070001
SKIPREC  DS    0H                  -                                    21080001
         GET   SYSPRINT            GET THE NEXT                         21090001
         B     READLOOP            AND TRY AGAIN                        21100001
SYSEOF   DS    0H                  -                                    21110001
*   CLOSE THE FILE                                                      21120001
         CLOSE (SYSPRINT,)         CLOSE IT                             21130001
         LR    R15,R3              PUT BACK THE RETURN CODE             21140001
         B     EXIT                GO BAIL OUT                          21150001
*   FLAG OPEN ERROR ON SYSPRINT FILE                                    21160001
NOSYSPR  WTO   'CLIP: THE SYSPRINT FILE DID NOT OPEN',                 X21170001
               ROUTCDE=2                                                21180001
         LA    R15,28              SET BAD RETURN CODE                  21190001
         B     EXIT                GO BAIL OUT                          21200001
EXIT     DS    0H                  -                                    21210001
         L     R13,4(R13)          POINT TO CALLER'S SAVE AREA          21220001
         L     R14,12(R13)         RESTORE REGISTER 14                  21230001
         LM    R0,R12,20(R13)      RESTORE REGISTERS 0 - 12             21240001
         MVI   12(R13),X'FF'       FLAG SAVE AREA                       21250001
         BR    R14                 RETURN TO O.S.                       21260001
DWORK    DS    D                   DOUBLEWORD WORK AREA                 21270001
SAVEAREA DS    18F                 SAVEAREA                             21280001
DSFCARD  DC    CL80' '             ICKDSF CONTROL CARD                  21290001
         ORG   DSFCARD             ORG BACK TO REDEFINE FIELDS          21300001
         DC    C'  REFORMAT UNITADDRESS('                               21310001
UNIT     DC    C'UUU'              UNIT ADDRESS                         21320001
         DC    C') VOLID('                                              21330001
VOLUME   DC    C'VVVVVV'           VOLUME SERIAL NUMBER                 21340001
         DC    C') NOVERIFY'                                            21350001
         ORG   ,                   RESET LOCATION COUNTER               21360001
DUMMYPRM DS    H'0'                DUMMY PARM FOR ICKDSF                21370001
SYSIN    DCB   DDNAME=SYSIN,                                           X21380001
               MACRF=(PM),                                             X21390001
               DSORG=PS,                                               X21400001
               RECFM=F,                                                X21410001
               LRECL=80,                                               X21420001
               BLKSIZE=80                                               21430001
SYSPRINT DCB   DDNAME=SYSPRINT,                                        X21440001
               MACRF=(GL),                                             X21450001
               DSORG=PS,                                               X21460001
               RECFM=VBA,                                              X21470001
               EODAD=SYSEOF                                             21480001
         LTORG ,                                                        21490001
         YREGS                                                          21500001
         END   CLIP                                                     21510001
//*                                                                     21520001
//LKED.SYSLMOD DD DSN=SYS2.LINKLIB,DISP=SHR                             21530001
//LKED.SYSIN   DD *                                                     21540001
  SETCODE AC(1)                        12/2014 JLM                      21550001
  NAME CLIP(R)                         12/2014 JLM                      21560001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -ASMFCL17 21570001
//*                                                                     21580001
//UPDATE18 EXEC PGM=IEBUPDTE,PARM=NEW                                   21590001
//* ***************************************************************** * 21600001
//* Installs CLIP procedure into SYS2.PROCLIB                         * 21610001
//* ***************************************************************** * 21620001
//SYSPRINT DD  SYSOUT=*                                                 21630001
//SYSUT2   DD  DISP=MOD,DSN=SYS2.PROCLIB                                21640001
//SYSIN    DD  DATA,DLM='><'                                            21650001
./ ADD NAME=CLIP                                                        21660001
./ NUMBER NEW1=10,INCR=10                                               21670001
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 21680001
//* INVOKE CLIP UTILITY WHICH USES ICKDSF TO RE-LABEL OFFLINE DASD    * 21690001
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 21700001
//CLIP     PROC U=,V=        U IS THE UNIT, V IS THE NEW VOLSER         21710001
//CLIP     EXEC PGM=CLIP,PARM='&U./&V.'                                 21720001
//SYSIN    DD   UNIT=SYSDA,SPACE=(TRK,(1,1))                            21730001
//SYSPRINT DD   UNIT=SYSDA,SPACE=(TRK,(1,1))                            21740001
./ ENDUP                                                                21750001
><                                                                      21760001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE18 21770001
//*        END OF PRIMARY SYSGEN03                                      21780001
