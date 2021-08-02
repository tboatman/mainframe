//SYSGEN04 JOB 'SETUP VTAM/TSO',
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)
/*JOBPARM LINES=100
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR
//*
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*        !!                                !!
//*        !! DO NOT RENUMBER THIS JOBSTREAM !!
//*        !!                                !!
//*        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//*
//* ***************************************************************** *
//* VTAM/TSO CONFIGURATION USED IS ALMOST ENTIRELY WITHOUT CHANGES AS *
//* IT WAS INCLUDED BY JIM MORRISON WITH HIS 3375/3380/3390 USERMODS. *
//* ***************************************************************** *
//*
//*--------------------------------------------------------------------
//ASMLKED  PROC U=3350,                  UNIT FOR WORK DATSETS
//             M=MISSING                MEMBER NAME TO BE ASSEMBLED
//ASM      EXEC PGM=ASMBLR,REGION=1024K,
//             PARM='TERM,LIST,NOXREF,NODECK,OBJ'
//STEPLIB  DD  DSN=SYS1.LINKLIB,UNIT=3350,VOL=SER=MVSRES,DISP=SHR
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=&U,SPACE=(1700,(600,100))
//SYSUT2   DD  UNIT=&U,SPACE=(1700,(600,100))
//SYSUT3   DD  UNIT=&U,SPACE=(1700,(600,100))
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSGO    DD  DISP=(,PASS),UNIT=&U,SPACE=(80,(200,50))
//SYSPUNCH DD  DUMMY,DCB=BLKSIZE=80
//SYSIN    DD  DISP=SHR,DSN=SYS1.VTAMSRC(&M)
//LKED     EXEC PGM=IEWL,REGION=512K,
//             PARM='XREF,MAP,LET,LIST,NCAL',
//             COND=(8,LT,ASM)
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=&U,SPACE=(1024,(50,20))
//SYSLIN   DD  DSN=*.ASM.SYSGO,DISP=(OLD,DELETE)
//         DD  DDNAME=SYSIN
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.VTAMLIB(&M)
//         PEND
//*
//*--------------------------------------------------------------------
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW
//* ***************************************************************** *
//* SYS1.PARMLIB: CREATE IKJTSO00 (TSO PARAMETERS)                    *
//* ***************************************************************** *
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.PARMLIB,DISP=MOD
//SYSIN    DD  *
./ ADD NAME=IKJTSO00,LIST=ALL
./ NUMBER NEW1=10,INCR=10
USERMAX=8,                       /* MAXIMUM USERS                    */
RECONLIM=120,                    /* MAXIMUM DISCONNECT MINUTES       */
BUFRSIZE=132,                    /* VTIOC BUFFER SIZE                */
HIBFREXT=13200,                  /* MAX BUFFERS BEFORE SWAP OUT      */
LOBFREXT=6600,                   /* MIMIMUM BUFFERS BEFORE SWAP IN   */
MODE=NOBREAK,                    /* KEYBOARD LOCK OPTION             */
MODESW=NO,                       /* MODESWITCH FROM TERMINAL OPTION  */
CHNLEN=4,                        /* NO. OF RU'S PER CHAIN            */
SCRSIZE=1920                     /* MAXIMUM SCREEN SIZE              */
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE01
//*
//UPDATE02 EXEC PGM=IEBUPDTE,PARM=NEW
//* ***************************************************************** *
//* SYS1.PROCLIB: CREATE IKJACCNT (TSO USER PROCEDURE)                *
//*                      NET (VTAM PROCEDURE)                         *
//*                      TSO (TSO PROCEDURE)                          *
//* ***************************************************************** *
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=IKJACCNT,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//IKJACCNT PROC
//IKJACCNT EXEC PGM=IKJEFT01,DYNAMNBR=20,
//             PARM='EX ''SYS1.CMDPROC(TSOLOGON)'''
//SYSHELP  DD  DISP=SHR,DSN=SYS1.HELP
//         DD  DISP=SHR,DSN=SYS2.HELP
//SYSPROC  DD  DISP=SHR,DSN=SYS1.CMDPROC
//DD1      DD  DYNAM
//DD2      DD  DYNAM
//DD3      DD  DYNAM
//DD4      DD  DYNAM
//DD5      DD  DYNAM
//DD6      DD  DYNAM
//DD7      DD  DYNAM
//DD8      DD  DYNAM
//DD9      DD  DYNAM
//DDA      DD  DYNAM
//DDB      DD  DYNAM
//DDC      DD  DYNAM
//DDD      DD  DYNAM
//DDE      DD  DYNAM
//DDF      DD  DYNAM
./ ADD NAME=NET,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//NET      PROC
//IEFPROC  EXEC PGM=ISTINM01,TIME=1440,REGION=4096K,DPRTY=(14,15)
//VTAMLST  DD  DSN=SYS1.VTAMLST,DISP=SHR
//VTAMLIB  DD  DSN=SYS1.VTAMLIB,DISP=SHR
//VTAMOBJ  DD  DSN=SYS1.VTAMOBJ,
//             UNIT=VIO,SPACE=(CYL,(5,5,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
./ ADD NAME=TSO,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//TSO      PROC MEM=00
//STEP1    EXEC PGM=IKTCAS00,TIME=1440
//PARMLIB  DD  DISP=SHR,DSN=SYS1.PARMLIB(IKJTSO&MEM),FREE=CLOSE
//PRINTOUT DD  SYSOUT=*,DCB=(LRECL=133,RECFM=FB)
./ ENDUP
><
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE02
//*
//UPDATE03 EXEC PGM=IEBUPDTE,PARM=NEW
//* ***************************************************************** *
//* SYS1.VTAMLST: ALLOCATE/CATALOG ON MVS000
//*               CREATE ATCSTR00                                     *
//*               CREATE ATCCON00                                     *
//*               CREATE APPLTSO                                      *
//*               CREATE LCL400 (DEFINE LOCAL 3277 TERMINALS)         *
//* ***************************************************************** *
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.VTAMLST,DISP=(,CATLG),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(CYL,(5,5,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//SYSIN    DD  *
./ ADD NAME=ATCSTR00,LIST=ALL
./ NUMBER NEW1=10,INCR=10
CONFIG=00,                         /*CONFIG LIST SUFFIX              */+
SSCPID=01,                         /*THIS VTAMS ID IN NETWORK        */+
NETSOL=YES,                        /*NETWORK SOLICITOR OPTION        */+
MAXSUBA=31,                        /*MAXIMUM SUBAREAS IN NETWORK     */+
NOPROMPT,                          /*OPERATOR PROMPT OPTION          */+
SUPP=NOSUP,                        /*MESSAGE SUPPRESSION OPTION      */+
COLD,                              /*RESTART OPTION   - COLD/WARM    */+
APBUF=(128,,064),                  /*ACE STORAGE POOL                */+
CRPLBUF=(256,,44),                 /*RPL COPY POOL                   */+
IOBUF=(20,3992,10,F),              /*FIXED IO (GP-5/2009)            */+
LFBUF=(016,,16,F),                 /*LARGE FIXED BUFFER POOL         */+
LPBUF=(032,,32,F),                 /*LARGE PAGEBLE BUFFER POOL       */+
NPBUF=(032,,08,F),                 /*NON WS FMCB                     */+
PPBUF=(20,3992,10,F),              /*PAGEBLE IO (GP-5/2009)          */+
SFBUF=(032,,32,F),                 /*SMALL FIXED BUFFER POOL         */+
SPBUF=(032,,32,F),                 /*SMALL PGBL BUFFER POOL          */+
UECBUF=(32,,16,F),                 /*USER EXIT CB                    */+
WPBUF=(64,,64,F)                   /*MESSAGE CONTROL BUFFER POOL     */
./ ADD NAME=ATCCON00,LIST=ALL
./ NUMBER NEW1=10,INCR=10
APPLTSO,                                             TSO APPLS         X
LCL400                                               LOCAL 3270S
./ ADD NAME=APPLTSO,LIST=ALL
./ NUMBER NEW1=10,INCR=10
TSO      APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0001  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0002  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0003  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0004  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0005  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0006  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0007  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
TSO0008  APPL AUTH=(PASS,NVPACE,TSO),BUFFACT=5
./ ADD NAME=LCL400,LIST=ALL
./ NUMBER NEW1=10,INCR=10
LCL400   LBUILD SUBAREA=2
CUU400   LOCAL TERM=3277,CUADDR=400,ISTATUS=ACTIVE,                    +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU401   LOCAL TERM=3277,CUADDR=401,ISTATUS=ACTIVE,                    +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU402   LOCAL TERM=3277,CUADDR=402,ISTATUS=ACTIVE,                    +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU403   LOCAL TERM=3277,CUADDR=403,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU404   LOCAL TERM=3277,CUADDR=404,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU405   LOCAL TERM=3277,CUADDR=405,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU406   LOCAL TERM=3277,CUADDR=406,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
CUU407   LOCAL TERM=3277,CUADDR=407,ISTATUS=INACTIVE,                  +
               LOGTAB=LOGTAB01,LOGAPPL=NETSOL,                         +
               FEATUR2=(MODEL2,PFK)
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE03
//*
//UPDATE04 EXEC PGM=IEBUPDTE,PARM=NEW
//* ***************************************************************** *
//* SYS1.VTAMSRC: ALLOCATE/CATALOG ON MVS000
//*               CREATE LOGTAB01 (VTAM LOGON INTERPRET TABLE)        *
//*               CREATE LOGMOD01 (VTAM LOGMODE TABLE)                *
//* ***************************************************************** *
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.VTAMSRC,DISP=(,CATLG),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(CYL,(1,1,10)),
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)
//SYSIN    DD  *
./ ADD NAME=LOGTAB01,LIST=ALL
./ NUMBER NEW1=10,INCR=10
***********************************************************************
*  VTAM LOGON INTERPRET TABLE                                         *
***********************************************************************
LOGTAB01 INTAB
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='LOGON'
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='logon'
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='TSO'
         LOGCHAR APPLID=(APPLICID,TSO),SEQNCE='tso'
         ENDINTAB
         END
./ ADD NAME=LOGMOD01,LIST=ALL
./ NUMBER NEW1=10,INCR=10
***********************************************************************
*  VTAM LOGMODE TABLE                                                 *
***********************************************************************
LOGMOD01 MODETAB
*****************************************************************
* NON-SNA 3270 LOCAL TERMINALS                                  *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
S3270    MODEENT LOGMODE=S3270,                                        X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               PSERVIC=X'000000000000000000000200'
*****************************************************************
* NON-SNA 3270 LOCAL TERMINALS                                  *
*      PRIMARY SCREEN   : MODEL 5                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
S32785   MODEENT LOGMODE=S32785,                                       X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               PSERVIC=X'00000000000018501B847F00'
*****************************************************************
* 3274 MODEL 1C WITH MODEL 2 SCREEN (REMOTE SNA)                *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D4C32782 MODEENT LOGMODE=D4C32782,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87F8',                                        X
               PSERVIC=X'020000000000185020507F00'
*****************************************************************
*      3276 SNA WITH MODEL 2 SCREEN (REMOTE SNA)                *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D6327802 MODEENT LOGMODE=D6327802,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'88F8',                                        X
               PSERVIC=X'020000000000185000007E00'
*****************************************************************
*      3274 1C SNA WITH MODEL 5 SCREEN (REMOTE SNA)             *
*      PRIMARY SCREEN   : MODEL 5                               *
*      SECONDARY SCREEN : NONE                                  *
*****************************************************************
D4C32785 MODEENT LOGMODE=D4C32785,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87F8',                                        X
               PSERVIC=X'0200000000001B8400007E00'
*****************************************************************
*      3276 SNA WITH MODEL 2 SCREEN (REMOTE SNA) (T.S.O)        *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D63278TS MODEENT LOGMODE=D63278TS,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'8587',                                        X
               PSERVIC=X'020000000000000000000200'
*****************************************************************
*      3276 SNA WITH 3289 MODEL 2 PRINTER                       *
*****************************************************************
D6328902 MODEENT LOGMODE=D6328902,                                     X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'8787',                                        X
               PSERVIC=X'030000000000185018507F00'
*****************************************************************
*      3274 NON-SNA  MODEL 2 SCREEN (LOCAL)                     *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*****************************************************************
D4B32782 MODEENT LOGMODE=D4B32782,                                     X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               RUSIZES=X'0000',                                        X
               PSERVIC=X'000000000000185000007E00'
*****************************************************************
*     S C S   P R I N T E R                                     *
*****************************************************************
SCS      MODEENT LOGMODE=SCS,                                          X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87C6',                                        X
               PSNDPAC=X'01',                                          X
               SRCVPAC=X'01',                                          X
               PSERVIC=X'01000000E100000000000000'
*****************************************************************
*        N C C F                                                *
*****************************************************************
DSILGMOD MODEENT LOGMODE=DSILGMOD,                                     X
               FMPROF=X'02',                                           X
               TSPROF=X'02',                                           X
               PRIPROT=X'71',                                          X
               SECPROT=X'40',                                          X
               COMPROT=X'2000',                                        X
               RUSIZES=X'0000',                                        X
               PSERVIC=X'000000000000000000000200'
*****************************************************************
*        N C C F                                                *
*****************************************************************
DSIXDMN  MODEENT LOGMODE=DSIXDMN,                                      X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'20',                                          X
               SECPROT=X'20',                                          X
               COMPROT=X'4000',                                        X
               RUSIZES=X'0000',                                        X
               PSERVIC=X'000000000000000000000000'
*****************************************************************
*      3276 SNA WITH MODEL 2 SCREEN (MAGNETIC STRIPE READER)    *
*      PRIMARY SCREEN   : MODEL 2                               *
*      SECONDARY SCREEN : NON                                   *
*      TEST TEST TEST TEST TEST TEST                            *
*****************************************************************
SCSLRDR  MODEENT LOGMODE=SCSLRDR,                                      X
               FMPROF=X'03',                                           X
               TSPROF=X'03',                                           X
               PRIPROT=X'B1',                                          X
               SECPROT=X'90',                                          X
               COMPROT=X'3080',                                        X
               RUSIZES=X'87C6',                                        X
               PSNDPAC=X'01',                                          X
               SRCVPAC=X'01',                                          X
               PSERVIC=X'04000000E100000000000000'
         MODEEND
         END
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE04
//*
//UPDATE05 EXEC PGM=IEBUPDTE,PARM=NEW
//* ***************************************************************** *
//* SYS1.CMDPROC: ALLOCATE/CATALOG ON MVS000
//*               CREATE STDLOGON (TSO LOGON GREETING)                *
//*               CREATE USRLOGON (TSO LOGON PROCEDURE)               *
//* ***************************************************************** *
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS1.CMDPROC,DISP=(,CATLG),
//             UNIT=3350,VOL=SER=MVS000,SPACE=(CYL,(20,,100)),
//             DCB=SYS1.MACLIB
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=STDLOGON
        PROC 0
CONTROL NOMSG,NOLIST,NOSYMLIST,NOCONLIST,NOFLUSH
CLS
WRITE ******************************************-
*************************************
WRITE *                                         -
                                    *
WRITE *                                         -
                                    *
WRITE *                    Welcome to the TSO sy-
stem                                *
WRITE *                    =====================-
============                        *
WRITE *                                         -
                                    *
WRITE *                                         -
                                    *
WRITE ******************************************-
*************************************
./ ADD NAME=TSOLOGON
        PROC 0
CONTROL NOMSG,NOLIST,NOSYMLIST,NOCONLIST,NOFLUSH
FREE FILE(SYSHELP)
WRITE Logging on to TSO at &SYSTIME using &SYSPROC
ALLOC FILE(SYSHELP) DSN('SYS1.HELP','SYS2.HELP') SHR
ALLOC FILE(X1) DSN('&SYSUID..CLIST(STDLOGON)') SHR
IF &LASTCC = 0 THEN +
   DO
      WRITE Logging on using private logon procedure
      FREE FILE(SYSPROC)
      FREE FILE(X1)
      ALLOC FILE(SYSPROC) DSN('&SYSUID..CLIST','SYS1.CMDPROC') SHR
   END
ELSE +
   DO
      WRITE Logging on using public logon procedure
      FREE FILE(X1)
   END
/* ENDIF */
%STDLOGON
EXIT
./ ENDUP
><
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE05
//*
//UPDATE06 EXEC PGM=IEBUPDTE,PARM=NEW
//* ***************************************************************** *
//* SYS2.PROCLIB: CREATE TSONUSER (USED TO ADD NEW TSO USERS)         *
//* ***************************************************************** *
//SYSPRINT DD  SYSOUT=*
//SYSUT2   DD  DSN=SYS2.PROCLIB,DISP=MOD
//SYSIN    DD  DATA,DLM='><'
./ ADD NAME=TSONUSER,LIST=ALL
./ NUMBER NEW1=10,INCR=10
//TSONUSER PROC ID=,              TSO USER ID                          +
//             PW='*',            TSO USER PASSWORD                    +
//             AN='*',            ACCOUNT NUMBER                       +
//             PR='IKJACCNT',     LOGON PROCEDURE                      +
//             MS='NOLIM',        REGION LIMIT: MAXSIZE(N)/NOLIM       +
//             OP='NOOPER',       OPERATOR CMNDS: OPER/NOOPER          +
//             AC='NOACCT',       ACCOUNT CMD: ACCT/NOACCT             +
//             JC='JCL',          JOB CONTROL: JCL/NOJCL               +
//             MT='NOMOUNT',      TAPE MOUNT: MOUNT/NOMOUNT            +
//             SZ='SIZE(4096)',   DEFAULT REGION SIZE                  +
//             UN='UNIT(3380)'    DYNAMIC ALLOCATION UNIT
//*
//DEL01    EXEC PGM=IEFBR14
//CLIST    DD  DSN=&ID..CLIST,DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,0),DCB=SYS1.CMDPROC
//CNTL     DD  DSN=&ID..CNTL,DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,0),DCB=SYS1.MACLIB
//SOURCE   DD  DSN=&ID..SOURCE,DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,0),DCB=SYS1.MACLIB
//LOAD     DD  DSN=&ID..LOAD,DISP=(MOD,DELETE,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,0),DCB=SYS1.LINKLIB
//*
//AL02     EXEC PGM=IEFBR14
//CLIST    DD  DSN=&ID..CLIST,DISP=(,KEEP,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.CMDPROC
//CNTL     DD  DSN=&ID..CNTL,DISP=(,KEEP,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.MACLIB
//SOURCE   DD  DSN=&ID..SOURCE,DISP=(,KEEP,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.MACLIB
//LOAD     DD  DSN=&ID..LOAD,DISP=(,KEEP,DELETE),
//             UNIT=SYSDA,VOL=SER=PUB000,
//             SPACE=(CYL,(1,1,20)),DCB=SYS1.LINKLIB
//*
//PW03     EXEC PGM=PARMSWTR,
//             PARM='@ PROFILE NOPREFIX@ ACCOUNT @   SYNC'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(,PASS),
//             UNIT=SYSDA,SPACE=(TRK,(15))
//PW04     EXEC PGM=PARMSWTR,
//             PARM='   DEL (&ID.)'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW05     EXEC PGM=PARMSWTR,
//             PARM='   ADD (&ID. &PW. &AN. &PR.) +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW06     EXEC PGM=PARMSWTR,
//             PARM='       &MS. +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW07     EXEC PGM=PARMSWTR,
//             PARM='       &OP. +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW08     EXEC PGM=PARMSWTR,
//             PARM='       &AC. +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW09     EXEC PGM=PARMSWTR,
//             PARM='       &JC. +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW10     EXEC PGM=PARMSWTR,
//             PARM='       &MT. +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW11     EXEC PGM=PARMSWTR,
//             PARM='       &SZ. +'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW12     EXEC PGM=PARMSWTR,
//             PARM='       &UN.'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//PW13     EXEC PGM=PARMSWTR,
//             PARM='@   LIST (&ID.)@   SYNC@   END'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&ACCOUNT,DISP=(MOD,PASS)
//*
//TSO14    EXEC PGM=IKJEFT01,REGION=4096K
//SYSUADS   DD DSN=SYS1.UADS,DISP=SHR
//SYSLBC    DD DSN=SYS1.BRODCAST,DISP=SHR
//SYSTSPRT  DD SYSOUT=*
//SYSTSIN   DD DSN=&&ACCOUNT,DISP=(OLD,DELETE)
//*
//PW15     EXEC PGM=PARMSWTR,
//             PARM=' DEL &ID..* NOSCRATCH'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(,PASS),
//             UNIT=SYSDA,SPACE=(TRK,(15))
//PW16     EXEC PGM=PARMSWTR,
//             PARM=' DELETE &ID. ALIAS'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW17     EXEC PGM=PARMSWTR,
//             PARM='@ SET LASTCC = 0@ SET MAXCC = 0'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW18     EXEC PGM=PARMSWTR,
//             PARM=' DEFINE ALIAS(NAME(&ID.) RELATE(UCPUB000))'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW19     EXEC PGM=PARMSWTR,
//    PARM=' DEF NONVSAM ( NAME(&ID..CLIST) DEVT(3380) VOL(PUB000) )'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW20     EXEC PGM=PARMSWTR,
//    PARM=' DEF NONVSAM ( NAME(&ID..CNTL) DEVT(3380) VOL(PUB000) )'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW21     EXEC PGM=PARMSWTR,
//    PARM=' DEF NONVSAM ( NAME(&ID..SOURCE) DEVT(3380) VOL(PUB000) )'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW22     EXEC PGM=PARMSWTR,
//    PARM=' DEF NONVSAM ( NAME(&ID..LOAD) DEVT(3380) VOL(PUB000) )'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//PW23     EXEC PGM=PARMSWTR,
//    PARM=' REPRO INFILE(SYSPROC) OUTDATASET(&ID..CLIST(STDLOGON))'
//STEPLIB  DD  DSN=SYSC.LINKLIB,DISP=SHR
//PARMSWTR DD  DSN=&&IDCAMS,DISP=(MOD,PASS)
//*
//IDC24    EXEC PGM=IDCAMS,REGION=1024K
//SYSPRINT DD  SYSOUT=*
//SYSPROC  DD  DSN=SYS1.CMDPROC(STDLOGON),DISP=SHR
//SYSIN    DD  DSN=&&IDCAMS,DISP=(OLD,DELETE)
./ ENDUP
><
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE06
//*
//LOGTAB07 EXEC PROC=ASMLKED,M=LOGTAB01
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -LOGTAB07
//*
//LOGMODE8 EXEC PROC=ASMLKED,M=LOGMOD01
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -LOGTAB08
//*
//* ----------------------------------------------------------------- *
//* REPLACE NETWORK SOLICITOR SCREEN, Step 1: restore default source  *
//* ----------------------------------------------------------------- *
//UPDATE09 EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD  DUMMY
//SYSUT2   DD  DISP=SHR,DSN=SYS1.MACLIB
//SYSIN    DD  *
./ ADD NAME=NETSOL
//         DD  DSN=SYS1.AMACLIB(NETSOL),DISP=SHR
//         DD  *
./ ENDUP
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE09
//*
//* ----------------------------------------------------------------- *
//* Step 2, update with changes to screen appearance                  *
//* ----------------------------------------------------------------- *
//UPDATE10 EXEC PGM=IEBUPDTE
//SYSPRINT DD  DUMMY
//SYSUT1   DD  DISP=SHR,DSN=SYS1.MACLIB
//SYSUT2   DD  DISP=SHR,DSN=SYS1.MACLIB
//SYSIN    DD  *
./ CHANGE NAME=NETSOL
         CLI   MSGINDEX,X'0C'                                           23164802
         BNE   EGSKIP                                                   23164804
         MVC   EGDATA(8),NAMEDEV                                        23164806
         MVC   EGDATA+9(8),APPLID                                       23164807
         LA    R3,EGMSGLN                                               23164808
         L     R4,=A(EGMSG)                                             23164810
*                                                                       23164812
         WRITE RPL=(PTRRPL),                                           X23164814
               OPTCD=(LBT,ERASE),                                      X23164816
               AREA=(R4),                                              X23164818
               RECLEN=(R3),                                            X23164820
               EXIT=WRITEND                                             23164822
*                                                                       23164824
         B     EGOK                                                     23164826
*                                                                       23164828
*                                                                       23164830
EGSKIP   DS    0H                                 EGSKIP                23164832
EGOK     DS    0H                                 EGOK                  23166010
EGMSG    DS    0C                                 EGMSG                 66810010
         DC    X'C3'                                                    66810020
*                                                                       66810030
 DC X'11',X'C5C4',X'1D',X'E8',X'2842F5'                                 66810040
 DC C'HH   HH  EEEEEEE  RRRRRR    CCCCC   UU   UU  LL       EEEEEEE  '  66810050
 DC C' SSSSS'                                                           66810060
*                                                                       66810070
 DC X'11',X'C6D4',X'1D',X'E8'                                           66810080
 DC C'HH   HH  EEEEEEE  RRRRRRR  CCCCCCC  UU   UU  LL       EEEEEEE  '  66810090
 DC C'SSSSSSS'                                                          66810100
*                                                                       66810110
 DC X'11',X'C7E4',X'1D',X'E8'                                           66810120
 DC C'HH   HH  EE       RR   RR  CC   CC  UU   UU  LL       EE       '  66810130
 DC C'SS   SS'                                                          66810140
*                                                                       66810150
 DC X'11',X'C8F4',X'1D',X'E8'                                           66810160
 DC C'HHHHHHH  EEEE     RRRRRRR  CC       UU   UU  LL       EEEE     '  66810170
 DC C' SSS   '                                                          66810180
*                                                                       66810190
 DC X'11',X'4AC4',X'1D',X'E8'                                           66810200
 DC C'HHHHHHH  EEEE     RRRRRR   CC       UU   UU  LL       EEEE     '  66810210
 DC C'   SSS '                                                          66810220
*                                                                       66810230
 DC X'11',X'4BD4',X'1D',X'E8'                                           66810240
 DC C'HH   HH  EE       RR RR    CC   CC  UU   UU  LL       EE       '  66810250
 DC C'SS   SS'                                                          66810260
*                                                                       66810270
 DC X'11',X'4CE4',X'1D',X'E8'                                           66810280
 DC C'HH   HH  EEEEEEE  RR  RR   CCCCCCC  UUUUUUU  LLLLLLL  EEEEEEE  '  66810290
 DC C'SSSSSSS'                                                          66810300
*                                                                       66810310
 DC X'11',X'4DF4',X'1D',X'E8'                                           66810320
 DC C'HH   HH  EEEEEEE  RR   RR   CCCCC    UUUUU   LLLLLLL  EEEEEEE  '  66810330
 DC C' SSSSS '                                                          66810340
*                                                                       66810350
 DC X'11',X'D27B',X'1DF0',X'284100',X'2842F4'                           66810360
 DC C'Welcome to MVS3.8j, running under the Hercules emulator'          66810370
 DC X'1DF0',X'284200'                                                   66810371
*                                                                       66810380
*                                                                       66810390
*                                                                       66810400
*                                                                       66810401
*                                                                       66810410
 DC X'11',X'5B60',X'1D',X'E8'                                           66810420
 DC C'Logon ===>'                                                       66810430
 DC X'1D',X'C1'                                                         66810440
 DC X'13'                                                               66810450
EGDATA DC CL8' ',C' ',CL8' '                                            66810460
EGMSGLN  EQU   *-EGMSG                                                  66810470
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE10
//*
//* ----------------------------------------------------------------- *
//* Step 3: assemble and link-edit new NETSOL                         *
//* ----------------------------------------------------------------- *
//*
//ASM11    EXEC PGM=IFOX00,REGION=1024K,
//             PARM='LINECOUNT(49),TERM'
//SYSLIB   DD  DISP=SHR,DSN=SYS1.MACLIB
//         DD  DISP=SHR,DSN=SYS1.AMODGEN
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(600,100))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(300,50))
//SYSTERM  DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*,DCB=BLKSIZE=1089
//SYSPUNCH DD  DSN=&&A,DISP=(NEW,PASS,DELETE),
//             UNIT=3350,SPACE=(TRK,(2,2)),
//             DCB=(BLKSIZE=80,LRECL=80,RECFM=F)
//SYSIN    DD  *
ISTNSC00 CSECT ,
         NETSOL SYSTEM=VS2
         END   ,
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ASM11
//*
//LKED12   EXEC PGM=IEWL,PARM='XREF,LIST,LET,NCAL',REGION=1024K
//SYSPRINT DD  SYSOUT=*
//SYSLIN   DD  DSN=&&A,DISP=(OLD,DELETE,DELETE)
//SYSLMOD  DD  DISP=SHR,DSN=SYS1.VTAMLIB(ISTNSC00)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(200,20))
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -LKED12
//
