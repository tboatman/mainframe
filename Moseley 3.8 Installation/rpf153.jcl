//RPF153$1 JOB (TSO),
//             'Install RPF',
//             CLASS=S,
//             MSGCLASS=A,
//             MSGLEVEL=(1,1)
//*********************************************************************
//*
//*                       MVS 3.8 SYSGEN
//*                       ==============
//*
//* DESC: Install RPF
//*
//*********************************************************************
//CLEANUP EXEC PGM=IDCAMS
//REPROIN  DD  *
99999999    SEED RECORD FOR THE RPF DATABASE
//SYSPRINT DD  SYSOUT=*
//SYSIN  DD *
 PARM GRAPHICS(CHAIN(SN))
 /***********************************************************/
 /*                                                         */
 /* Note:  you do not need this define alias if you do not  */
 /*        want to use a private user catalog               */
 /*                                                         */
 /* Note:  you need to update the relate parameter to       */
 /*        point to the catalog that you want to use        */
 /*                                                         */
 /***********************************************************/
   DEFINE ALIAS(NAME(RPF) RELATE(UCPUB000))
   SET LASTCC = 0
   SET MAXCC = 0
   DELETE RPF.V1R5M3.SRPFASM  NONVSAM
   DELETE RPF.V1R5M3.SRPFOBJ  NONVSAM
   DELETE RPF.V1R5M3.SRPFHELP NONVSAM
   DELETE RPF.V1R5M3.SRPFLOAD NONVSAM
   DELETE RPF.V1R5M3.CNTL     NONVSAM
   DELETE RPF.V1R5M3.SRPFPROF CLUSTER
   SET LASTCC = 0
   SET MAXCC  = 0
 /***********************************************************/
 /*                                                         */
 /* Note:  You will have to modify the volume names         */
 /*        and the dataset high level qualifiers            */
 /*        to reflect your system environment               */
 /*                                                         */
 /* Note:  Do __not__ modify the low level qualifier, as    */
 /*        they will be needed as is if and when RPF        */
 /*        will be distributed in smp format                */
 /*                                                         */
 /***********************************************************/
  DEFINE CLUSTER ( NAME(RPF.V1R5M3.SRPFPROF) -
                   VOL(PUB000) -
                   FREESPACE(20 10) -
                   RECORDSIZE(1750 1750) -
                   INDEXED -
                   IMBED -
                   UNIQUE  -
                   KEYS(8 0) -
                   CYLINDERS(1 1) -
                 ) -
            DATA ( NAME(RPF.V1R5M3.SRPFPROF.DATA) -
                   SHR(3 3) -
                 ) -
           INDEX ( NAME(RPF.V1R5M3.SRPFPROF.INDEX) -
                   SHR(3 3) -
                 )
  IF LASTCC = 0 THEN -
     REPRO INFILE(REPROIN) -
           OUTDATASET(RPF.V1R5M3.SRPFPROF)
/*
//ALLOC   EXEC PGM=IEBCOPY
//ASM      DD  DISP=(NEW,CATLG),
//             DSN=RPF.V1R5M3.SRPFASM,
//             DCB=SYS1.MACLIB,
//             SPACE=(CYL,(5,5,20)),
//             UNIT=3380,
//             VOL=SER=PUB000
//OBJ      DD  DISP=(NEW,CATLG),
//             DSN=RPF.V1R5M3.SRPFOBJ,
//             DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB),
//             SPACE=(CYL,(1,1,20)),
//             UNIT=3380,
//             VOL=SER=PUB000
//HELP     DD  DISP=(NEW,CATLG),
//             DSN=RPF.V1R5M3.SRPFHELP,
//             DCB=SYS1.MACLIB,
//             SPACE=(CYL,(1,1,5)),
//             UNIT=3380,
//             VOL=SER=PUB000
//LOAD     DD  DISP=(NEW,CATLG),
//             DSN=RPF.V1R5M3.SRPFLOAD,
//             DCB=SYS1.LINKLIB,
//             SPACE=(CYL,(1,1,20)),
//             UNIT=3380,
//             VOL=SER=PUB000
//CNTL     DD  DISP=(NEW,CATLG),
//             DSN=RPF.V1R5M3.CNTL,
//             DCB=SYS1.MACLIB,
//             SPACE=(CYL,(1,1,20)),
//             UNIT=3380,
//             VOL=SER=PUB000
//ASMIN    DD  DISP=OLD,
//             DSN=RPF.ASM,
//             UNIT=(TAPE,,DEFER),
//             VOL=SER=RPF153,
//             LABEL=(1,SL)
//OBJIN    DD  DISP=OLD,
//             DSN=RPF.OBJ,
//             UNIT=AFF=ASMIN,
//             VOL=REF=*.ASMIN,
//             LABEL=(2,SL)
//LOADIN   DD  DISP=OLD,
//             DSN=RPF.LOAD,
//             UNIT=AFF=OBJIN,
//             VOL=REF=*.OBJIN,
//             LABEL=(3,SL)
//CNTLIN  DD   DISP=OLD,
//             DSN=RPF.CNTL,
//             UNIT=AFF=LOADIN,
//             LABEL=(4,SL),
//             VOL=REF=*.OBJIN
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 COPY INDD=ASMIN,OUTDD=ASM
 COPY INDD=OBJIN,OUTDD=OBJ
 COPY INDD=LOADIN,OUTDD=LOAD
 COPY INDD=CNTLIN,OUTDD=CNTL
//GENER   EXEC PGM=IEBUPDTE,PARM=NEW
//SYSUT2   DD  DISP=SHR,DSN=SYS1.PARMLIB
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
./ ADD NAME=RPFKEY00
RPF.V1R5M3.SRPFPROF
HELP=RPF.V1R5M3.SRPFHELP
//ALLOC   EXEC PGM=IEBCOPY
//LOAD     DD  DISP=SHR,DSN=RPF.V1R5M3.SRPFLOAD
//CMDLIB   DD  DISP=SHR,DSN=SYS2.CMDLIB
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 COPY INDD=CMDLIB,OUTDD=CMDLIB
 COPY INDD=((LOAD,R)),OUTDD=CMDLIB
 COPY INDD=CMDLIB,OUTDD=CMDLIB
/*
//HELP    EXEC PGM=IEBCOPY
//ASM      DD  DISP=SHR,DSN=RPF.V1R5M3.SRPFASM
//HELP     DD  DISP=SHR,DSN=RPF.V1R5M3.SRPFHELP
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 COPY INDD=ASM,OUTDD=HELP
 S M=(RPFHELP1,RPFHELP2,RPFHELP3,RPFHELP4,RPFHELP5)
/*
//
