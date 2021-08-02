//SMPJOB00 JOB (SYSGEN),'PREP FOR RECEIVE',                             00010000
//             CLASS=A,MSGLEVEL=(1,1),MSGCLASS=A                        00020000
//*                                                                     00030000
//********************************************************************* 00040000
//* PREPARE TO RECEIVE MVS3.8J PRODUCTS/PTFS/MODS                       00050000
//*   1) DELETE EXISTING SMP DATASETS AND WORK DATASETS                 00060000
//*   2) ALLOCATE SMP DATASETS AND INITIALIZE SMP TARGET ENVIRONMENT    00070000
//********************************************************************* 00080000
//*                                                                     00090000
//IEHPROGM EXEC PGM=IEHPROGM                                            00100000
//SYSPRINT  DD SYSOUT=*                                                 00110000
//DD1       DD UNIT=SYSDA,VOL=SER=SMP000,DISP=OLD                       00120000
//DD2       DD UNIT=SYSDA,VOL=SER=WORK01,DISP=OLD                       00130000
//SYSIN     DD *                                                        00140000
  UNCATLG DSNAME=SYS1.ACMDLIB                                           00150000
  SCRATCH DSNAME=SYS1.ACMDLIB,VOL=SYSDA=SMP000,PURGE                    00160000
  UNCATLG DSNAME=SYS1.AGENLIB                                           00170000
  SCRATCH DSNAME=SYS1.AGENLIB,VOL=SYSDA=SMP000,PURGE                    00180000
  UNCATLG DSNAME=SYS1.AHELP                                             00190000
  SCRATCH DSNAME=SYS1.AHELP,VOL=SYSDA=SMP000,PURGE                      00200000
  UNCATLG DSNAME=SYS1.AIMAGE                                            00210000
  SCRATCH DSNAME=SYS1.AIMAGE,VOL=SYSDA=SMP000,PURGE                     00220000
  UNCATLG DSNAME=SYS1.ALPALIB                                           00230000
  SCRATCH DSNAME=SYS1.ALPALIB,VOL=SYSDA=SMP000,PURGE                    00240000
  UNCATLG DSNAME=SYS1.AMACLIB                                           00250000
  SCRATCH DSNAME=SYS1.AMACLIB,VOL=SYSDA=SMP000,PURGE                    00260000
  UNCATLG DSNAME=SYS1.AMODGEN                                           00270000
  SCRATCH DSNAME=SYS1.AMODGEN,VOL=SYSDA=SMP000,PURGE                    00280000
  UNCATLG DSNAME=SYS1.AOSA0                                             00290000
  SCRATCH DSNAME=SYS1.AOSA0,VOL=SYSDA=SMP000,PURGE                      00300000
  UNCATLG DSNAME=SYS1.AOSA1                                             00310000
  SCRATCH DSNAME=SYS1.AOSA1,VOL=SYSDA=SMP000,PURGE                      00320000
  UNCATLG DSNAME=SYS1.AOSBN                                             00330000
  SCRATCH DSNAME=SYS1.AOSBN,VOL=SYSDA=SMP000,PURGE                      00340000
  UNCATLG DSNAME=SYS1.AOSB0                                             00350000
  SCRATCH DSNAME=SYS1.AOSB0,VOL=SYSDA=SMP000,PURGE                      00360000
  UNCATLG DSNAME=SYS1.AOSB3                                             00370000
  SCRATCH DSNAME=SYS1.AOSB3,VOL=SYSDA=SMP000,PURGE                      00380000
  UNCATLG DSNAME=SYS1.AOSCA                                             00390000
  SCRATCH DSNAME=SYS1.AOSCA,VOL=SYSDA=SMP000,PURGE                      00400000
  UNCATLG DSNAME=SYS1.AOSCD                                             00410000
  SCRATCH DSNAME=SYS1.AOSCD,VOL=SYSDA=SMP000,PURGE                      00420000
  UNCATLG DSNAME=SYS1.AOSCE                                             00430000
  SCRATCH DSNAME=SYS1.AOSCE,VOL=SYSDA=SMP000,PURGE                      00440000
  UNCATLG DSNAME=SYS1.AOSC2                                             00450000
  SCRATCH DSNAME=SYS1.AOSC2,VOL=SYSDA=SMP000,PURGE                      00460000
  UNCATLG DSNAME=SYS1.AOSC5                                             00470000
  SCRATCH DSNAME=SYS1.AOSC5,VOL=SYSDA=SMP000,PURGE                      00480000
  UNCATLG DSNAME=SYS1.AOSC6                                             00490000
  SCRATCH DSNAME=SYS1.AOSC6,VOL=SYSDA=SMP000,PURGE                      00500000
  UNCATLG DSNAME=SYS1.AOSD0                                             00510000
  SCRATCH DSNAME=SYS1.AOSD0,VOL=SYSDA=SMP000,PURGE                      00520000
  UNCATLG DSNAME=SYS1.AOSD7                                             00530000
  SCRATCH DSNAME=SYS1.AOSD7,VOL=SYSDA=SMP000,PURGE                      00540000
  UNCATLG DSNAME=SYS1.AOSD8                                             00550000
  SCRATCH DSNAME=SYS1.AOSD8,VOL=SYSDA=SMP000,PURGE                      00560000
  UNCATLG DSNAME=SYS1.AOSG0                                             00570000
  SCRATCH DSNAME=SYS1.AOSG0,VOL=SYSDA=SMP000,PURGE                      00580000
  UNCATLG DSNAME=SYS1.AOSH1                                             00590000
  SCRATCH DSNAME=SYS1.AOSH1,VOL=SYSDA=SMP000,PURGE                      00600000
  UNCATLG DSNAME=SYS1.AOSH3                                             00610000
  SCRATCH DSNAME=SYS1.AOSH3,VOL=SYSDA=SMP000,PURGE                      00620000
  UNCATLG DSNAME=SYS1.AOST3                                             00630000
  SCRATCH DSNAME=SYS1.AOST3,VOL=SYSDA=SMP000,PURGE                      00640000
  UNCATLG DSNAME=SYS1.AOST4                                             00650000
  SCRATCH DSNAME=SYS1.AOST4,VOL=SYSDA=SMP000,PURGE                      00660000
  UNCATLG DSNAME=SYS1.AOSU0                                             00670000
  SCRATCH DSNAME=SYS1.AOSU0,VOL=SYSDA=SMP000,PURGE                      00680000
  UNCATLG DSNAME=SYS1.AOS00                                             00690000
  SCRATCH DSNAME=SYS1.AOS00,VOL=SYSDA=SMP000,PURGE                      00700000
  UNCATLG DSNAME=SYS1.AOS03                                             00710000
  SCRATCH DSNAME=SYS1.AOS03,VOL=SYSDA=SMP000,PURGE                      00720000
  UNCATLG DSNAME=SYS1.AOS04                                             00730000
  SCRATCH DSNAME=SYS1.AOS04,VOL=SYSDA=SMP000,PURGE                      00740000
  UNCATLG DSNAME=SYS1.AOS05                                             00750000
  SCRATCH DSNAME=SYS1.AOS05,VOL=SYSDA=SMP000,PURGE                      00760000
  UNCATLG DSNAME=SYS1.AOS06                                             00770000
  SCRATCH DSNAME=SYS1.AOS06,VOL=SYSDA=SMP000,PURGE                      00780000
  UNCATLG DSNAME=SYS1.AOS07                                             00790000
  SCRATCH DSNAME=SYS1.AOS07,VOL=SYSDA=SMP000,PURGE                      00800000
  UNCATLG DSNAME=SYS1.AOS11                                             00810000
  SCRATCH DSNAME=SYS1.AOS11,VOL=SYSDA=SMP000,PURGE                      00820000
  UNCATLG DSNAME=SYS1.AOS12                                             00830000
  SCRATCH DSNAME=SYS1.AOS12,VOL=SYSDA=SMP000,PURGE                      00840000
  UNCATLG DSNAME=SYS1.AOS20                                             00850000
  SCRATCH DSNAME=SYS1.AOS20,VOL=SYSDA=SMP000,PURGE                      00860000
  UNCATLG DSNAME=SYS1.AOS21                                             00870000
  SCRATCH DSNAME=SYS1.AOS21,VOL=SYSDA=SMP000,PURGE                      00880000
  UNCATLG DSNAME=SYS1.AOS24                                             00890000
  SCRATCH DSNAME=SYS1.AOS24,VOL=SYSDA=SMP000,PURGE                      00900000
  UNCATLG DSNAME=SYS1.AOS26                                             00910000
  SCRATCH DSNAME=SYS1.AOS26,VOL=SYSDA=SMP000,PURGE                      00920000
  UNCATLG DSNAME=SYS1.AOS29                                             00930000
  SCRATCH DSNAME=SYS1.AOS29,VOL=SYSDA=SMP000,PURGE                      00940000
  UNCATLG DSNAME=SYS1.AOS32                                             00950000
  SCRATCH DSNAME=SYS1.AOS32,VOL=SYSDA=SMP000,PURGE                      00960000
  UNCATLG DSNAME=SYS1.APARMLIB                                          00970000
  SCRATCH DSNAME=SYS1.APARMLIB,VOL=SYSDA=SMP000,PURGE                   00980000
  UNCATLG DSNAME=SYS1.APROCLIB                                          00990000
  SCRATCH DSNAME=SYS1.APROCLIB,VOL=SYSDA=SMP000,PURGE                   01000000
  UNCATLG DSNAME=SYS1.ASAMPLIB                                          01010000
  SCRATCH DSNAME=SYS1.ASAMPLIB,VOL=SYSDA=SMP000,PURGE                   01020000
  UNCATLG DSNAME=SYS1.ATCAMMAC                                          01030000
  SCRATCH DSNAME=SYS1.ATCAMMAC,VOL=SYSDA=SMP000,PURGE                   01040000
  UNCATLG DSNAME=SYS1.ATSOMAC                                           01050000
  SCRATCH DSNAME=SYS1.ATSOMAC,VOL=SYSDA=SMP000,PURGE                    01060000
  UNCATLG DSNAME=SYS1.AUADS                                             01070000
  SCRATCH DSNAME=SYS1.AUADS,VOL=SYSDA=SMP000,PURGE                      01080000
  UNCATLG DSNAME=SYS1.HASPSRC                                           01090000
  SCRATCH DSNAME=SYS1.HASPSRC,VOL=SYSDA=SMP000,PURGE                    01100000
  UNCATLG DSNAME=SYS1.SMPACDS                                           01110000
  SCRATCH DSNAME=SYS1.SMPACDS,VOL=SYSDA=SMP000,PURGE                    01120000
  UNCATLG DSNAME=SYS1.SMPACRQ                                           01130000
  SCRATCH DSNAME=SYS1.SMPACRQ,VOL=SYSDA=SMP000,PURGE                    01140000
  UNCATLG DSNAME=SYS1.SMPMTS                                            01150000
  SCRATCH DSNAME=SYS1.SMPMTS,VOL=SYSDA=SMP000,PURGE                     01160000
  UNCATLG DSNAME=SYS1.SMPPTS                                            01170000
  SCRATCH DSNAME=SYS1.SMPPTS,VOL=SYSDA=SMP000,PURGE                     01180000
  UNCATLG DSNAME=SYS1.SMPSTS                                            01190000
  SCRATCH DSNAME=SYS1.SMPSTS,VOL=SYSDA=SMP000,PURGE                     01200000
  SCRATCH VTOC,VOL=SYSDA=WORK01,PURGE                                   01210000
//*                                                                     01220000
//********************************************************************* 01230000
//* ALLOCATE DATASETS REQUIRED FOR BUILDING DISTRIBUTION LIBRARIES      01240000
//********************************************************************* 01250000
//*                                                                     01260000
//IEFBR14  EXEC PGM=IEFBR14                                             01270000
//ACMDLIB   DD UNIT=3350,DSN=SYS1.ACMDLIB,DISP=(,CATLG),                01280000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(600,4,40)),        01290000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01300000
//AGENLIB   DD UNIT=3350,DSN=SYS1.AGENLIB,DISP=(,CATLG),                01310000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(300,5,20)),         01320001
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    01330000
//AHELP     DD UNIT=3350,DSN=SYS1.AHELP,DISP=(,CATLG),                  01340000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(680,20,17)),       01350000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    01360000
//AIMAGE    DD UNIT=3350,DSN=SYS1.AIMAGE,DISP=(,CATLG),                 01370000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6400,(500,30,100)),      01380000
//             DCB=(LRECL=80,RECFM=FB,BLKSIZE=6400)                     01390000
//ALPALIB   DD UNIT=3350,DSN=SYS1.ALPALIB,DISP=(,CATLG),                01400000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,2,15)),         01410000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01420000
//AMACLIB   DD UNIT=3350,DSN=SYS1.AMACLIB,DISP=(,CATLG),                01430000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(7400,100,50)),     01440000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    01450000
//AMODGEN   DD UNIT=3350,DSN=SYS1.AMODGEN,DISP=(,CATLG),                01460000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(4500,100,50)),     01470000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    01480000
//AOS00     DD UNIT=3350,DSN=SYS1.AOS00,DISP=(,CATLG),                  01490000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(15,1,15)),         01500000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01510000
//AOS03     DD UNIT=3350,DSN=SYS1.AOS03,DISP=(,CATLG),                  01520000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(35,1,15)),         01530000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01540000
//AOS04     DD UNIT=3350,DSN=SYS1.AOS04,DISP=(,CATLG),                  01550000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(30,1,15)),         01560000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01570000
//AOS05     DD UNIT=3350,DSN=SYS1.AOS05,DISP=(,CATLG),                  01580000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,1,7)),          01590000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01600000
//AOS06     DD UNIT=3350,DSN=SYS1.AOS06,DISP=(,CATLG),                  01610000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(75,1,30)),         01620000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01630000
//AOS07     DD UNIT=3350,DSN=SYS1.AOS07,DISP=(,CATLG),                  01640000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(32,1,20)),         01650000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01660000
//AOS11     DD UNIT=3350,DSN=SYS1.AOS11,DISP=(,CATLG),                  01670000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(80,5,15)),         01680000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01690000
//AOS12     DD UNIT=3350,DSN=SYS1.AOS12,DISP=(,CATLG),                  01700000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(320,10,40)),       01710000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01720000
//AOS20     DD UNIT=3350,DSN=SYS1.AOS20,DISP=(,CATLG),                  01730000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(50,2,25)),         01740000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01750000
//AOS21     DD UNIT=3350,DSN=SYS1.AOS21,DISP=(,CATLG),                  01760000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(490,20,167)),      01770000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01780000
//AOS24     DD UNIT=3350,DSN=SYS1.AOS24,DISP=(,CATLG),                  01790000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(240,25,50)),       01800000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01810000
//AOS26     DD UNIT=3350,DSN=SYS1.AOS26,DISP=(,CATLG),                  01820000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(550,25,160)),      01830000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01840000
//AOS29     DD UNIT=3350,DSN=SYS1.AOS29,DISP=(,CATLG),                  01850000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(114,19,27)),       01860000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01870000
//AOS32     DD UNIT=3350,DSN=SYS1.AOS32,DISP=(,CATLG),                  01880000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(200,38,60)),       01890000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01900000
//AOSA0     DD UNIT=3350,DSN=SYS1.AOSA0,DISP=(,CATLG),                  01910000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(210,10,70)),       01920000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01930000
//AOSA1     DD UNIT=3350,DSN=SYS1.AOSA1,DISP=(,CATLG),                  01940000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(10,1,30)),         01950000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01960000
//AOSB0     DD UNIT=3350,DSN=SYS1.AOSB0,DISP=(,CATLG),                  01970000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(9,1,5)),           01980000
//             DCB=(BLKSIZE=6144,RECFM=U)                               01990000
//AOSB3     DD UNIT=3350,DSN=SYS1.AOSB3,DISP=(,CATLG),                  02000000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(320,3,100)),       02010000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02020000
//AOSBN     DD UNIT=3350,DSN=SYS1.AOSBN,DISP=(,CATLG),                  02030000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(130,10,25)),       02040000
//             DCB=(RECFM=U,BLKSIZE=6144)                               02050000
//AOSC2     DD UNIT=3350,DSN=SYS1.AOSC2,DISP=(,CATLG),                  02060000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(5,2,4)),           02070000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02080000
//AOSC5     DD UNIT=3350,DSN=SYS1.AOSC5,DISP=(,CATLG),                  02090000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(500,20,150)),      02100000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02110000
//AOSC6     DD UNIT=3350,DSN=SYS1.AOSC6,DISP=(,CATLG),                  02120000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,1,12)),         02130000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02140000
//AOSCA     DD UNIT=3350,DSN=SYS1.AOSCA,DISP=(,CATLG),                  02150000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(11,1,6)),          02160000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02170000
//AOSCD     DD UNIT=3350,DSN=SYS1.AOSCD,DISP=(,CATLG),                  02180000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(290,20,51)),       02190000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02200000
//AOSCE     DD UNIT=3350,DSN=SYS1.AOSCE,DISP=(,CATLG),                  02210000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(55,5,15)),         02220000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02230000
//AOSD0     DD UNIT=3350,DSN=SYS1.AOSD0,DISP=(,CATLG),                  02240000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(465,10,150)),      02250000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02260000
//AOSD7     DD UNIT=3350,DSN=SYS1.AOSD7,DISP=(,CATLG),                  02270000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(20,1,15)),         02280000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02290000
//AOSD8     DD UNIT=3350,DSN=SYS1.AOSD8,DISP=(,CATLG),                  02300000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(60,2,25)),         02310000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02320000
//AOSG0     DD UNIT=3350,DSN=SYS1.AOSG0,DISP=(,CATLG),                  02330000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(25,5,35)),         02340001
//             DCB=(BLKSIZE=6144,RECFM=U)                               02350000
//AOSH1     DD UNIT=3350,DSN=SYS1.AOSH1,DISP=(,CATLG),                  02360000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(5,1,3)),           02370000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02380000
//AOSH3     DD UNIT=3350,DSN=SYS1.AOSH3,DISP=(,CATLG),                  02390000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(100,5,10)),        02400000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02410000
//AOST3     DD UNIT=3350,DSN=SYS1.AOST3,DISP=(,CATLG),                  02420000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(75,5,30)),         02430000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02440000
//AOST4     DD UNIT=3350,DSN=SYS1.AOST4,DISP=(,CATLG),                  02450000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(190,20,35)),       02460000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02470000
//AOSU0     DD UNIT=3350,DSN=SYS1.AOSU0,DISP=(,CATLG),                  02480000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(6144,(710,20,150)),      02490000
//             DCB=(BLKSIZE=6144,RECFM=U)                               02500000
//APARMLIB  DD UNIT=3350,DSN=SYS1.APARMLIB,DISP=(,CATLG),               02510000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(80,(320,10,17)),         02520000
//             DCB=(RECFM=F,BLKSIZE=80)                                 02530000
//APROCLIB  DD UNIT=3350,DSN=SYS1.APROCLIB,DISP=(,CATLG),               02540000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(800,(85,10,17)),         02550000
//             DCB=(RECFM=FB,BLKSIZE=800,LRECL=80)                      02560000
//ASAMPLIB  DD UNIT=3350,DSN=SYS1.ASAMPLIB,DISP=(,CATLG),               02570000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(120,5,20)),         02580001
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02590000
//ATCAMMAC  DD UNIT=3350,DSN=SYS1.ATCAMMAC,DISP=(,CATLG),               02600000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(2000,500,55)),     02610000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02620000
//ATSOMAC   DD UNIT=3350,DSN=SYS1.ATSOMAC,DISP=(,CATLG),                02630000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(1030,20,17)),      02640000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02650000
//AUADS     DD UNIT=3350,DSN=SYS1.AUADS,DISP=(,CATLG),                  02660000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(2,1,2)),           02670000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02680000
//HASPSRC   DD UNIT=3350,DSN=SYS1.HASPSRC,DISP=(,CATLG),                02690000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(5000,500,15)),     02700000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02710000
//SMPACDS   DD UNIT=3350,DSN=SYS1.SMPACDS,DISP=(,CATLG),                02720000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(9000,1000,3000)),  02730000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02740000
//SMPACRQ   DD UNIT=3350,DSN=SYS1.SMPACRQ,DISP=(,CATLG),                02750000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(100,100,84)),      02760000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02770000
//SMPMTS    DD UNIT=3350,DSN=SYS1.SMPMTS,DISP=(,CATLG),                 02780000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(100,100,20)),      02790000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02800000
//SMPPTS    DD UNIT=3350,DSN=SYS1.SMPPTS,DISP=(,CATLG),                 02810000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(TRK,(3330,60,1000)),     02820001
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02830000
//SMPSTS    DD UNIT=3350,DSN=SYS1.SMPSTS,DISP=(,CATLG),                 02840000
//             VOL=(,RETAIN,SER=SMP000),SPACE=(1680,(40,40,20)),        02850000
//             DCB=(RECFM=FB,BLKSIZE=19040,LRECL=80)                    02860000
//*                                                                     02870000
//* ***************************************************************** * 02880000
//* INITIALIZE SMP ENVIRONMENT                                        * 02890000
//* ***************************************************************** * 02900000
//*                                                                     02910000
//DLBUCL EXEC DLBSMP                                                    02920000
//SMPCNTL  DD  *                                                        02930000
 UCLIN PTS .                                                            02940000
    DEL SYS .                                                           02950000
    ADD SYS ASMNAME(IFOX00)                                             02960000
            ASMPARM(XREF(SHORT),NOLOAD,DECK,LINECOUNT(56))              02970000
            ASMPRINT(ASMPRINT)                                          02980000
            ASMRC(4)                                                    02990000
            COMPNAME(IEBCOPY)                                           03000000
            COMPPARM(SIZE=2048K)                                        03010000
            COMPPRINT(CMPPRINT)                                         03020000
            COMPRC(0)                                                   03030000
            COPYNAME(IEBCOPY)                                           03040000
            COPYPRINT(COPPRINT)                                         03050000
            COPYPARM(SIZE=2048K)                                        03060000
            COPYRC(0)                                                   03070000
            DSPREFIX(MVS.SCRATCH.TLIB)                                  03080000
            DSSPACE(200,200,250)                                        03090000
            LKEDNAME(IEWL)                                              03100000
            LKEDPARM(SIZE=(500K,80K),NCAL,LIST,LET,XREF)                03110000
            LKEDPRINT(LKDPRINT)                                         03120000
            LKEDRC(8)                                                   03130000
            PAGELEN(0061)                                               03140000
            PEMAX(9999)                                                 03150000
            RETRYNAME(IEBCOPY)                                          03160000
            RETRYPARM(SIZE=2048K)                                       03170000
            RETRYPRINT(E37PRINT)                                        03180000
            RETRYRC(0)                                                  03190000
            SREL(Z038)                                                  03200000
            UPDATNAME(IEBUPDTE)                                         03210000
            UPDATPRINT(UPDPRINT)                                        03220000
            UPDATRC(0)                                                  03230000
            ZAPNAME(AMASPZAP)                                           03240000
            ZAPPARM(IGNIDRFULL)                                         03250000
            ZAPPRINT(ZAPPRINT)                                          03260000
            ZAPRC(4)                                                    03270000
            .                                                           03280000
 ENDUCL .                                                               03290000
 RESETRC .                                                              03300000
 LIST PTS SYS .                                                         03310000
 UCLIN ACDS .                                                           03320000
    DEL SYS .                                                           03330000
    ADD SYS CDSID(MVS)                                                  03340000
            NUCID(2)                                                    03350000
            PEMAX(9999)                                                 03360000
            RETRYDDN(ALL)                                               03370000
            SREL(Z038)                                                  03380000
            .                                                           03390000
 ENDUCL .                                                               03400000
 LIST ACDS SYS .                                                        03410000
//*                                                                     03420000
//SMPPTFIN DD  DUMMY                                                    03430000
//                                                                      03440000
