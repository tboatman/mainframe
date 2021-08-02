//SYSGEN01 JOB 'ASSEMBLE STAGE 1',                                      00010003
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        00020003
/*JOBPARM  LINES=100                                                    00030003
//*                                                                     00040003
//CLEANUP  EXEC PGM=IEHPROGM                                            00050003
//* ***************************************************************** * 00060003
//* DELETE STAGE 1 OUTPUT FROM PRIOR RUN (IF ANY)                     * 00070003
//* ***************************************************************** * 00080003
//SYSPRINT DD  SYSOUT=*                                                 00090003
//DD1      DD  UNIT=3350,VOL=SER=WORK01,DISP=OLD                        00100003
//SYSIN    DD  *                                                        00110003
  SCRATCH DSNAME=SYS1.STAGE1.OUTPUT,VOL=3350=WORK01,PURGE               00120003
//*                                                                     00130003
//ASMBLR   EXEC PGM=ASMBLR,PARM='LIST,NOLOAD,DECK,NOXREF',REGION=2056K  00140003
//* ***************************************************************** * 00150003
//* ASSEMBLE STAGE 1                                                  * 00160003
//* ***************************************************************** * 00170003
//SYSPRINT DD  SYSOUT=*                                                 00180003
//SYSLIB   DD  DSN=SYS1.AMODGEN,DISP=SHR                                00190003
//         DD  DSN=SYS1.AGENLIB,DISP=SHR                                00200003
//SYSPUNCH DD  DSN=SYS1.STAGE1.OUTPUT,DISP=(NEW,KEEP),                  00210003
//             UNIT=3350,VOL=SER=WORK01,                                00220003
//             SPACE=(TRK,(30,30),RLSE),                                00230003
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=3120)                     00240003
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(35,10))                           00250003
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(35,10))                           00260003
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(50,10))                           00270003
//SYSIN    DD  *                                                        00280003
         PRINT ON,NOGEN,NODATA                                          00290003
*                                                                       00300003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00310003
*                      INPUT/OUTPUT CHANNELS                          * 00320003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00330003
*                                                                       00340003
MPLXR    CHANNEL                                                       C00350003
               ADDRESS=(0),                                            C00360003
               TYPE=MULTIPLEXOR                                         00370003
*                                                                       00380003
SELECTOR CHANNEL                                                       C00390003
               ADDRESS=(1,2,3,4,5,6),                                  C00400003
               TYPE=SELECTOR                                            00410003
         EJECT                                                          00420003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00430003
*                         SYSTEM CONSOLES                             * 00440003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00450003
*                                                                       00460003
CONSMST  CONSOLE                                                       C00470003
               MCONS=010,                                              C00480003
               ALTCONS=009,                                            C00490003
               AREA=04,                                                C00500003
               PFK=12,                                                 C00510003
               ROUTCDE=ALL                                              00520003
*                                                                       00530003
CONSALT  CONSOLE                                                       C00540003
               SECONS=009,                                             C00550003
               ALTCONS=010,                                            C00560003
               AREA=04,                                                C00570003
               PFK=12,                                                 C00580003
               ROUTCDE=ALL                                              00590003
*                                                                       00600003
CONSHC   CONSOLE                                                       C00610003
               SECONS=O-015,                                           C00620003
               ALTCONS=009,                                            C00630003
               ROUTCDE=ALL                                              00640003
*                                                                       00650003
MSTCONS  IODEVICE                                                      C00660003
               UNIT=3277,                                              C00670003
               MODEL=2,                                                C00680003
               ADDRESS=010,                                            C00690003
               FEATURE=(EBKY3277,DOCHAR,KB78KEY,AUDALRM,NUMLOCK,SELPEN) 00700003
*                                                                       00710003
ALTCONS  IODEVICE                                                      C00720003
               UNIT=3277,                                              C00730003
               MODEL=2,                                                C00740003
               ADDRESS=009,                                            C00750003
               FEATURE=(EBKY3277,DOCHAR,KB78KEY,AUDALRM,NUMLOCK,SELPEN) 00760003
*                                                                       00770003
HCCONS   IODEVICE                                                      C00780003
               UNIT=1403,                                              C00790003
               MODEL=2,                                                C00800003
               ADDRESS=015                                              00810003
         EJECT                                                          00820003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00830003
*                          CHANNEL 0 DEVICES                          * 00840003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00850003
*                                                                       00860003
RDR00A   IODEVICE                                                      C00870003
               UNIT=3505,                                              C00880003
               ADDRESS=00A,                                            C00890003
               FEATURE=CARDIMAGE                                        00900003
*                                                                       00910003
PCH00B   IODEVICE                                                      C00920003
               UNIT=3525,                                              C00930003
               ADDRESS=00B,                                            C00940003
               FEATURE=CARDIMAGE                                        00950003
*                                                                       00960003
RDR00C   IODEVICE                                                      C00970003
               UNIT=2540R,                                             C00980003
               MODEL=1,                                                C00990003
               ADDRESS=00C,                                            C01000003
               FEATURE=CARDIMAGE                                        01010003
*                                                                       01020003
PCH00D   IODEVICE                                                      C01030003
               UNIT=2540P,                                             C01040003
               MODEL=1,                                                C01050003
               ADDRESS=00D,                                            C01060003
               FEATURE=CARDIMAGE                                        01070003
*                                                                       01080003
PRT00E   IODEVICE                                                      C01090003
               UNIT=1403,                                              C01100003
               MODEL=N1,                                               C01110003
               ADDRESS=00E,                                            C01120003
               FEATURE=UNVCHSET                                         01130003
*                                                                       01140003
PRT00F   IODEVICE                                                      C01150003
               UNIT=3211,                                              C01160003
               ADDRESS=00F                                              01170003
*                                                                       01180003
RDR01A   IODEVICE                                                      C01190003
               UNIT=2540R,                                             C01200003
               MODEL=1,                                                C01210003
               ADDRESS=01A,                                            C01220003
               FEATURE=CARDIMAGE                                        01230003
*                                                                       01240003
PCH01B   IODEVICE                                                      C01250003
               UNIT=2540P,                                             C01260003
               MODEL=1,                                                C01270003
               ADDRESS=01B,                                            C01280003
               FEATURE=CARDIMAGE                                        01290003
*                                                                       01300003
RDR01C   IODEVICE                                                      C01310003
               UNIT=2540R,                                             C01320003
               MODEL=1,                                                C01330003
               ADDRESS=01C,                                            C01340003
               FEATURE=CARDIMAGE                                        01350003
*                                                                       01360003
PCH01D   IODEVICE                                                      C01370003
               UNIT=2540P,                                             C01380003
               MODEL=1,                                                C01390003
               ADDRESS=01D,                                            C01400003
               FEATURE=CARDIMAGE                                        01410003
*                                                                       01420003
PRT01E   IODEVICE                                                      C01430003
               UNIT=1403,                                              C01440003
               MODEL=N1,                                               C01450003
               ADDRESS=01E,                                            C01460003
               FEATURE=UNVCHSET                                         01470003
*                                                                       01480003
PRT01F   IODEVICE                                                      C01490003
               UNIT=3211,                                              C01500003
               ADDRESS=01F                                              01510003
         EJECT                                                          01520003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01530003
*                          CHANNEL 1 DEVICES                          * 01540003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 01550003
*                                                                       01560003
T2401@1  IODEVICE                                                      C01570003
               UNIT=2401,                                              C01580003
               MODEL=6,                                                C01590003
               ADDRESS=(100,4),                                        C01600003
               FEATURE=(9-TRACK,DUALDENS)                               01610003
*                                                                       01620003
T3420@1  IODEVICE                                                      C01630003
               UNIT=3420,                                              C01640003
               MODEL=8,                                                C01650003
               ADDRESS=(110,4),                                        C01660003
               FEATURE=(9-TRACK,OPT1600)                                01670003
*                                                                       01680003
D2314@1  IODEVICE                                                      C01690003
               UNIT=2314,                                              C01700003
               ADDRESS=(120,8)                                          01710003
*                                                                       01720003
D33301@1 IODEVICE                                                      C01730003
               MODEL=1,                                                C01740003
               UNIT=3330,                                              C01750003
               ADDRESS=(130,8)                                          01760003
*                                                                       01770003
D33302@1 IODEVICE                                                      C01780003
               MODEL=11,                                               C01790003
               UNIT=3330,                                              C01800003
               ADDRESS=(138,8)                                          01810003
*                                                                       01820003
D3340@1  IODEVICE                                                      C01830003
               UNIT=3340,                                              C01840003
               ADDRESS=(140,8)                                          01850003
*                                                                       01860003
D3350@1  IODEVICE                                                      C01870003
               UNIT=3350,                                              C01880003
               ADDRESS=(150,8)                                          01890003
*                                                                       01900003
D3375@1  IODEVICE                                                      C01910003
               UNIT=3375,                                              C01920003
               ADDRESS=(170,8)                                          01930003
*                                                                       01940003
D3380@1  IODEVICE                                                      C01950003
               UNIT=3380,                                              C01960003
               ADDRESS=(180,8)                                          01970003
*                                                                       01980003
D3390@1  IODEVICE                                                      C01990003
               UNIT=3390,                                              C02000003
               ADDRESS=(190,8)                                          02010003
         EJECT                                                          02020003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02030003
*                          CHANNEL 2 DEVICES                          * 02040003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02050003
*                                                                       02060003
T2401@2  IODEVICE                                                      C02070003
               UNIT=2401,                                              C02080003
               MODEL=6,                                                C02090003
               ADDRESS=(200,4),                                        C02100003
               FEATURE=(9-TRACK,DUALDENS)                               02110003
*                                                                       02120003
T3420@2  IODEVICE                                                      C02130003
               UNIT=3420,                                              C02140003
               MODEL=8,                                                C02150003
               ADDRESS=(210,4),                                        C02160003
               FEATURE=(9-TRACK,OPT1600)                                02170003
*                                                                       02180003
D2314@2  IODEVICE                                                      C02190003
               UNIT=2314,                                              C02200003
               ADDRESS=(220,8)                                          02210003
*                                                                       02220003
D33301@2 IODEVICE                                                      C02230003
               MODEL=1,                                                C02240003
               UNIT=3330,                                              C02250003
               ADDRESS=(230,8)                                          02260003
*                                                                       02270003
D33302@2 IODEVICE                                                      C02280003
               MODEL=11,                                               C02290003
               UNIT=3330,                                              C02300003
               ADDRESS=(238,8)                                          02310003
*                                                                       02320003
D3340@2  IODEVICE                                                      C02330003
               UNIT=3340,                                              C02340003
               ADDRESS=(240,8)                                          02350003
*                                                                       02360003
D3350@2  IODEVICE                                                      C02370003
               UNIT=3350,                                              C02380003
               ADDRESS=(250,8)                                          02390003
*                                                                       02400003
D3375@2  IODEVICE                                                      C02410003
               UNIT=3375,                                              C02420003
               ADDRESS=(270,8)                                          02430003
*                                                                       02440003
D3380@2  IODEVICE                                                      C02450003
               UNIT=3380,                                              C02460003
               ADDRESS=(280,8)                                          02470003
*                                                                       02480003
D3390@2  IODEVICE                                                      C02490003
               UNIT=3390,                                              C02500003
               ADDRESS=(290,8)                                          02510003
         EJECT                                                          02520003
*                                                                       02530003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02540003
*                          CHANNEL 3 DEVICES                          * 02550003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 02560003
*                                                                       02570003
T2401@3  IODEVICE                                                      C02580003
               UNIT=2401,                                              C02590003
               MODEL=6,                                                C02600003
               ADDRESS=(300,4),                                        C02610003
               FEATURE=(9-TRACK,DUALDENS)                               02620003
*                                                                       02630003
T3420@3  IODEVICE                                                      C02640003
               UNIT=3420,                                              C02650003
               MODEL=8,                                                C02660003
               ADDRESS=(310,4),                                        C02670003
               FEATURE=(9-TRACK,OPT1600)                                02680003
*                                                                       02690003
D2314@3  IODEVICE                                                      C02700003
               UNIT=2314,                                              C02710003
               ADDRESS=(320,8)                                          02720003
*                                                                       02730003
D33301@3 IODEVICE                                                      C02740003
               MODEL=1,                                                C02750003
               UNIT=3330,                                              C02760003
               ADDRESS=(330,8)                                          02770003
*                                                                       02780003
D33302@3 IODEVICE                                                      C02790003
               MODEL=11,                                               C02800003
               UNIT=3330,                                              C02810003
               ADDRESS=(338,8)                                          02820003
*                                                                       02830003
D3340@3  IODEVICE                                                      C02840003
               UNIT=3340,                                              C02850003
               ADDRESS=(340,8)                                          02860003
*                                                                       02870003
D3350@3  IODEVICE                                                      C02880003
               UNIT=3350,                                              C02890003
               ADDRESS=(350,8)                                          02900003
*                                                                       02910003
D3375@3  IODEVICE                                                      C02920003
               UNIT=3375,                                              C02930003
               ADDRESS=(370,8)                                          02940003
*                                                                       02950003
D3380@3  IODEVICE                                                      C02960003
               UNIT=3380,                                              C02970003
               ADDRESS=(380,8)                                          02980003
*                                                                       02990003
D3390@3  IODEVICE                                                      C03000003
               UNIT=3390,                                              C03010003
               ADDRESS=(390,8)                                          03020003
         EJECT                                                          03030003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 03040003
*                          CHANNEL 4 DEVICES                          * 03050003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 03060003
T3278@4  IODEVICE                                                      C03070003
               UNIT=3277,                                              C03080003
               MODEL=2,                                                C03090003
               ADDRESS=(400,32),                                       C03100003
               FEATURE=(EBKY3277,DOCHAR,KB78KEY,AUDALRM,NUMLOCK)        03110003
         EJECT                                                          03120003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 03130003
*                          CHANNEL 5 DEVICES                          * 03140003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 03150003
CTC1@5   IODEVICE                                                      C03160003
               UNIT=CTC,                                               C03170003
               ADDRESS=(500,4)                                          03180003
*                                                                       03190003
CTC2@5   IODEVICE                                                      C03200003
               UNIT=CTC,                                               C03210003
               FEATURE=(370),                                          C03220003
               ADDRESS=(510,4)                                          03230003
         EJECT                                                          03240003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 03250003
*                          CHANNEL 6 DEVICES                          * 03260003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 03270003
C37051@6 IODEVICE                                                      C03280003
               UNIT=3705,                                              C03290003
               ADAPTER=CA1,                                            C03300003
               ADDRESS=600                                              03310003
*                                                                       03320003
C37052@6 IODEVICE                                                      C03330003
               UNIT=3705,                                              C03340003
               ADAPTER=CA2,                                            C03350003
               ADDRESS=601                                              03360003
*                                                                       03370003
BSC1@6   IODEVICE                                                      C03380003
               UNIT=BSC1,                                              C03390003
               ADAPTER=BSCA,                                           C03400003
               TCU=2703,                                               C03410003
               ADDRESS=602                                              03420003
*                                                                       03430003
BSC2@6   IODEVICE                                                      C03440003
               UNIT=BSC2,                                              C03450003
               ADAPTER=BSCA,                                           C03460003
               TCU=2703,                                               C03470003
               FEATURE=(AUTOANSR,AUTOCALL),                            C03480003
               ADDRESS=603                                              03490003
*                                                                       03500003
BSC3@6   IODEVICE                                                      C03510003
               UNIT=BSC3,                                              C03520003
               ADAPTER=BSCA,                                           C03530003
               TCU=2703,                                               C03540003
               ADDRESS=604                                              03550003
*                                                                       03560003
T2740C@6  IODEVICE                                                     C03570003
               UNIT=2740C,                                             C03580003
               ADAPTER=IBM1,                                           C03590003
               TCU=2701,                                               C03600003
               FEATURE=(AUTOANSR,CHECKING),                            C03610003
               ADDRESS=605                                              03620003
*                                                                       03630003
T2740X@6  IODEVICE                                                     C03640003
               UNIT=2740X,                                             C03650003
               ADAPTER=IBM1,                                           C03660003
               TCU=2701,                                               C03670003
               FEATURE=(AUTOANSR,CHECKING),                            C03680003
               ADDRESS=606                                              03690003
*                                                                       03700003
T2741P@6  IODEVICE                                                     C03710003
               UNIT=2741P,                                             C03720003
               ADAPTER=IBM1,                                           C03730003
               TCU=2701,                                               C03740003
               FEATURE=(AUTOANSR),                                     C03750003
               ADDRESS=607                                              03760003
*                                                                       03770003
T2741C@6  IODEVICE                                                     C03780003
               UNIT=2741C,                                             C03790003
               ADAPTER=IBM1,                                           C03800003
               TCU=2701,                                               C03810003
               FEATURE=(AUTOANSR),                                     C03820003
               ADDRESS=608                                              03830003
*                                                                       03840003
T1030@6   IODEVICE                                                     C03850003
               UNIT=1030,                                              C03860003
               ADAPTER=IBM2,                                           C03870003
               TCU=2701,                                               C03880003
               ADDRESS=609                                              03890003
*                                                                       03900003
T115A@6   IODEVICE                                                     C03910003
               UNIT=115A,                                              C03920003
               ADAPTER=TELE1,                                          C03930003
               TCU=2701,                                               C03940003
               FEATURE=(AUTOANSR),                                     C03950003
               ADDRESS=60A                                              03960003
*                                                                       03970003
T83B3@6   IODEVICE                                                     C03980003
               UNIT=83B3,                                              C03990003
               ADAPTER=TELE1,                                          C04000003
               TCU=2701,                                               C04010003
               FEATURE=(AUTOANSR),                                     C04020003
               ADDRESS=60B                                              04030003
*                                                                       04040003
T2265@6   IODEVICE                                                     C04050003
               UNIT=2265,                                              C04060003
               ADAPTER=IBM3,                                           C04070003
               TCU=2701,                                               C04080003
               FEATURE=(AUTOANSR),                                     C04090003
               ADDRESS=60C                                              04100003
*                                                                       04110003
TWX@6    IODEVICE                                                      C04120003
               UNIT=TWX,                                               C04130003
               ADAPTER=TELE2,                                          C04140003
               TCU=2703,                                               C04150003
               FEATURE=(AUTOANSR),                                     C04160003
               ADDRESS=(60D,4)                                          04170003
*                                                                       04180003
WTTA@6   IODEVICE                                                      C04190003
               UNIT=WTTA,                                              C04200003
               ADAPTER=TELEW,                                          C04210003
               TCU=2703,                                               C04220003
               FEATURE=(AUTOANSR),                                     C04230003
               ADDRESS=(611,4)                                          04240003
         EJECT                                                          04250003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 04260003
*                      SYMBOLIC DEVICE NAMES                          * 04270003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 04280003
SYSRDR   UNITNAME                                                      C04290003
               NAME=SYSRDR,                                            C04300003
               UNIT=(00A)                                               04310003
*                                                                       04320003
SYSDA    UNITNAME                                                      C04330003
               NAME=SYSDA,                                             C04340003
               UNIT=((130,8),(138,8),(140,8),(150,8),                  C04350003
               (170,8),(180,8),(190,8))                                 04360003
*                                                                       04370003
SYSDA    UNITNAME                                                      C04380003
               NAME=SYSDA,                                             C04390003
               UNIT=((230,8),(238,8),(240,8),(250,8),                  C04400003
               (270,8),(280,8),(290,8))                                 04410003
*                                                                       04420003
SYSDA    UNITNAME                                                      C04430003
               NAME=SYSDA,                                             C04440003
               UNIT=((330,8),(338,8),(340,8),(350,8),                  C04450003
               (370,8),(380,8),(390,8))                                 04460003
*                                                                       04470003
SORTDA   UNITNAME                                                      C04480003
               NAME=SORTDA,                                            C04490003
               UNIT=((120,8),(220,8),(320,8))                           04500003
*                                                                       04510003
SYSALLDA UNITNAME                                                      C04520003
               NAME=SYSALLDA,                                          C04530003
               UNIT=((120,8),(220,8),(320,8))                           04540003
*                                                                       04550003
SYSALLDA UNITNAME                                                      C04560003
               NAME=SYSALLDA,                                          C04570003
               UNIT=((130,8),(138,8),(140,8),(150,8),                  C04580003
               (170,8),(180,8),(190,8))                                 04590003
*                                                                       04600003
SYSALLDA UNITNAME                                                      C04610003
               NAME=SYSALLDA,                                          C04620003
               UNIT=((230,8),(238,8),(240,8),(250,8),                  C04630003
               (270,8),(280,8),(290,8))                                 04640003
*                                                                       04650003
SYSALLDA UNITNAME                                                      C04660003
               NAME=SYSALLDA,                                          C04670003
               UNIT=((330,8),(338,8),(340,8),(350,8),                  C04680003
               (370,8),(380,8),(390,8))                                 04690003
*                                                                       04700003
TAPE     UNITNAME                                                      C04710003
               NAME=TAPE,                                              C04720003
               UNIT=((100,4),(110,4),(200,4),(210,4),(300,4),(310,4))   04730003
*                                                                       04740003
SYSSQ    UNITNAME                                                      C04750003
               NAME=SYSSQ,                                             C04760003
               UNIT=((100,4),(110,4),(200,4),(210,4),(300,4),(310,4),  C04770003
               (130,8),(138,8),(140,8),(150,8),                        C04780003
               (170,8),(180,8),(190,8),                                C04790003
               (230,8),(238,8),(240,8),(250,8),                        C04800003
               (270,8),(280,8),(290,8),                                C04810003
               (330,8),(338,8),(340,8),(350,8),                        C04820003
               (370,8),(380,8),(390,8))                                 04830003
*                                                                       04840003
T2401    UNITNAME                                                      C04850003
               NAME=T2401,                                             C04860003
               UNIT=((100,4),(200,4),(300,4))                           04870003
*                                                                       04880003
T3420    UNITNAME                                                      C04890003
               NAME=T3420,                                             C04900003
               UNIT=((110,4),(210,4),(310,4))                           04910003
         EJECT                                                          04920003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 04930003
*                         VIRTUAL I/O DEVICES                         * 04940003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 04950003
* THE DEVICES SPECIFIED FOR THE VIO UNITNAME ARE 3350.  IF VIO IS USED  04960003
* THE EMULATED DEVICE WILL TAKE ON THE CHARACTERISTICS OF A 3350.       04970003
*                                                                       04980003
VIO      UNITNAME                                                      C04990003
               NAME=VIO,                                               C05000003
               VIO=YES,                                                C05010003
               UNIT=((150,8),(250,8),(350,8))                           05020003
         EJECT                                                          05030003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05040003
*                       CONTROL PROGRAM OPTIONS                       * 05050003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05060003
*                                                                       05070003
CTLPG    CTRLPROG ASCII=INCLUDE,     ASCII TRANSLATE ROUTINE           C05080003
               OPTIONS=(RER,         REDUCED ERROR RECOVERY            C05090003
               DEVSTAT,              OFFLINE NOT READY DEVICES AT IPL  C05100003
               RDE,                  LOGREC DATA EXTRACTOR             C05110003
               BLDL),                BLDL IN FIXED STORAGE             C05120003
               SQA=5,                # 64K ADDITIONAL SQA BLOCKS       C05130003
               REAL=128,             # 1K V=R BLOCKS                   C05140003
               STORAGE=0,            DETERMINE MAX REAL DYNAMICALLY    C05150003
               WARN=0,               IGNORE POWER WARN FEATURE         C05160003
               ACRCODE=NO,           ALTERNATE PROCESSOR RECOVERY      C05170003
               APFLIB=(SYS1.VTAMLIB,MVSRES, VTAM REQUIRES              C05180003
               SYS1.INDMAC,MVSRES),         IND=YES REQUIRES           C05190003
               CSA=3072,             # 1K BLOCKS CSA                   C05200003
               VRREGN=64,            DEFAULT V=R REGION                C05210003
               TZ=(W,5)              ONE HOUR WEST OF GMT               05220003
         EJECT                                                          05230003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05240003
*                     JOB SCHEDULER OPTIONS                           * 05250003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05260003
*                                                                       05270003
SCHDL    SCHEDULR BCLMT=100,              BROADCAST NOTICE LIMIT       C05280003
               HARDCPY=(015,ALL,CMDS),    RECORD EVERYTHING ON HC LOG  C05290003
               PRISUB=JES2,               JES2 SUBSYSTEM               C05300003
               DEVPREF=(3350,3380,3330-1, DEVICE ALLOCATION PREFERENCE C05310003
               3330,3390,3340)                                          05320003
         EJECT                                                          05330003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05340003
*                           TSO OPTIONS                               * 05350003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05360003
*                                                                       05370003
TSOOPTS  TSO CMDS=YES,LOGLINE=4,LOGTIME=50                              05380003
         EJECT                                                          05390003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05400003
*                      OPTIONAL ACCESS METHODS                        * 05410003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05420003
*                                                                       05430003
OPTAM    DATAMGT                                                       C05440003
               ACSMETH=(BTAM,     BASIC TELECOMMUNICATIONS             C05450003
               ISAM,              INDEXED SEQUENTIAL                   C05460003
               TCAM,              TELECOMMUNICATIONS                   C05470003
               VTAM,              VIRTUAL TELECOMMUNCATIONS            C05480003
               GAM),              GRAPHICS                             C05490003
               IND=YES,           3270 SUPPORT                         C05500003
               TABLE=ALL,         ALL CHARACTER TABLES FOR 3800        C05510003
               UCSDFLT=ALL        USE DEFAULT UNIV CHAR SET             05520003
         EJECT                                                          05530003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05540003
*                         SYSTEM DATASETS                             * 05550003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 05560003
*                                                                       05570003
BRODCAST DATASET BRODCAST,VOL=(MVSRES,3350),SPACE=(CYL,(1))             05580003
CMDLIB   DATASET CMDLIB,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,71))          05590003
DCMLIB   DATASET DCMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(1,,35))           05600003
DUMP00   DATASET DUMP00,VOL=(MVSRES,3350),SPACE=(CYL,(30))              05610003
DUMP01   DATASET DUMP01,VOL=(MVSRES,3350),SPACE=(CYL,(30))              05620003
DUMP02   DATASET DUMP02,VOL=(MVSRES,3350),SPACE=(CYL,(30))              05630003
HELP     DATASET HELP,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,71))            05640003
IMAGE    DATASET IMAGELIB,VOL=(MVSRES,3350),SPACE=(CYL,(1,,35))         05650003
INDMAC   DATASET INDMAC,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,71))          05660003
LINKLIB  DATASET LINKLIB,VOL=(MVSRES,3350),SPACE=(CYL,(20,1,323))       05670003
LPALIB   DATASET LPALIB,VOL=(MVSRES,3350),SPACE=(CYL,(20,1,360)),      C05680003
               PDS=SYS2.LOCAL.LPALIB,                                  C05690003
               MEMBERS=(IGC0024{,        SVC 240                       C05700003
               IGC0024A,                 SVC 241                       C05710003
               IGC0024B,                 SVC 242                       C05720003
               IGC0024C,                 SVC 243                       C05730003
               IGC0024D,                 SVC 244                       C05740003
               IGC0024E,                 SVC 245                       C05750003
               IGC0024F,                 SVC 246                       C05760003
               IGC0024G,                 SVC 247                       C05770003
               IGC0024H,                 SVC 248                       C05780003
               IGC0024I,                 SVC 249                       C05790003
               IGC0025{,                 SVC 250                       C05800003
               IGC0025A,                 SVC 251                       C05810003
               IGC0025B,                 SVC 252                       C05820003
               IGC0025C,                 SVC 253                       C05830003
               IGC0025D,                 SVC 254                       C05840003
               IGC0025E)                 SVC 255                        05850003
MACLIB   DATASET MACLIB,VOL=(MVSRES,3350),SPACE=(CYL,(30,1,107))        05860003
MANX     DATASET MANX,VOL=(MVSRES,3350),SPACE=(CYL,(6))                 05870003
MANY     DATASET MANY,VOL=(MVSRES,3350),SPACE=(CYL,(6))                 05880003
NUCLEUS  DATASET NUCLEUS,VOL=(MVSRES,3350),SPACE=(CYL,(10,,20))         05890003
PAGE01   DATASET PAGEDSN=SYS1.PAGELPA,                                 C05900003
               VOL=(PAGE00,3350),SPACE=(CYL,(185))                      05910003
PAGE02   DATASET PAGEDSN=SYS1.PAGECSA,                                 C05920003
               VOL=(PAGE00,3350),SPACE=(CYL,(185))                      05930003
PAGE03   DATASET PAGEDSN=SYS1.PAGEL00,                                 C05940003
               VOL=(PAGE00,3350),SPACE=(CYL,(184))                      05950003
STGINDX  DATASET STGINDEX,VOL=(MVSRES,3350),SPACE=(CYL,(2))             05960003
PARMLIB  DATASET PARMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(8,,40))          05970003
PROCLIB  DATASET PROCLIB,VOL=(MVSRES,3350),SPACE=(CYL,(6,1,71))         05980003
SAMPLIB  DATASET SAMPLIB,VOL=(MVSRES,3350),SPACE=(CYL,(10,1,20))        05990003
SVCLIB   DATASET SVCLIB,VOL=(MVSRES,3350),SPACE=(CYL,(2,1,35))          06000003
TCAMMAC  DATASET TCOMMAC,VOL=(MVSRES,3350),SPACE=(CYL,(3,1,35))         06010003
TELCMLB  DATASET TELCMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(4,1,71))        06020003
UADS     DATASET UADS,VOL=(MVSRES,3350),SPACE=(CYL,(1,1,35))            06030003
VSCATLG  DATASET VSCATLG,NAME=SYS1.VSAM.MASTER.CATALOG                  06040003
VTAMLB   DATASET VTAMLIB,VOL=(MVSRES,3350),SPACE=(CYL,(4,1,35))         06050003
         EJECT                                                          06060003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 06070003
*                          USER SVCS                                  * 06080003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 06090003
*                                                                       06100003
*                  + REQUIRED - SPECIFIES USER SVC AS A DECIMAL NUMBER; 06110003
*                  | MUST BE UNIQUE AND WITHIN THE RANGE 200-255.       06120003
*                  |                                                    06130003
*                  |   + REQUIRED - SPECIFIES SVC TYPE AS 1, 2, 3, 4,   06140003
*                  |   | 5, OR 6.                                       06150003
*                  |   |                                                06160003
*                  |   |  + OPTIONAL - SPECIFIES VALUE(S) TO INDICATE   06170003
*                  |   |  | WHICH LOCKS TO OBTAIN BEFORE ENTRY TO SVC;  06180003
*                  |   |  | L1=LOCAL, L2=CMS, L3=SRM, L4=SALLOC9,       06190003
*                  |   |  | L5=DISPATCHER; INVALID FOR TYPE 6 SVC.      06200003
*                  |   |  |                                             06210003
*                  |   |  |  + OPTIONAL - CODE FC01 RESTRICTS USE OF    06220003
*                  |   |  |  | THE SVC TO AUTHORIZED STEPS; IF NOT      06230003
*                  |   |  |  | SPECIFIED, FC00 IS ASSUMED MAKING SVC    06240003
*                  |   |  |  | UNRESTRICTED.                            06250003
*                  |   |  |  |                                          06260003
*                  |   |  |  |    + OPTIONAL - SPECIFIES THAT SVC RUNS  06270003
*                  |   |  |  |    | NON-PREMPTIBLE FOR I/O INTERRUPTS.  06280003
*                  |   |  |  |    |                                     06290003
*                  V   V  V  V    V                                     06300003
*     SVCTABLE SVC-255-T1-L1-FC00-NP                                    06310003
         SVCTABLE                                                      C06320003
               SVC-255-T3-L1-FC00,     OPEN                            C06330003
               SVC-254-T4-FC00,        OPEN                            C06340003
               SVC-253-T3-FC00,        OPEN                            C06350003
               SVC-252-T4-FC00,        OPEN                            C06360003
               SVC-251-T3-FC00,        OPEN                            C06370003
               SVC-250-T4-FC00,        OPEN                            C06380003
               SVC-249-T3-FC00,        OPEN                            C06390003
               SVC-248-T4-FC00,        FSE                             C06400003
               SVC-247-T3-FC00,        OPEN                            C06410003
               SVC-246-T4-FC00,        OPEN                            C06420003
               SVC-245-T3-FC00,        OPEN                            C06430003
               SVC-244-T4-FC00,        AUTHORIZATION                   C06440003
               SVC-243-T3-FC00,        OPEN                            C06450003
               SVC-242-T4-FC00,        OPEN                            C06460003
               SVC-241-T3-FC00,        OPEN                            C06470003
               SVC-240-T4-FC00         OPEN                             06480003
         EJECT                                                          06490003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 06500003
*                           GENERATE                                  * 06510003
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 06520003
*                                                                       06530003
         GENERATE GENTYPE=ALL,        FULL SYSGEN                      C06540003
               INDEX=SYS1,            HIGH LEVEL QUALIFIER FOR DS      C06550003
               JCLASS=A,              JOB CLASS                        C06560003
               OBJPDS=SYS1.OBJPDS,    OBJECT DATASETS                  C06570003
               OCLASS=A,              OUTPUT CLASS                     C06580003
               RESVOL=(MVSRES,3350)   SYSRES VOLUME                     06590003
         END                                                            06600003
/*                                                                      06610003
//*                                                                     06620003
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE,ASMBLR)                         06630003
//* ***************************************************************** * 06640003
//* IF ASMBLR RC=0000, SUBMIT CONTINUATION JOB TO INTERNAL READER.    * 06650003
//* ***************************************************************** * 06660003
//SYSPRINT DD  DUMMY                                                    06670003
//SYSIN    DD  DUMMY                                                    06680003
//SYSUT1   DD  DATA,DLM='><'                                            06690003
//SYSGEN01 JOB 'PUNCH STAGE 2 DECK',                                    06700003
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)                        06710003
/*JOBPARM  CARDS=15000                                                  06720003
//*                                                                     06730003
//PUNCH   EXEC PGM=IEBGENER                                             06740003
//* ***************************************************************** * 06750003
//* PUNCH STAGE1 OUTPUT DECK TO PCH00D OUTPUT FILE                    * 06760003
//* ***************************************************************** * 06770003
//SYSPRINT DD  DUMMY                                                    06780003
//SYSIN    DD  DUMMY                                                    06790003
//SYSUT1   DD  DISP=SHR,DSN=SYS1.STAGE1.OUTPUT,UNIT=3350,VOL=SER=WORK01 06800003
//SYSUT2   DD  SYSOUT=B,DCB=BLKSIZE=80                                  06810003
//*        END OF SUBMITTED SYSGEN01                                    06820003
><                                                                      06830003
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        06840003
//*         END OF PRIMARY SYSGEN01                                     06850003
