//SYSGEN05 JOB 'IEFACTRT/IEFU29',                                       00010001
//             CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),REGION=4096K           00020001
/*JOBPARM LINES=100                                                     00030001
//JOBCAT   DD  DSN=SYS1.VSAM.MASTER.CATALOG,DISP=SHR                    00040001
//*                                                                     00050001
//********************************************************************* 00060001
//* Install USERMOD JMUM001 - IEFACTRT exit to provide job/step       * 00070001
//* accounting information /and/                                      * 00080001
//* Install Procedures, define Generation Data Group, and install     * 00090001
//* IEFU29 exit to make SMF more easily managed.                      * 00100001
//********************************************************************* 00110001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 00120001
//UPDATE01 EXEC PGM=IEBUPDTE,PARM=NEW                                   00130001
//SYSPRINT DD  SYSOUT=*                                                 00140001
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC                                00150001
//SYSIN    DD  *                                                        00160001
./ ADD NAME=IEFACTRT                                                    00170001
ACTR     TITLE 'IEFACTRT - ACCOUNTING ROUTINE DOCUMENTAION'             00180001
*        MVS SMF ACCOUNTING ROUTINE TO PUT STEP STATISTICS              00190001
*        ON JOB OUTPUT AND THE SYSTEM CONSOLE.  OPTIONALLY,             00200001
*        A MESSAGE MAY ALSO BE SENT TO A TSO USER.                      00210001
*                                                                       00220001
*        REGISTER USAGE:                                                00230001
*                                                                       00240001
*              R0   ON ENTRY CONTAINS REASON FOR ENTRY + USED AS        00250001
*                       WORK REGISTER                                   00260001
*              R1   ON ENTRY POINTS TO SMF PARAMETER LIST + USED AS     00270001
*                       WORK REGISTER                                   00280001
*              R2   WORK REGISTER                                       00290001
*              R3   POINTS TO SMF RECORD RDW (TYPE 4 FOR A BATCH JOB,   00300001
*                       OR TYPE 34 FOR A TSO USER)                      00310001
*              R4   USED AS WORK REGISTER                               00320001
*              R5   WORK REGISTER                                       00330001
*              R6   WORK REGISTER                                       00340001
*              R7   WORK REGISTER                                       00350001
*              R8   RETURN FOR BRANCH AND LINK INSTRUCTIONS +           00360001
*                       USED AS A TEMP BASE IN ESTAE ROUTINES           00370001
*              R9   WORK REGISTER + USED AS POINTER TO RELOCATE         00380001
*                       SECTION OF SMF RECORD                           00390001
*              R10  BASE REGISTER                                       00400001
*              R11  POINTS TO SMF PARAMETER LIST                        00410001
*              R12  ADDRESS OF LINKAGE CONTROL TABLE (USED TO PASS      00420001
*                       PARAMETERS TO 'IEFYS' ROUTINE)                  00430001
*              R13  SAVE AREA AND BASE FOR WORKAREA DSECT               00440001
*              R14  RESERVED FOR LINKAGE + USED AS WORK REGISTER        00450001
*              R15  RESERVED FOR LINKAGE + USED AS WORK REGISTER        00460001
*                                                                       00470001
*        STEP ACCOUNTING DATA WHICH IS PRINTED IS DESCRIBED BELOW:      00480001
*                                                                       00490001
*              STEP NAME                                                00500001
*              PROGRAM NAME (FROM JCL 'EXEC' CARD)                      00510001
*              STEP START TIME                                          00520001
*              STEP STOP TIME                                           00530001
*              ELAPSED TIME                                             00540001
*              ALLOCATION START TIME                                    00550001
*              PROGRAM LOAD TIME                                        00560001
*              STEP CPU TIME UNDER TCB'S                                00570001
*              STEP CPU TIME UNDER SRB'S                                00580001
*              STORAGE USED FROM BOTTOM OF PRIVATE AREA                 00590001
*              STORAGE USED FROM TOP OF PRIVATE AREA                    00600001
*              SIZE OF PRIVATE AREA                                     00610001
*              STEP COMPLETION STATUS (& CODE, IF APPLICABLE)           00620001
*              NUMBER OF DISK UNITS USED                                00630001
*              NUMBER OF DISK EXCP'S                                    00640001
*              NUMBER OF TAPE UNITS USED                                00650001
*              NUMBER OF TAPE EXCP'S                                    00660001
*              NUMBER OF VIO EXCP'S (IF ANY)                            00670001
*                                                                       00680001
*              PERFORMANCE GROUP NUMBER                                 00690001
*              # OF SERVICE UNITS USED                                  00700001
*              STEP ACTIVE TIME                                         00710001
*              # PAGES PAGED IN                                         00720001
*              # PAGES PAGED OUT                                        00730001
*              # TIMES SWAPPED                                          00740001
*              # PAGES SWAPPED IN                                       00750001
*              # PAGES SWAPPED OUT                                      00760001
*              # VIO PAGES PAGED IN                                     00770001
*              # VIO PAGES PAGED OUT                                    00780001
*                                                                       00790001
*        STEP ACCOUNTING DATA WHICH IS WRITTEN ON THE SYSTEM            00800001
*        CONSOLE IN THE FORMAT DESCRIBED BELOW                          00810001
*                                                                       00820001
*    STEPNAME/PGMNAMEX/CP:UT:IM.EX/WA:LL:TI/CCODE/ACCTINFO/JOBNAMEX     00830001
*                                                                       00840001
*        WHERE:                                                         00850001
*                                                                       00860001
*              STEPNAME     IS STEPNAME                                 00870001
*              PGMNAMEX     IS PROGRAM NAME (FROM JCL 'EXEC' CARD)      00880001
*              CP:UT:IM.EX  IS STEP CPU TIME (UNDER TCB'S) IN           00890001
*                               HUNDREDTHS OF SECONDS                   00900001
*              WA:LL:TI     IS ELAPSED WALL TIME FOR STEP EXECUTION     00910001
*              CCODE        IS STEP EXECUTION STATUS/CODE, IN ONE       00920001
*                               OF THE FOLLOWING FORMATS:               00930001
*                  NNNNN    CONDITION CODE IF STEP WAS EXECUTED         00940001
*                               AND TERMINATED NORMALLY                 00950001
*                  UNNNN    USER ABEND CODE IN DECIMAL                  00960001
*                  S-HHH    SYSTEM ABEND CODE IN HEXADECIMAL            00970001
*                  NOXEC    IF STEP WAS FLUSHED (I.E., NOT              00980001
*                               EXECUTED AT ALL                         00990001
*              ACCTINFO     ACCOUNTING INFO (FROM JOBCARD)              01000001
*              JOBNAMEX     JOBNAME                                     01010001
*                                                                       01020001
*        OPTIONAL TSO TERMINAL USER NOTIFICATION OF STEP                01030001
*        TERMINATION IS REQUESTED BY CODING AN ACCOUNTING               01040001
*        PARAMETER IN THE FOLLOWING FORMAT ON THE JCL EXEC              01050001
*        STATEMENT OF THE STEP FOR WHICH NOTIFICATION IS DESIRED:       01060001
*                                                                       01070001
*    NOTIFY=USERID,OPTIONALUSERDATA                                     01080001
*                                                                       01090001
*        WHERE:                                                         01100001
*                                                                       01110001
*              USERID       IS LOGON ID OF USER TO BE NOTIFIED          01120001
*              OPTIONALUSERDATA  IS UP TO 32 BYTES OF USER              01130001
*                               DATA WHICH MAY OPTIONALLY BE            01140001
*                               SPECIFIED TO BE ADDED TO THE            01150001
*                               NOTIFICATION MESSAGE                    01160001
*                                                                       01170001
*        FORMAT OF MESSAGE SENT TO TSO USER IS AS FOLLOWS:              01180001
*                                                                       01190001
*    JOBNAMEX (JES2JOB#)/STEPNAME/CCODE OPTIONALUSERDATA                01200001
*                                                                       01210001
*        WHERE THE MEANING OF THE PARAMETERS IS AS PER ABOVE.           01220001
*                                                                       01230001
*********************************************************************** 01240001
         SPACE 3                                                        01250001
         LCLA  &SP                 SUBPOOL FOR GETMAIN FOR WORKAREA     01260001
&SP      SETA  241                                                      01270001
         TITLE 'IEFACTRT - SMF JOB/STEP TERMINATION ACCOUNTING'         01280001
         PRINT NOGEN                                                    01290001
LCT      DSECT                                                          01300001
         IEFALLCT                                                       01310001
         SPACE 3                                                        01320001
SMFR     DSECT                                                          01330001
         IFASMFR (4,34)       I USE THE SMF TYPE 4 RECORD DEFINITION   X01340001
                              TO MAP BOTH TYPE 4 AND TYPE 34 RECORDS   X01350001
                              --- WATCH OUT FOR DIFFERENCES             01360001
         SPACE 3                                                        01370001
         PRINT GEN                                                      01380001
R0       EQU   0                                                        01390001
R1       EQU   1                                                        01400001
R2       EQU   2                                                        01410001
R3       EQU   3                                                        01420001
R4       EQU   4                                                        01430001
R5       EQU   5                                                        01440001
R6       EQU   6                                                        01450001
R7       EQU   7                                                        01460001
R8       EQU   8                                                        01470001
R9       EQU   9                                                        01480001
R10      EQU   10                                                       01490001
R11      EQU   11                                                       01500001
R12      EQU   12                                                       01510001
R13      EQU   13                                                       01520001
R14      EQU   14                                                       01530001
R15      EQU   15                                                       01540001
LENFACT  EQU   106                                                      01550001
SPACE    EQU   X'40'                                                    01560001
ASTERISK EQU   C'*'                                                     01570001
COMMA    EQU   C','                                                     01580001
RPARENS  EQU   C')'                                                     01590001
LPARENS  EQU   C'('                                                     01600001
SLASH    EQU   C'/'                                                     01610001
DADEV    EQU   X'20'          UCBTYP MASK FOR DIRECT ACCESS            X01620001
                              STORAGE CLASS DEVICES                     01630001
TADEV    EQU   X'80'               UCBTYP MASK FOR MAG TAPE DEVICES     01640001
CVTPTR   EQU   X'4C'               PSA POINTER TO CVT                   01650001
         SPACE 3                                                        01660001
         PRINT NOGEN                                                    01670001
         IEZJSCB                                                        01680001
         IHASDWA                                                        01690001
         SPACE 3                                                        01700001
         PRINT GEN                                                      01710001
IEFACTRT CSECT                                                          01720001
         USING LCT,R12                                                  01730001
         USING IEFACTRT,R15                                             01740001
         B     SAVEREGS                                                 01750001
         DC    AL1(SAVEREGS-*)                      LENGTH              01760001
         DC    C' IEFACTRT &SYSDATE &SYSTIME '      ** EYE CATCHER **   01770001
SAVEREGS STM   R14,R12,12(R13)                                          01780001
         DROP  R15                                                      01790001
         LR    R10,R15             LOAD BASE REGISTER                   01800001
         USING IEFACTRT,R10        USE R10 AS BASE                      01810001
         LR    R11,R1              SAVE PARAMETER LIST                  01820001
         LR    R5,R0               SAVE ENTRY TYPE                      01830001
         GETMAIN R,LV=AREASIZE,SP=&SP                                   01840001
         USING WORKAREA,R1                                              01850001
         ST    R10,SAVE10     SAVE BASE REGISTER                        01860001
         ST    R13,SAVE13     SAVE CALLER'S R13 (I AM NOT FOLLOWING    X01870001
                              STANDARD LINKAGE CONVENTIONS HERE ---    X01880001
                              THE REASON IS THAT I ALSO USE MY          01890001
*                                  SAVEAREA AS THE 45 WORD WORKAREA     01900001
*                                  REQUIRED BY 'IEFYS', AND I DON'T     01910001
*                                  KNOW WHAT HE DOES WITH IT)           01920001
         LR    R13,R1                                                   01930001
         DROP  R1                                                       01940001
         USING WORKAREA,R13                                             01950001
* PROTECT YOURSELF WITH AN ESTAE                                        01960001
         ST    R10,STAER10         REGISTER REQUIRED BY RETRY ROUTINE   01970001
         ST    R13,STAER13         REGISTER REQUIRED BY RETRY ROUTINE   01980001
         MVC   STAELIST(STAELN),STAELFRM                                01990001
         LA    R2,ERREXIT          RETRY ROUTINE ENTRY ADDRESS          02000001
         LA    R3,STAEPRMS                                              02010001
         ESTAE (2),PARAM=(3),MF=(E,STAELIST)                            02020001
         LTR   R15,R15                                                  02030001
         BNZ   STAEFAIL       IF ESTAE WAS NOT SUCCESSFUL,             X02040001
                              ISSUE ERROR MESSAGE & RETURN TO CALLER    02050001
         L     R3,36(R11)          GET ADDRESS OF SMF RECORD            02060001
         USING SMFRCD4,R3                                               02070001
         SPACE 1                                                        02080001
         CH    R5,=H'12'           ENTRY FOR STEP TERMINATION ??        02090001
         BE    STEPTERM            YES                                  02100001
         L     R1,0(R11)           R1-->COMMON EXIT PARM AREA           02110001
         CLI   SMF4RTY,5           TYPE 5 RECORD ???  WAW 10/86         02120001
         BNE   EXIT                NO - THEN DON'T HOSE IT              02130001
*                                  MOVE TAPE & DISK COUNTS TO           02140001
         MVC   115(2,R3),32(R1)    LAST 2 BYTES OF TYPE 5               02150001
*                                  PROGRAMMER NAME FIELD                02160001
         B     EXIT                END TYPE 5 PROCESSING                02170001
         SPACE 3                                                        02180001
*                                                                       02190001
*        STEP TERMINATION PROCESSING                                    02200001
*                                                                       02210001
         SPACE 1                                                        02220001
STEPTERM MVC   WTOAREA(WTO001CL),WTO001C                                02230001
         SPACE 1                                                        02240001
         MVI   MSGAREA,ASTERISK                                         02250001
         MVC   MSGAREA+1(L'MSGAREA-1),MSGAREA                           02260001
         L     R1,28(R11)                                               02270001
         CLI   1(R1),1             FIRST STEP OF JOB?                   02280001
         BNE   PRNTLIN1                                                 02290001
* FORMAT TIME JOB ENTERED SYSTEM (I.E., WHEN JOBCARD WAS READ, OR       02300001
* TSO LOGON WAS RECOGNIZED)                                             02310001
         CLI   SMF4RTY,34          TSO LOGON?                           02320001
         BE    TSOLOGON                                                 02330001
* REMOVED 5/22/86  DW                                                   02340001
*        L     R1,0(R11)           ADDR OF COMMON PARM AREA             02350001
*        LA    R1,20(R1)           POINT TO ACCT & SUB ACCT             02360001
*        LINK  EP=TMSACTRT                                              02370001
         MVC   MSGJCRL,JCRLJOB                                          02380001
         B     GETJCRT                                                  02390001
TSOLOGON MVC   MSGJCRL,JCRLTSO                                          02400001
GETJCRT  MVC   MSGJCRDT,DTFMT                                           02410001
         LA    R4,MSGJCRT                                               02420001
         ICM   R7,15,SMF4RST                                            02430001
         BAL   R8,TIME             FORMAT TIME JOB ENTERED SYSTEM       02440001
         UNPK  MSGJCRD,SMF4RSD     FORMAT DATE JOB ENTERED SYSTEM       02450001
PRNTLIN1 BAL   R14,PRINT           PRINT TOP BORDER OF ASTERISKS        02460001
         SPACE 1                                                        02470001
         MVI   MSGAREA+1,SPACE                                          02480001
         MVC   MSGAREA+2(L'MSGAREA-3),MSGAREA+1                         02490001
         MVC   MSGHDR,HDRMSG                                            02500001
         L     R1,CVTPTR      POINT TO CVT TO GET CPU MODEL NUMBER     X02510001
                              AND OPERATING SYSTEM RELEASE LEVEL        02520001
         LA    R1,0(R1)       CLEAR HIGH-ORDER BYTE                     02530001
         SH    R1,=H'6'                                                 02540001
         UNPK  HDRMODNR(4),0(3,R1) CPU MODEL #                          02550001
         TR    HDRMODNR,HEXTRANS-X'F0'                                  02560001
         MVI   HDRMODNR+L'HDRMODNR,SPACE                                02570001
         MVC   HDRNUMB,2(R1)       OPERATING SYSTEM RELEASE #           02580001
         MVC   HDRSUBNM,4(R1)      SUB-RELEASE #                        02590001
         MVC   HDRSID,SMF4SID      GET SMF SYSTEM ID CODE               02600001
         BAL   R14,PRINT                                                02610001
         SPACE 1                                                        02620001
         MVI   MSGAREA+1,SPACE     CLEAR ALL BUT 1ST AND LAST *         02630001
         MVC   MSGAREA+2(L'MSGAREA-3),MSGAREA+1                         02640001
         MVC   MSGAREA+1(L'STEPMSG1),STEPMSG1                           02650001
         MVC   MSGAREA+1+25(L'MSGCORE1),MSGCORE1                        02660001
         MVC   MSGAREA+1+25+26(L'SM1B),SM1B                             02670001
         MVI   MSGAREA+1+25+26+L'SM1B+4,C'/'                            02680001
         MVC   MSGAREA+1+25+26+30(L'SM1C),SM1C                          02690001
         MVC   MSGAREA+1+25+26+30+26(L'SM1D),SM1D                       02700001
         MVC   MSGSTPN,SMF4STMN                                         02710001
         CLI   SMF4STMN,SPACE      ANY STEP NAME SUPPLIED?              02720001
         BNE   GETSTPN                                                  02730001
         SR    R0,R0                                                    02740001
         IC    R0,SMF4STN          STEP # OF THIS STEP                  02750001
         CVD   R0,DBLWORD                                               02760001
         UNPK  STP#001C,DBLWORD+6(2)                                    02770001
         OI    STP#001C+L'STP#001C-1,X'F0'                              02780001
         B     GETH0ST                                                  02790001
GETSTPN  MVC   STPN001C,SMF4STMN                                        02800001
GETH0ST  LH    R1,SMF4H0ST         PROBLEM PROGRAM CORE USED            02810001
         ST    R1,MSVMEM           SAVE FOR DOLLARS LINE CALULATION     02820001
         CVD   R1,DBLWORD                                               02830001
         ED    MSGUCORE-1(6),DBLWORD+5                                  02840001
         LA    R4,MSGSTIME                                              02850001
         ICM   R7,15,SMF4SIT       STEP INITIATION TIME                 02860001
         BAL   R8,TIME             FORMAT STEP INITIATION TIME          02870001
* CALCULATE # OF TAPE DEVICES & DISK DEVICES USED BY STEP, & # OF       02880001
* TAPE EXCPS & DISK EXCPS EXECUTED IN STEP.                             02890001
         SR    R4,R4               # DISK DEVICES USED                  02900001
         LR    R5,R4               # TAPE DEVICES USED                  02910001
         LR    R7,R5               # DISK EXCPS ISSUED                  02920001
         LR    R8,R7               # TAPE EXCPS ISSUED                  02930001
         LR    R9,R8               # VIO EXCPS ISSUED                   02940001
         LH    R2,SMF4LENN                                              02950001
         AH    R2,=H'-2'                                                02960001
         BNP   FORMATIO                                                 02970001
* GET WORKAREA FOR A TABLE IN WHICH TO STORE DEVICE ADDRESSES.          02980001
* AS EACH SMF DEVICE ENTRY IS READ, A CHECK IS MADE TO SEE IF IT        02990001
* A DASD OR TAPE DEVICE.  IF NOT, THE NEXT ENTRY IS READ.  IF           03000001
* IT IS, THE 'SMF4EXCP' FIELD IS ADDED TO THE DASD OR TAPE EXCP         03010001
* COUNTER, AS APPROPRIATE.  THEN A TABLE OF TWO BYTE DEVICE             03020001
* ADDRESSES IS SCANNED.  IF THE DEVICE ADDRESS HAS ALREADY BEEN         03030001
* ENCOUNTERED, A MATCHING ENTRY WILL BE FOUND IN THE TABLE.  IF         03040001
* END OF TABLE IS REACHED (A ZERO ENTRY), THEN WE HAVE ENCOUNTERED      03050001
* A NEW DEVICE ADDRESS --- THE ADDRESS IS INSERTED AT THE END OF        03060001
* THE TABLE, A NEW END-OF-TABLE ENTRY IS ESTABLISHED, AND 1 IS          03070001
* ADDED TO THE APPROPRIATE DEVICES-USED COUNTER.                        03080001
         MVC   VCORE,VCOREFMT      LIST FORM MACRO FOR CONDITIONAL     X03090001
                                   GETMAIN FOR DEVICE ADDRESS TABLE     03100001
         SRL   R2,2                                                     03110001
         LR    R6,R2                                                    03120001
         SRL   R6,1                R6 HAS # OF DEVICE ENTRIES           03130001
         LA    R2,7+2(R2)     ADD 2 BYTES FOR DUMMY TABLE ENTRY, AND   X03140001
                              ROUND UP TO DOUBLEWORD MULTIPLE           03150001
         N     R2,=X'00FFFFF8'                                          03160001
         GETMAIN LV=(2),A=AREAADDR,MF=(E,VCORE)                         03170001
         LTR   R1,R15                                                   03180001
         BNZ   IOFAILED            (R1 CONTAINS ZERO ON FALL-THRU)      03190001
         L     R2,AREAADDR         R2 POINTS TO DEVICE ADDRESS TABLE    03200001
         STH   R1,0(R2)       INDICATE INITIAL STATUS OF TABLE IS      X03210001
                              EMPTY                                     03220001
         LA    R15,SMF4LENN+2                                           03230001
         USING SMF4DEVC,R15                                             03240001
IOLOOP   CLI   SMF4DEVC,DADEV      DIRECT ACCESS DEVICE?                03250001
         BE    DEVDA                                                    03260001
         CLI   SMF4DEVC,TADEV      TAPE DEVICE?                         03270001
         BE    DEVTA                                                    03280001
         CLC   SMF4DEVC(4),=X'00000FFF'                                 03290001
         BNE   NEXTIO                                                   03300001
         A     R9,SMF4EXCP         ACCUMULATE VIO EXCP'S                03310001
NEXTIO   LA    R15,8(R15)                                               03320001
         BCT   R6,IOLOOP                                                03330001
         B     FREEVCOR                                                 03340001
DEVDA    A     R7,SMF4EXCP         ACCUMULATE DASD EXCP'S               03350001
         LR    R14,R2              PINT TO START OF DEVICE ADDR TABLE   03360001
FINDDA   CH    R1,0(R14)           NO MORE ENTRIES TO SCAN?             03370001
         BE    NEWDA                                                    03380001
         CLC   0(2,R14),SMF4CUAD   DOES ADDRESS IN CURRENT ENTRY       X03390001
                              IN TABLE MATCH THAT OF CURRENT           X03400001
                              ENTRY IN SMF RECORD?                      03410001
         BE    NEXTIO                                                   03420001
         LA    R14,2(R14)          BUMP TO NEXT ENTRY IN TABLE          03430001
         B     FINDDA                                                   03440001
NEWDA    MVC   0(2,R14),SMF4CUAD   ENTER NEW DEVICE ADDRESS IN TABLE    03450001
         STH   R1,2(R14)           SET NEW END-OF-TABLE INDICATOR       03460001
         LA    R4,1(R4)            INCREMENT #-OF-DASD-DEV-USED COUNTER 03470001
         B     NEXTIO                                                   03480001
DEVTA    A     R8,SMF4EXCP         ACCUMULATE TAPE EXCP'S               03490001
         LR    R14,R2                                                   03500001
FINDTA   CH    R1,0(R14)                                                03510001
         BE    NEWTA                                                    03520001
         CLC   0(2,R14),SMF4CUAD                                        03530001
         BE    NEXTIO                                                   03540001
         LA    R14,2(R14)                                               03550001
         B     FINDTA                                                   03560001
NEWTA    MVC   0(2,R14),SMF4CUAD   ENTER NEW DEVICE ADDRESS IN TABLE    03570001
         STH   R1,2(R14)           SET NEW END-OF-TABLE INDICATOR       03580001
         LA    R5,1(R5)            INCREMENT #-OF-TAPE-DEV-USED COUNTER 03590001
         B     NEXTIO                                                   03600001
         DROP  R15                                                      03610001
FREEVCOR FREEMAIN MF=(E,VCORE)                                          03620001
FORMATIO TM    SMF4RIN,B'00000010'      IS I/O-COUNTS-POSSIBLY-INVALID X03630001
                                        INDICATOR ON?                   03640001
         BZ    BZ0010                                                   03650001
         MVI   MSGTEXCP+L'MSGTEXCP,ASTERISK                             03660001
BZ0010   LR    R0,R7                    TOTAL DISK EXCPS                03670001
         AR    R0,R8                    + TOTAL TAPE EXCPS              03680001
         AR    R0,R9                    + TOTAL VIO EXCPS               03690001
         ST    R0,MSVEXCP               SAVE INTERMEDIATE TOTAL         03700001
         CVD   R5,DBLWORD                                               03710001
         UNPK  MSG#TAPE,DBLWORD+6(2)                                    03720001
         OI    MSG#TAPE+L'MSG#TAPE-1,X'F0'                              03730001
         CVD   R8,DBLWORD                                               03740001
         UNPK  MSGTEXCP,DBLWORD+3(5)                                    03750001
         OI    MSGTEXCP+L'MSGTEXCP-1,X'F0'                              03760001
         CVD   R4,#DISKS           SAVE COUNT OF DASD DEVICES USED      03770001
         CVD   R7,DIO              SAVE COUNT OF DASD EXCP'S            03780001
         CVD   R9,VIO              SAVE COUNT OF VIO EXCP'S             03790001
         MVI   IOSW,0              INDICATE DASD USE INFO AVAILABLE     03800001
         B     IODONE                                                   03810001
IOFAILED MVI   IOSW,12             INDICATE DASD USE INFO NOT AVAILABLE 03820001
         SPACE 3                                                        03830001
IODONE   L     R2,0(R11)           PT TO EXIT PARM LIST                 03840001
         CLI   28(R2),1            IS THIS 1ST STEP OF JOB              03850001
         BNE   NOT1ST                                                   03860001
         XC    32(4,R2),32(R2)     ZERO USER COMM AREA  T&D COUNTS      03870001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 03880001
NOT1ST   LH    R6,LENFACT(R3)      LENGTH FACTOR FOR DEVICES            03890001
         LA    R6,LENFACT(R6,R3)   SET LOOP END                         03900001
         LA    R8,LENFACT+2(R3)    START OF DEVICE INFO                 03910001
         SR    R0,R0               INIT SUM UP REG                      03920001
         XC    FULLWRDS+4(2),FULLWRDS+4  0 TO STEP TAPE&DISK COUNTER    03930001
         L     R1,16               --> CVT                              03940001
         L     R4,36(R1)           CVTILK1                              03950001
         L     R7,40(R1)           CVTILK2                              03960001
TOPLOP   CR    R8,R6               ANY MORE DEVICES?                    03970001
         BNL   ENDLOP              SUM IS COMPLETE, GO SAVE             03980001
         CLI   0(R8),X'20'         THIS A DISK UCB TYP?                 03990001
         BNE   XUCBTAPE            IF NO, GO CHECK FOR TAPE             04000001
         LH    R14,2(R8)           CHAN ADDR/CNTRL ADDR/DEV ADDR        04010001
         LR    R1,R14              SAVE FOR AWHILE                      04020001
         SRDL  R14,8               ISOLATE CHAN ADDR                    04030001
         SRL   R15,28              ISOLATE CNTRL ADDR                   04040001
         IC    R14,0(R14,R4)       IECILK1+CHAN ADDR->ADDR OF K         04050001
         AR    R14,R15             CNTRL ADDR+K                         04060001
*        CLI   7(R4),X'FF'         > 248 DEVICES IN SYSTEM              04070001
*        BNE   LESS249             ONLY SINGLE BYTE CU TABLE IF < 249   04080001
         AR    R14,R14             2(CNTRL ADDR+ K)                     04090001
         LH    R14,0(R14,R4)       IECILK1+2(CNTRL ADDR+K)->ADDR L      04100001
*        B     GRTR248                                                  04110001
*ESS249  IC    R14,0(R14,R4)       IECILK1+CNTRL ADDR+K->ADDR L         04120001
GRTR248  N     R1,X0F              ISOLATE DEV ADDR                     04130001
         AR    R1,R14              DEV ADDR + L                         04140001
         AR    R1,R1               2(DEV ADDR +L)                       04150001
         LH    R15,0(R1,R7)        IECIKL2+2(DEVADDR+L)->UCB            04160001
         TM    3(R15),X'04'        IS UCB A PERM RES PACK               04170001
         BZ    XUCBCNT             NO IF BIT OFF, GO CHK MAX USE        04180001
         OI    1(R8),X'40'         SET SYSTEM VOL BIT IN TYP 4 REC      04190001
ADDEMUP  A     R0,4(R8)            ADD TAPE AND DISK EXCPS              04200001
MIDLOP   LA    R8,8(R8)            INCR TO NXT DEV ENTRY                04210001
         B     TOPLOP              GO TEST FOR MORE                     04220001
XUCBTAPE CLI   0(R8),X'80'         IS THIS A TAPE UCB TYPE?             04230001
         BNE   MIDLOP              IF NO, ALL DONE WITH IT.             04240001
XUCBCNT  LA    R15,LENFACT+2(R3)   GET HERE 4 ONLY TAPE&NON PER RES     04250001
XUCBSEEK CR    R15,R8              THIS POINT = CURRENT POS IN LIST     04260001
         BNL   XUCBKEEP            IF = THEN CUR UCB IS UNIQUE, GO SUM  04270001
         CLC   2(2,R15),2(R8)      THIS UCB= TO CURRENT UCB             04280001
         BE    ADDEMUP             IF= THEN CUR UCB NOT UNIQUE, SKIP    04290001
         LA    R15,8(R15)                                               04300001
         B     XUCBSEEK            KEEP CHECKING FOR UNIQUE UCB         04310001
XUCBKEEP LA    R15,1               DISK COUNTER  X'0001'                04320001
         CLI   0(R8),X'80'         IS THIS A TAPE?                      04330001
         BNE   XUCBDISK            DISK IF BRANCH                       04340001
         LA    R15,256             TAPE COUNTER  X'0100'                04350001
XUCBDISK AH    R15,FULLWRDS+4      ADD 'EM UP                           04360001
         STH   R15,FULLWRDS+4      SAVE TILL ALL UCB'S DONE             04370001
         B     ADDEMUP             CONTINUE TESTING                     04380001
ENDLOP   ST    R0,FULLWRDS         SAVE STEP EXCP COUNT FOR CVD         04390001
         CLC   FULLWRDS+4(1),32(R2)  WAS A NEW MAX TAPE USAGE REACHED   04400001
         BNH   NONUMAXT            NO IF BRANCH                         04410001
         MVC   32(1,R2),FULLWRDS+4 USE NEW MAX TAPE VALUE               04420001
NONUMAXT CLC   FULLWRDS+5(1),33(R2)  WAS A NEW MAX DISK USAGE REACHED   04430001
         BNH   NONUMAXD            NO IF BRANCH                         04440001
         MVC   33(1,R2),FULLWRDS+5 USE NEW MAX DISK VALUE               04450001
NONUMAXD CLI   SMF4RTY,34          TSO STEP?                            04460001
         BE    GETSCPU             YES - BYPASS NEXT                    04470001
         L     R6,16               -> CVT                               04480001
         L     R6,296(,R6)         -> IEFJESCT                          04490001
         L     R6,24(,R6)          -> JESSCT                            04500001
         L     R6,16(,R6)          -> SSCTSSVT                          04510001
         L     R6,884(,R6)         -> SVJXCLS SJBS EXEC JOB-BY-CLS      04520001
JOBLOOP  CLC   0(8,R2),236(R6)     JOBNAME MATCH? (SJBJOBNM)            04530001
         BE    JOBMATCH            YES - RECHECK                        04540001
         ICM   R6,15,200(R6)       CHAIN END? (SJBXQCHN)                04550001
         BNZ   JOBLOOP             NO - CONTINUE TEST                   04560001
         B     GETSCPU             YES - IGNORE                         04570001
JOBMATCH L     R6,96(,R6)          -> SJBJCT                            04580001
         CLC   0(8,R2),156(R6)     JOBNAME MATCH?  (JCTJNAME)           04590001
         BNE   GETSCPU             NO - IGNORE                          04600001
         MVC   182(2,R6),32(R2)    SET HIGH WATER  (JCTPNAME+18)        04610001
GETSCPU  MVI   FULLWORD,0                                               04620001
         L     R5,20(R11)          -> STEP CPU TIME UNDER TCB'S         04630001
* CHECK TO SEE THAT BOTH SOURCES OF STEP CPU TIME UNDER TCB'S           04640001
* CONTAIN THE SAME VALUE                                                04650001
         LH    R1,SMF4LENN                                              04660001
         LA    R1,SMF4LENN(R1)                                          04670001
         CLC   1(3,R1),0(R5)                                            04680001
         BE    BE0010                                                   04690001
         MVI   MSGTCBTM-1,ASTERISK INDICATE VALUES DISAGREE             04700001
BE0010   MVC   FULLWORD+1(3),0(R5)                                      04710001
         L     R7,FULLWORD                                              04720001
         ST    R7,MSVCPU           SAVE FOR DOLLAR CALCULATIONS         04730001
         LA    R4,MSGTCBTM                                              04740001
         BAL   R8,TIMETH           FORMAT STEP CPU TIME UNDER TCB'S     04750001
         MVC   CPUT001C,MSGTCBTM                                        04760001
         BAL   R14,PRINT           PRINT MSG                            04770001
         SPACE 1                                                        04780001
         MVI   MSGAREA+1,SPACE     CLEAR ALL BUT 1ST AND LAST *         04790001
         MVC   MSGAREA+2(L'MSGAREA-3),MSGAREA+1                         04800001
         MVC   MSGAREA+1(L'STEPMSG2),STEPMSG2                           04810001
         MVC   MSGAREA+1+25(L'MSGCORE2),MSGCORE2                        04820001
         MVC   MSGAREA+1+25+26(L'SM2B),SM2B                             04830001
         MVI   MSGAREA+1+25+26+L'SM2B+4,C'/'                            04840001
         MVC   MSGAREA+1+25+26+30(L'SM2C),SM2C                          04850001
         MVC   MSGAREA+1+25+26+30+26(L'SM2D),SM2D                       04860001
         MVC   MSGPGMN,SMF4PGMN                                         04870001
         MVC   PGMN001C,SMF4PGMN                                        04880001
         LH    R1,SMF4SYST         SYSTEM CORE USED ON USER'S BEHALF    04890001
         CVD   R1,DBLWORD                                               04900001
         ED    MSGSCORE-1(6),DBLWORD+5                                  04910001
         CLI   IOSW,0                                                   04920001
         BNE   GETPTIME                                                 04930001
* FORMAT INFORMATION ABOUT DASD DEVICE UTILIZATION                      04940001
         TM    SMF4RIN,B'00000010'                                      04950001
         BZ    BZ0020                                                   04960001
         MVI   MSGDEXCP+L'MSGDEXCP,ASTERISK                             04970001
BZ0020   UNPK  MSG#DISK,#DISKS+6(2)                                     04980001
         OI    MSG#DISK+L'MSG#DISK-1,X'F0'                              04990001
         UNPK  MSGDEXCP,DIO+3(5)                                        05000001
         OI    MSGDEXCP+L'MSGDEXCP-1,X'F0'                              05010001
GETPTIME LA    R4,MSGPTIME                                              05020001
         ICM   R7,15,SMF4TME                                            05030001
         BAL   R8,TIME             FORMAT STEP TERMINATION TIME         05040001
         MVI   FULLWORD,0                                               05050001
         MVC   FULLWORD+1(3),SMF4SRBT                                   05060001
         L     R7,FULLWORD                                              05070001
         LA    R4,MSGSRBTM                                              05080001
         BAL   R8,TIMETH           FORMAT STEP CPU TIME UNDER SRB'S     05090001
         BAL   R14,PRINT                                                05100001
         SPACE 1                                                        05110001
         MVI   MSGAREA+1,SPACE     CLEAR ALL BUT 1ST AND LAST *         05120001
         MVC   MSGAREA+2(L'MSGAREA-3),MSGAREA+1                         05130001
         MVC   MSGAREA+1(L'STEPMSG3),STEPMSG3                           05140001
         MVC   MSGAREA+1+25(L'MSGCORE3),MSGCORE3                        05150001
         MVC   MSGAREA+1+25+28(L'SM3B),SM3B                             05160001
         MVC   MSGAREA+1+25+28+28(L'SM3C),SM3C                          05170001
         MVC   MSGAREA+1+25+28+28+26(L'SM3D),SM3D                       05180001
* DETERMINE KIND OF STEP TERMINATION, & FORMAT TERMINATION CODE.        05190001
         TM    SMF4STI,B'00000001' STEP FLUSHED?                        05200001
         BO    FLUSHED                                                  05210001
         TM    SMF4STI,B'00000010' ABEND?                               05220001
         BO    ABEND                                                    05230001
         MVC   MSGTTYPE,=C'COND'                                        05240001
         MVI   TERM001C,C'0'                                            05250001
         B     DSPLUCOD                                                 05260001
FLUSHED  MVC   MSGNOEXC,=C'-STEP NOT EXECUTED-'                         05270001
         B     NEXTFLD                                                  05280001
ABEND    MVC   MSGTTYPE,=C'COMP'                                        05290001
         TM    SMF4SCC,X'80'       USER ABEND CODE?                     05300001
         BO    USERABE                                                  05310001
* SYSTEM ABEND CODE                                                     05320001
         MVI   TERM001C,C'S'                                            05330001
         MVI   MSGATYPE,C'S'                                            05340001
         UNPK  DBLWORD(4),SMF4SCC(3)                                    05350001
         TR    DBLWORD(3),HEXTRANS-X'F0'                                05360001
         MVC   MSGCODE3,DBLWORD                                         05370001
         MVI   CODE001C,C'-'                                            05380001
         MVC   CODE001C+1(3),DBLWORD                                    05390001
         B     NEXTFLD                                                  05400001
USERABE  MVI   MSGATYPE,C'U'                                            05410001
         MVI   TERM001C,C'U'                                            05420001
DSPLUCOD ICM   R1,3,SMF4SCC                                             05430001
         N     R1,=X'00000FFF'                                          05440001
         CVD   R1,DBLWORD                                               05450001
         UNPK  MSGCODE4,DBLWORD+5(3)                                    05460001
         OI    MSGCODE4+L'MSGCODE4-1,X'F0'                              05470001
         MVC   CODE001C,MSGCODE4                                        05480001
NEXTFLD  LH    R1,SMF4RSH0    PRIVATE AREA SIZE (CORE AVAILABLE TO     X05490001
                              USER)                                     05500001
         CVD   R1,DBLWORD                                               05510001
         ED    MSGARESZ-1(6),DBLWORD+5                                  05520001
         TM    SMF4RIN,B'00000001' V=R JOBSTEP?                         05530001
         BZ    BZ0030                                                   05540001
         MVC   MSGVRIND,=C'V=R'                                         05550001
* GET JOBCARD ACCOUNTING INFO FOR STEP STATISTICS WTO                   05560001
BZ0030   L     R15,12(R11)                                              05570001
         CLI   3(R15),1                                                 05580001
         BL    GETJBN                                                   05590001
         L     R1,16(R11)                                               05600001
         BE    ONEACCTF                                                 05610001
         SR    R7,R7                                                    05620001
         IC    R7,0(R1)                                                 05630001
         AH    R7,=H'-1'                                                05640001
         BM    ACNTFLD2                                                 05650001
         CH    R7,=H'3'                                                 05660001
         BNH   BNH010                                                   05670001
         LA    R7,3                                                     05680001
BNH010   EX    R7,MOVACCT1                                              05690001
ACNTFLD2 SR    R7,R7                                                    05700001
         IC    R7,0(R1)                                                 05710001
         LA    R1,1(R1,R7)                                              05720001
         IC    R7,0(R1)                                                 05730001
         AH    R7,=H'-1'                                                05740001
         BM    GETJBN                                                   05750001
         CH    R7,=H'3'                                                 05760001
         BNH   BNH020                                                   05770001
         LA    R7,3                                                     05780001
BNH020   EX    R7,MOVACCT2                                              05790001
         B     GETJBN                                                   05800001
ONEACCTF SR    R7,R7                                                    05810001
         IC    R7,0(R1)                                                 05820001
         AH    R7,=H'-1'                                                05830001
         BM    GETJBN                                                   05840001
         CH    R7,=H'7'                                                 05850001
         BNH   BNH030                                                   05860001
         LA    R7,7                                                     05870001
BNH030   EX    R7,MOVACCT1                                              05880001
GETJBN   MVC   JBN001C,SMF4JBN                                          05890001
         ICM   R7,15,SMF4AST                                            05900001
         LA    R4,MSGATIME                                              05910001
         BAL   R8,TIME             FORMAT DEVICE ALLOCATION START TIME  05920001
         ICM   R7,15,SMF4PPST                                           05930001
         LA    R4,MSGLTIME                                              05940001
         BAL   R8,TIME             FORMAT PROBLEM PROGRAM LOAD TIME     05950001
* CALCULATE STEP EXECUTION ELAPSED TIME                                 05960001
         ICM   R7,15,SMF4TME                                            05970001
         ICM   R1,15,SMF4SIT                                            05980001
         SR    R7,R1                                                    05990001
         BM    NEGETME                                                  06000001
         SR    R1,R1                                                    06010001
         B     TSOETCHK                                                 06020001
NEGETME  LA    R1,4                                                     06030001
TSOETCHK CLI   SMF4RTY,34                                               06040001
         BE    TSOETME(R1)                                              06050001
         ZAP   FULLWORD,SMF4DTE                                         06060001
         SP    FULLWORD,SMF4STID                                        06070001
         BZ    SAMEDAY(R1)                                              06080001
         BP    MULTIDAY(R1)                                             06090001
         B     MOVETME        STEP TERMINATION DATE < STEP INITIATION  X06100001
                              DATE IS AN ERROR - BYPASS FORMATTING     X06110001
                              ELAPSED TIME                              06120001
* JOB TERMINATED ON SAME DAY IT WAS INITIATED                           06130001
SAMEDAY  B     ROUNDETM                                                 06140001
         B     MOVETME        STEP INITIATION TIME > STEP TERMINATION  X06150001
                              TIME IS AN ERROR IF STEP INITIATION      X06160001
                              DATE = STEP TERMINATION DATE              06170001
* THERE IS NO STEP INITIATION DATE FIELD IN A TYPE 34 (TSO USER)        06180001
* RECORD --- DO THE BEST YOU CAN WITH THE TIME ONLY                     06190001
TSOETME  B     ROUNDETM                                                 06200001
         B     ADD24HRS                                                 06210001
* JOB TERMINATED ON A LATER DAY THAN THE ONE ON WHICH IT WAS            06220001
* INITIATED (ELAPSED TIME WILL NOT BE CALCULATED FOR A JOB THAT RUNS    06230001
* MORE THAN TWO DAYS OR PASSES THRU A CHANGE OF YEAR)                   06240001
MULTIPOS CP    FULLWORD,=P'1'                                           06250001
         BE    ADD24HRS                                                 06260001
         B     MOVETME                                                  06270001
MULTIDAY B     MULTIPOS                                                 06280001
         SP    FULLWORD,=P'2'                                           06290001
         BM    ADD24HRS                                                 06300001
         BP    MOVETME                                                  06310001
         A     R7,MIDTIME                                               06320001
ADD24HRS A     R7,MIDTIME     IF START TIME > STOP TIME, WE MUST HAVE  X06330001
                              PASSED THRU MIDNIGHT --- ADD 24 HRS       06340001
ROUNDETM AH    R7,=H'50'           ROUND VALUE TO NEAREST SECOND        06350001
         LA    R4,MSGETIME                                              06360001
         BAL   R8,TIME             FORMAT ELAPSED TIME FOR STEP         06370001
MOVETME  MVC   ETME001C,MSGETIME                                        06380001
         BAL   R14,PRINT                                                06390001
         SPACE 1                                                        06400001
         TM    SMF4STI,B'00000001' WAS STEP FLUSHED?                    06410001
         BO    BODBORDR       IF IT WAS, BYPASS GENERATING             X06420001
                              PERFORMANCE INFORMATION                   06430001
         MVC   MSGAREA+1(130),STEPMSG4                                  06440001
         BAL   R14,PRINT                                                06450001
         SPACE 1                                                        06460001
STEPMSG5 MVI   MSGAREA+1,SPACE     CLEAR ALL BUT 1ST AND LAST *         06470001
         MVC   MSGAREA+2(L'MSGAREA-3),MSGAREA+1                         06480001
*              THE BLANKS ARE ALSO USED AS EDIT FILL BYTES              06490001
         MVC   MSGAREA+1+10(11),=X'2020202020202020202120'              06500001
*              COPY THE ABOVE PATTERN BASE FOR ALL OTHERS               06510001
         MVC   MSGAREA+1+37(11),MSGAREA+1+10                            06520001
         MVC   MSGAREA+1+51(11),MSGAREA+1+10                            06530001
         MVC   MSGAREA+1+65(7),MSGAREA+1+14                             06540001
         MVC   MSGAREA+1+75(11),MSGAREA+1+10                            06550001
         MVC   MSGAREA+1+89(11),MSGAREA+1+10                            06560001
         MVC   MSGAREA+1+103(11),MSGAREA+1+10                           06570001
         MVC   MSGAREA+1+117(11),MSGAREA+1+10                           06580001
* POINT TO RELOCATE SECTION OF SMF RECORD TO GET MVS                    06590001
* PAGING/SWAP/PERFORMANCE INFORMATION                                   06600001
         SR    R9,R9                                                    06610001
         ICM   R9,3,SMF4RLCT                                            06620001
         LA    R9,SMFRCD4+4(R9)                                         06630001
         USING SMF4PGIN,R9                                              06640001
         SR    R1,R1                                                    06650001
         ICM   R1,3,SMF4PGNO       PERFORMANCE GROUP NUMBER             06660001
         CVD   R1,DBLWORD                                               06670001
         UNPK  MSGPGNO,DBLWORD+6(2)                                     06680001
         OI    MSGPGNO+L'MSGPGNO-1,X'F0'                                06690001
         ICM   R7,15,SMF4ACT  GET STEP ACTIVE TIME (UNIT IS 1024       X06700001
                              MICROSECONDS)                             06710001
         SR    R6,R6               USE REGISTER PAIR TO HOLD VALUE      06720001
         SLDL  R6,10               MULTIPLY BY 1024 TO GET MICROSECONDS 06730001
         AL    R7,=A(5000)         ROUND TO NEAREST HUNDREDTH OF A SEC  06740001
         BC    12,BC0010           BRANCH IF NO CARRY                   06750001
         LA    R6,1(R6)            INCREMENT R6 ON OVERFLOW FROM R7     06760001
BC0010   D     R6,=A(10000)        REDUCE TO HUNDREDTHS OF SECONDS      06770001
         LA    R4,MSGACT                                                06780001
         BAL   R8,TIMETH           FORMAT STEP ACTIVE TIME              06790001
         ICM   R1,15,SMF4SST       # SERVICE UNITS USED BY STEP         06800001
         CVD   R1,DBLWORD                                               06810001
         ED    MSGSST-1(12),DBLWORD+2                                   06820001
         ICM   R1,15,SMF4PGIN                                           06830001
         CVD   R1,DBLWORD                                               06840001
         ED    MSGPGIN-1(12),DBLWORD+2                                  06850001
         ICM   R1,15,SMF4PGOT                                           06860001
         CVD   R1,DBLWORD                                               06870001
         ED    MSGPGOT-1(12),DBLWORD+2                                  06880001
         ICM   R1,15,SMF4NSW       # TIMES SWAPPED                      06890001
         CVD   R1,DBLWORD                                               06900001
         ED    MSGNSW-1(8),DBLWORD+4                                    06910001
         ICM   R1,15,SMF4PSI                                            06920001
         CVD   R1,DBLWORD                                               06930001
         ED    MSGPSI-1(12),DBLWORD+2                                   06940001
         ICM   R1,15,SMF4PSO                                            06950001
         CVD   R1,DBLWORD                                               06960001
         ED    MSGPSO-1(12),DBLWORD+2                                   06970001
         ICM   R1,15,SMF4VPI                                            06980001
         CVD   R1,DBLWORD                                               06990001
         ED    MSGVPI-1(12),DBLWORD+2                                   07000001
         ICM   R1,15,SMF4VPO                                            07010001
         CVD   R1,DBLWORD                                               07020001
         ED    MSGVPO-1(12),DBLWORD+2                                   07030001
         DROP  R9                                                       07040001
         BAL   R14,PRINT                                                07050001
         SPACE 1                                                        07060001
BODBORDR MVC   MSGAREA+1(130),MSGAREA                                   07070001
         CP    VIO,=P'0'           ANY VIO USAGE TO REPORT?             07080001
         BE    PRNTBRDR                                                 07090001
         MVC   MSGVIOLA,VIOLABL                                         07100001
         UNPK  MSGVIOCT,VIO                                             07110001
         OI    MSGVIOCT+L'MSGVIOCT-1,X'F0'                              07120001
         MVI   MSGVIOSP,SPACE                                           07130001
PRNTBRDR BAL   R14,PRINT           PRINT 2ND BORDER OF ASTERISKS        07140001
         SPACE 1                                                        07150001
         L     R6,16          CVT ADDRESS                               07160001
         L     R4,0(,R6)      TCB/ASCB DBL WORDS                        07170001
         L     R4,12(,R4)     CURRENT ASCB ADDRESS                      07180001
         L     R6,204(,R6)    ACCTABLE ADDR FROM CVTUSER                07190001
         LTR   R6,R6          IS THERE AN ADDR ??                       07200001
         BZ    NOEXCPTB       NO - BYPASS                               07210001
         USING ACCTABLE,R6    ADDRESSABILITY                            07220001
         CLC   UJIHDR,=CL8'UJITABLE'  VERIFY ACCTABLE                   07230001
         BNE   NOEXCPTB       NOT THERE - BYPASS                        07240001
         A     R6,UJITBLEN    POINT TO EXCP TABLE                       07250001
         DROP  R6                                                       07260001
         CLC   0(8,R6),=CL8'EXCPTABL'  VERIFY EXCPTABL                  07270001
         BNE   NOEXCPTB       NOT THERE - BYPASS                        07280001
         LH    R5,36(,R4)     GET ASID NUMBER                           07290001
         M     R4,=F'16'      GET DISPLACEMENT INTO EXCP TABLE          07300001
         LA    R6,8(R5,R6)    POINT TO PROPER ASID ENTRY                07310001
         USING EXCPTABL,R6    ADDRESSABILITY                            07320001
         CLC   SMF4RST,RDRTIME    COMPARE RDR START TIME                07330001
         BNE   NOEXCPTB       NO MATCH - BYPASS                         07340001
         ICM   R0,15,EXCPCNT    COPY DYNAMIC ALLOC EXCP-S               07350001
         A     R0,MSVEXCP     COMBINE WITH NON DYNAMIC EXCPS            07360001
         ST    R0,MSVEXCP     SAVE FOR DOLLAR CALCULATIONS              07370001
         DROP  R6                                                       07380001
NOEXCPTB DS    0H                                                       07390001
         MVI   MSGAREA+1,SPACE     CLEAR ALL BUT 1ST AND LAST *         07400001
         MVC   MSGAREA+2(L'MSGAREA-3),MSGAREA+1                         07410001
         MVC   BLOCKC1,=C'CPU $ (' INSERT HEADERS                       07420001
         MVC   BLOCKC2,=C') + EXCP $ ('                                 07430001
         MVC   BLOCKC3,=C') + MEMORY $ ('                               07440001
         MVC   BLOCKC4,=C') = TOTAL $ ('                                07450001
         MVI   BLOCKC5,C')'                                             07460001
         L     R5,MSVCPU          CPU TIME                              07470001
         M     R4,=A(36)                                                07480001
         D     R4,=A(100)                                               07490001
         LR    R0,R5              = HUND OF $ CPU CHARGE                07500001
         LR    R1,R5                                                    07510001
         BAL   R6,B2$             CONVERT TO ZZ,ZZN.NN $S               07520001
         MVC   BLOCKCP$,WRK$0PAT+1                                      07530001
         L     R5,MSVEXCP         GET HELD EXCP SUM                     07540001
         M     R4,=A(135)                                               07550001
         D     R4,=A(1000)                                              07560001
         AR    R0,R5              = HUND OF $ EXCP CHARGE               07570001
         LR    R1,R5                                                    07580001
         BAL   R6,B2$             CONVERT TO ZZ,ZZN.NN $S               07590001
         MVC   BLOCKEX$,WRK$0PAT+1                                      07600001
         L     R5,MSVMEM          MEMORY USED IN 1K UNITS               07610001
         SRL   R5,1               DIV BY 2 FOR 2K UNITS                 07620001
         M     R4,=A(57)                                                07630001
         L     R4,MSVCPU          CPU USED                              07640001
         MR    R4,R4                                                    07650001
         D     R4,=A(10000)                                             07660001
         AR    R0,R5              = HUND OF $ MEMORY CHARGE             07670001
         LR    R1,R5                                                    07680001
         BAL   R6,B2$             CONVERT TO ZZ,ZZN.NN $S               07690001
         MVC   BLOCKME$,WRK$0PAT+1                                      07700001
         LR    R1,R0              GET HUND OF $ TOTAL CHARGE            07710001
         BAL   R6,B2$             CONVERT TO ZZ,ZZN.NN $S               07720001
         MVC   BLOCKTO$,WRK$0PAT+1                                      07730001
         BAL   R14,PRINT           PRINT DOLLAR LINE                    07740001
         MVC   MSGAREA+1(130),MSGAREA      FILL LINE WITH *             07750001
         BAL   R14,PRINT           PRINT BOTTOM BORDER OF ASTERISKS     07760001
         SPACE 1                                                        07770001
         WTO   MF=(E,WTOAREA)      WRITE STEP STATISTICS MESSAGE TO    X07780001
                                   CONSOLE                              07790001
         SPACE 1                                                        07800001
* CHECK TO SEE IF USER HAS REQUESTED THAT NOTIFICATION OF               07810001
* BACKGROUND JOB STEP TERMINATION BE SENT TO HIS TSO TERMINAL           07820001
         SPACE 1                                                        07830001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 07840001
*        SEND JOB STEP/LOGOFF NOTIFICATION TO TSO USER                * 07850001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 07860001
         MVI   SENDSW,X'00'        INITIALIZE INDICATOR                 07870001
         CLI   SMF4RTY,34          TYPE 34 RECORD?                      07880001
         BE    NOTIFY34            YES, GO SET UP FOR SAYONARA          07890001
         LA    R6,SMF4LENN         -->  LEN FACTOR FOR DEVICES          07900001
         AH    R6,SMF4LENN         POINT TO ACCTG FIELDS                07910001
         SR    R4,R4               CLEAR WORK REG                       07920001
         IC    R4,4(R6)            GET NO. ACCT FIELDS                  07930001
         LTR   R4,R4               ANY ACCT FIELDS FROM EXEC STMT?      07940001
         BZ    EXIT                NO - NO TSO NOTIFY                   07950001
         LA    R6,5(R6)            POINT TO 1ST ACCT FIELD              07960001
         CLI   0(R6),X'06'         LENGTH 6 FROM 'NOTIFY'?              07970001
         BNE   EXIT                NO - NO TSO NOTIFY                   07980001
         CLC   1(6,R6),NOTIFY      IS 1ST PARM 'NOTIFY'?                07990001
         BNE   EXIT                NO - NO TSO NOTIFY                   08000001
         BCTR  R4,0                SUB 1 FROM NO. FIELDS                08010001
         LTR   R4,R4               WAS 'NOTIFY' ONLY FIELD?             08020001
         BZ    EXIT                YES - THEN NO TSO NOTIFY             08030001
         LA    R6,7(,R6)           GET ADDR FOR 1ST FLD PAST ENTRY      08040001
UID34    MVC   SEND(L'SEPFXFMT),SEPFXFMT  MOVE MASK                     08050001
*              INSURE SHASHS (/-S) ARE EVERY PLACE NEEDED               08060001
         MVC   SEND+L'SEPFXFMT(SELEN-L'SEPFXFMT),SEND+L'SEPFXFMT-1      08070001
         L     R1,LCTJSCB          POINT TO P/P JSCB                    08080001
         L     R1,JSCBSSIB-IEZJSCB(R1)                                  08090001
         MVC   SEJOB#,X'0C'(R1)    GET JES2 JOB# FROM SSIB              08100001
         OC    SEJOB#+3(4),=C'0000'  CHANGE BLANKS TO ZEROS             08110001
         MVC   JBNTSO,JBN001C      MOVE JOBNAME INTO MESSAGE            08120001
         MVC   TERMTSO,TERM001C    MOVE STEP TERM CODE TO MSG           08130001
*                                                                       08140001
         CLC   SMF4STMN(8),TAGNAME 'TELAGRAF' PROC ?                    08150001
         BNE   NOTAG               NO, NO VECTOR COUNTS                 08160001
         L     R14,16              R14-->CVT                            08170001
         L     R14,0(R14)          R14-->TCB WORDS                      08180001
         L     R14,4(R14)          R14-->TCB                            08190001
GOTINIT  L     R7,132(R14)         R7-->OTC                             08200001
         LTR   R7,R7               TOP OF TCB CHAIN ?                   08210001
         BZ    GVEC                YES, GET VECTOR COUNT                08220001
         LR    R14,R7              R14-->OTC                            08230001
         B     GOTINIT                                                  08240001
GVEC     MVC   SMF4AST(4),168(R14) SAVE TELAGRAF VECTOR COUNT...        08250001
*                                  ...IN 34 RCD - USE DEVICE ALLOC      08260001
*                                  ...START TIME FIELD                  08270001
         L     R7,168(R14)         R14 = TELAGRAF VECTOR COUNT          08280001
         XC    DUBLWORK,DUBLWORK                                        08290001
         CVD   R7,DUBLWORK                                              08300001
         UNPK  TERMTSO(5),DUBLWORK+5(3)                                 08310001
         OI    TERMTSO+4,X'F0'                                          08320001
         SR    R7,R7                                                    08330001
         ST    R7,168(R14)         RESET TCB VECTOR COUNT               08340001
*                                                                       08350001
NOTAG    MVC   PGMNTSO,PGMN001C    MOVE PROGRAM NAME                    08360001
         MVC   CPUTTSO,CPUT001C    MOVE CPU TIME                        08370001
         MVC   ETIMETSO,ETME001C   MOVE ELAPSED TIME                    08380001
         MVC   ACCTTSO,ACCT001C    MOVE ACCOUNT INFO                    08390001
         MVC   SUBATSO,SUBA001C    MOVE SUB ACCOUNT INFO                08400001
         MVC   JBNTSO,JBN001C      MOVE JOBNAME                         08410001
         MVC   TSOA,TSOTRLR        MOVE ',LOGON,USER=(                  08420001
         XC    TSOU,TSOU           CLEAR WORK AREA (MCS?)               08430001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 08440001
         LA    R14,TSOU            TARGET FOR 1ST ID FIELD              08450001
GETUID   SR    R7,R7               CLEAR WORK REG                       08460001
         IC    R7,0(R6)            GET USERID LENGTH                    08470001
         LTR   R7,R7               0? (NULL PARM)                       08480001
         BZ    NEXTID              YES - GET NEXT ID                    08490001
         CLI   0(R6),X'07'         LEN MORE THAN 7 CHARS?               08500001
         BH    IDTOLONG            YES - IGNORE IT                      08510001
         BCTR  R7,0                DECREM FOR EX                        08520001
         EX    R7,MVCUID           MOVE USERID TO SEND MSG              08530001
         MVI   SENDSW,X'FF'        INDICATE NOTIFY MSG TO SEND          08540001
         LA    R15,1(R7,R14)       POINT TO 1ST CHAR AFTER ID           08550001
         MVI   0(R15),C','         MOVE ID SEPARATOR                    08560001
         LA    R7,1(R7)            UNADJUST FROM EX                     08570001
         LA    R14,1(R14)          ADJUST FOR COMMA                     08580001
NEXTID   LA    R14,0(R7,R14)       TARGET FOR NEXT ID                   08590001
IDTOLONG LA    R6,1(R7,R6)         GET ADDR NEXT ID PARM                08600001
         CLI   TERM001C,C'0'       WAS STEP RC = 0?                     08610001
         BE    EOSEND              YES - THEN NO MORE NOTIFIES          08620001
         BCT   R4,GETUID           GO GET NEXT USERID                   08630001
EOSEND   CLI   SENDSW,X'FF'        IS THERE A NOTIFY MSG TO SEND?       08640001
         BNE   EXIT                NO, THEN WRITE STEP END MSG TO CON   08650001
         BCTR  R14,0               YES, BACK UP TO SET ')'              08660001
         MVI   0(R14),C')'         DOIT                                 08670001
         LA    R4,TSOSE-1          GET BEGIN ADDR OF MSG                08680001
         SR    R14,R4              GET LENGTH OF NOTIFY MSG             08690001
         SLL   R14,16              SHIFT                                08700001
         ST    R14,TSOSE           SET LENGTH                           08710001
         LA    1,1(R4)             SVC 34 PARM ADDR                     08720001
         SR    R0,R0               CLEAR REGISTER 0                     08730001
         SVC   34                  **** TSO STEP NOTIFY MESSAGE ****    08740001
         B     EXIT                                                     08750001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 08760001
*        BUILD PARAMATER LIST FOR 'CCI000I' MESSAGE AT TSO LOGOFF     * 08770001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 08780001
*                                                                       08790001
*        GET LINE ADDRESS FROM TSB AND PLUG INTO TYPE 34                08800001
*    (IF VTAM LINE, PACK LOW ORDER 3 BYTES OF TERM SYMBOLIC NAME)       08810001
*    (INSURE NUMERIC, AND FORCE VALUE TO BE AT LEAST 800 DECIMAL)       08820001
*                                                                       08830001
NOTIFY34 L     R6,16               R4-->CVT                             08840001
         L     R6,0(,R6)           R4-->TCB/ASCB DBL WRDS               08850001
         L     R6,12(,R6)          R4-->CURRENT ASCB                    08860001
         XC    SMF4SYST(2),SMF4SYST  CLEAR NOT NEEDED 2 BYTES           08870001
         CLC   0(4,R6),=C'ASCB'    ENSURE VALIDITY                      08880001
         BNE   ASCBNOF             INVALID, NOTIFY OPERATOR             08890001
         L     R6,60(R6)           R6-->TSB                             08900001
         TM    25(R6),X'01'        THIS A VTAM TSB?                     08910001
         BO    VTAMTSB             YES - GO GET TERM NAME               08920001
         MVC   SMF4SYST(2),82(R6)  GET  TSBLINE (TCAM'S)                08930001
         B     SETSAYO             BACK IN LINE CODE                    08940001
VTAMTSB  LA    R4,109(,R6)         --> XXX OF LOCNSXXX                  08950001
         CLI   104(R6),C'L'        IS IT LOCAL TERM?                    08960001
         BE    VTAMLOC             YES - USE R4 AS IS                   08970001
         CLI   104(R6),C'T'        IS IT NCP TERM?                      08980001
         BNE   SETSAYO             NO - IGNORE                          08990001
         LA    R4,106(,R6)         --> XXX OF TMXXXNN                   09000001
VTAMLOC  LA    R6,2(,R4)           --> LAST BYTE                        09010001
         LA    R7,DBLWORD+4        --> TARGET WORK AREA                 09020001
VTAMTST  LA    R1,VTAMTBL          --> VALID DIGITS TABLE               09030001
VTAMLOP  CLC   0(1,R4),0(R1)       BYTE IN TABLE?                       09040001
         BL    SETSAYO             NO - IGNORE                          09050001
         BE    VTAMHIT             YES - GET REPLACEMENT                09060001
         LA    R1,2(,R1)           ADVANCE IN TABLE                     09070001
         B     VTAMLOP             CONTINUE TEST                        09080001
         SPACE 1                                                        09090001
VTAMHIT  MVC   0(1,R7),1(R1)       PUT X'0X' TO TARGET                  09100001
         CR    R6,R4               END OF CONVERSION?                   09110001
         BNH   VTAMCON             YES - PACK 'EM IN                    09120001
         LA    R4,1(,R4)           ADVANCE TO NEXT INPUT BYTE           09130001
         LA    R7,1(,R7)           ADVANCE TO NEXT OUTPUT BYTE          09140001
         B     VTAMTST             CONTINUE TEST                        09150001
         SPACE 1                                                        09160001
VTAMCON  PACK  DBLWORD(3),DBLWORD+4(4)  PACK NUMERIC PART               09170001
         MVC   SMF4SYST(2),DBLWORD MOVE TO SMF RECORD                   09180001
         OI    SMF4SYST,X'08'      FORCE AT LEAST 800 SERIES NUMBERS    09190001
*                                                                       09200001
*        NOW SEND LOGOFF MSG TO TSO USER                                09210001
*                                                                       09220001
SETSAYO  MVC   SEND34+1(8),JBN001C FOR DUMMY ACCT PARM                  09230001
         LA    R6,0                INIT CTR                             09240001
         LA    R4,8                LOOP CTL                             09250001
         LA    R7,JBN001C          SET TO SCAN USERID FLD               09260001
GETLEN   CLI   0(R7),C' '          END OF USERID?                       09270001
         BE    GOTLEN              YES - SET LEN IN DUMMY PARM          09280001
         LA    R6,1(R6)            UP COUNT OF CHARS                    09290001
         LA    R7,1(R7)            NEXT CHAR IN USERID FLD              09300001
         BCT   R4,GETLEN           SCAN                                 09310001
GOTLEN   STC   R6,SEND34           SET LEN IN DUMMY PARM                09320001
         LA    R4,2                SET NO. IDS TO NOTIFY                09330001
         LA    R7,SEND34           START ADDR OF PARM LIST              09340001
         STC   R6,1(R6,R7)         SET 2ND LEN FIELD                    09350001
         LA    R7,2(R6,R7)         POINT TO 2ND FLD IN LIST             09360001
         MVC   0(8,R7),JBN001C     MOVE IN USERID                       09370001
         LA    R6,SEND34           SET PARM ADDR FOR SCANNING           09380001
         B     UID34               GO FAKE IT OUT                       09390001
*-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --* 09400001
ASCBNOF  WTO   '* NO TSU ASCB *'                                        09410001
         B     SETSAYO                                                  09420001
         SPACE 3                                                        09430001
* EXIT PROCESSING - CANCEL ESTAE, FREE WORKAREA, RESTORE REGISTERS, &   09440001
* RETURN TO CALLER                                                      09450001
         SPACE 1                                                        09460001
EXIT     ESTAE 0                                                        09470001
EXIT1    DS    0H                                                       09480001
*        L     R15,=V(IBMACTRT)       @ ACF2 TRT EXIT DRIVER  WAW       09490001
*        L     R14,SAVE13             @ OUR ORIGINAL REGS               09500001
*        LM    R0,R12,20(R14)         RESTORE REGS 0-12                 09510001
*        BALR  R14,R15                CALL ACF76TRT                     09520001
         L     R10,SAVE10-WORKAREA(,R13)  RELOAD BASE REG               09530001
         LR    R1,R13                                                   09540001
         L     R13,SAVE13                                               09550001
         L     R0,WKAREALN                                              09560001
         FREEMAIN R,LV=(0),A=(1)                                        09570001
         LM    R14,R12,12(R13)     RESTORE REGS                         09580001
         SR    R1,R1                                                    09590001
         SR    R15,R15                                                  09600001
         BR    R14                 RETURN TO CALLER                     09610001
         SPACE 3                                                        09620001
STAEFAIL MVC   WTOAREA(WTOSTAEL),WTOSTAE                                09630001
         CVD   R15,DBLWORD                                              09640001
         UNPK  STAERC,DBLWORD+6(2)                                      09650001
         OI    STAERC+L'STAERC-1,X'F0'                                  09660001
         WTO   MF=(E,WTOAREA)                                           09670001
         B     EXIT1                                                    09680001
         SPACE 3                                                        09690001
         TITLE 'IEFACTRT - SUBROUTINES'                                 09700001
*                                                                       09710001
*        T I M E  CONVERTS 4 BYTES BINARY TIME TO THE PRINT             09720001
*        FORMAT 'HH:MM:SS'.  REGISTER USAGE IS AS FOLLOWS-              09730001
*              R1  NOT USED BY 'TIME' - RESERVED FOR 'TIMETH'           09740001
*                      ROUTINE, TO SAVE RETURN ADDRESS                  09750001
*              R4  POINTS TO LOCATION FOR TIME                          09760001
*              R6  USED FOR DIVIDE                                      09770001
*              R7  USED FOR DIVIDE (CONTAINS TIME IN HUNDREDTHS OF      09780001
*                      SECONDS, ON ENTRY)                               09790001
*              R8  RETURN REGISTER                                      09800001
*                                                                       09810001
*        THIS ROUTINE INSERTS THE :-S AND .-S                           09820001
*                                                                       09830001
         SPACE 1                                                        09840001
TIME     SR    R6,R6                                                    09850001
         D     R6,F360000          DIVIDE BY 360000 TO GET HOURS        09860001
         CVD   R7,DBLWORD          CONVERT HOURS TO PACKED DECIMAL      09870001
         UNPK  0(2,R4),DBLWORD+6(2)  UNPACK HOURS                       09880001
         OI    1(R4),X'F0'         FIX SIGN                             09890001
         SRDL  R6,32               MAKE REMAINDER A DBLWORD WORD        09900001
         D     R6,F6000            DIVIDE BY 6000 TO GET MINUTES        09910001
         CVD   R7,DBLWORD          CONVERT MINUTES TO PACKED DECIMAL    09920001
         UNPK  3(2,R4),DBLWORD+6(2)  UNPACK MINUTES                     09930001
         OI    4(R4),X'F0'         FIX SIGN                             09940001
         SRDL  R6,32               MAKE SEC A DBLWORD WORD IN R6 & 7    09950001
         D     R6,F100             GET RID OF TENTHS AND HUNDREDTHS     09960001
         CVD   R7,DBLWORD          CONVERT SECONDS TO PACKED DECIMAL    09970001
         UNPK  6(2,R4),DBLWORD+6(2)  UNPACK SECONDS                     09980001
         OI    7(R4),X'F0'         FIX SIGN                             09990001
         MVI   2(R4),C':'          1ST COLON                            10000001
         MVI   5(R4),C':'          2ND COLON                            10010001
* NOTE THAT R6 CONTAINS TENTHS & HUNDREDS OF SECONDS. ---THIS           10020001
* VALUE MAY BE USED AFTER RETURN TO CALLER TO EXPAND TIME               10030001
* FROM 'HH:MM:SS' FORMAT TO 'HH:MM:SS.TH' FORMAT.                       10040001
         BR    R8                  RETURN                               10050001
         SPACE 1                                                        10060001
*        T I M E T H  ADDS TENTHS AND HUNDREDTHS OF SECONDS TO          10070001
*        THE TIME FORMATTED AS 'HH:MM:SS' BY 'TIME' SUBROUTINE          10080001
         SPACE 1                                                        10090001
TIMETH   LR    R1,R8               SAVE RETURN ADDRESS                  10100001
         BAL   R8,TIME                                                  10110001
         CVD   R6,DBLWORD                                               10120001
         UNPK  9(2,R4),DBLWORD+6(2)                                     10130001
         OI    10(R4),X'F0'                                             10140001
         MVI   8(R4),C'.'          1ST DOT                              10150001
         BR    R1                                                       10160001
         SPACE 3                                                        10170001
*                                                                       10180001
*        P R I N T  OUTPUTS A 132 BYTE MESSAGE AT JOB OR STEP           10190001
*        TERMINATION TO THE SYSPRINT DEVICE.  REGISTER USAGE IS AS      10200001
*        FOLLOWS-                                                       10210001
*              R12  ADDRESS OF LINKAGE CONTROL TABLE                    10220001
*              R13  POINTS TO 45 WORD AREA                              10230001
*              R14  RETURN REGISTER (PRE-LOADED BY CALLER)              10240001
*              R15  ENTRY POINT TO IEFYS                                10250001
*                                                                       10260001
         SPACE 1                                                        10270001
PRINT    CLI   SMF4RTY,34          TSO STEP TERMINATION?                10280001
         BER   R14            ... SAVE THE CYCLES --- WE'RE            X10290001
                              SENDING TSO LOGON JCL TO THE             X10300001
                              'TRASHWTR' AT THIS TIME...                10310001
         LA    R15,MSGAREA         GET ADDRESS OF MESSAGE               10320001
         ST    R15,36(R12)                                              10330001
         MVC   42(2,R12),MSGLENG   PASS MESSAGE LENGTH                  10340001
         L     R15,VIEFYS          GET ENTRY POINT ADDRESS              10350001
         BR    R15                 GO THERE!                            10360001
         SPACE 1                                                        10370001
*                                                                       10380001
*        B 2 $  CONVERTS A BINARY DOLLAR AMMOUNT TO EBCDIC              10390001
*        AND FORMATS IT FOR PRINTING                                    10400001
*                                                                       10410001
*              R1   BINARY DOLLAR AND CENTS VALUE                       10420001
*              R6   RETURN REGISTER                                     10430001
*              DBLWORD IS A DOUBLE WORD WORK AREA                       10440001
*              WRK$0PAT IS A 10 BYTE WORK AREA                          10450001
*                                                                       10460001
B2$      CVD   R1,DBLWORD                                               10470001
         OI    DBLWORD+7,X'0F'                                          10480001
         MVC   WRK$0PAT,=X'4020206B2021204B2020'                        10490001
         ED    WRK$0PAT,DBLWORD+4                                       10500001
         BR    R6                                                       10510001
         SPACE 1                                                        10520001
         TITLE 'IEFACTRT - CONSTANTS AND WORK AREAS'                    10530001
* EX-EXUTED INSTRUCTIONS                                                10540001
MVCUID   MVC   0(0,R14),1(R6)                                           10550001
MOVACCT1 MVC   ACCT001C(1),1(R1)                                        10560001
MOVACCT2 MVC   SUBA001C(1),1(R1)                                        10570001
X0F      DC    F'15'                                                    10580001
         SPACE 1                                                        10590001
VIEFYS   DC    0F'0',V(IEFYS)                                           10600001
VCOREFMT GETMAIN EC,SP=&SP,MF=L                                         10610001
VCORELN  EQU   *-VCOREFMT                                               10620001
MSGLENG  DC    AL2(L'MSGAREA)                                           10630001
STAELFRM ESTAE TERM=NO,MF=L                                             10640001
STAELN   EQU   *-STAELFRM                                               10650001
WKAREALN DC    0F'0',AL1(&SP),AL3(AREASIZE)                             10660001
MIDTIME  DC    A(24*60*60*100)     1 DAY (IN HUNDREDTHS OF SECONDS)     10670001
F360000  DC    A(60*60*100)        1 HOUR ( " " " )                     10680001
F6000    DC    A(60*100)           1 MINUTE ( " " " )                   10690001
F100     DC    F'100'              1 SECOND ( " " " )                   10700001
         LTORG                                                          10710001
VTAMTBL  DC    C'A',X'0A'          FOR VTAM TERMINAL ID CONVERSION      10720001
         DC    C'B',X'0B'                                               10730001
         DC    C'C',X'0C'                                               10740001
         DC    C'D',X'0D'                                               10750001
         DC    C'E',X'0E'                                               10760001
         DC    C'F',X'0F'                                               10770001
         DC    C'0',X'00'                                               10780001
         DC    C'1',X'01'                                               10790001
         DC    C'2',X'02'                                               10800001
         DC    C'3',X'03'                                               10810001
         DC    C'4',X'04'                                               10820001
         DC    C'5',X'05'                                               10830001
         DC    C'6',X'06'                                               10840001
         DC    C'7',X'07'                                               10850001
         DC    C'8',X'08'                                               10860001
         DC    C'9',X'09'                                               10870001
         DC    X'FF'              STOPPER                               10880001
HEXTRANS DC    C'0123456789ABCDEF'                                      10890001
         SPACE 1                                                        10900001
JCRLJOB  DC    C' JOBCARD READ'                                         10910001
JCRLTSO  DC    C' TSO LOGON AT'                                         10920001
DTFMT    DC    C' YYDDD HH:MM:SS '                                      10930001
VIOLABL  DC    C' VIO IO '                                              10940001
         SPACE 1                                                        10950001
HDRMSG   DC    C'PRC-CCI  370/XXX VS2 RXX.XX XXXX  STEP STATISTICS'     10960001
         SPACE 1                                                        10970001
STEPMSG1 DC    C'  STEP NAME'                    25                     10980001
MSGCORE1 DC    C'USER CORE        *****K'        26                     10990001
         ORG   MSGCORE1+16                                              11000001
         DC    X'402020202120'                                          11010001
         ORG                                                            11020001
SM1B     DC    C'TAPES USED/IO'                  30                     11030001
SM1C     DC    C'START   TIME'                   26                     11040001
SM1D     DC    C'TCB TIME'                       23                     11050001
         SPACE 1                                                        11060001
STEPMSG2 DC    C'  PGM  NAME'                    25                     11070001
MSGCORE2 DC    C'SYSTEM CORE      *****K'        26                     11080001
         ORG   MSGCORE2+16                                              11090001
         DC    X'402020202120'                                          11100001
         ORG                                                            11110001
SM2B     DC    C'DISKS USED/IO'                  30                     11120001
SM2C     DC    C'STOP    TIME'                   26                     11130001
SM2D     DC    C'SRB TIME'                       23                     11140001
         SPACE 1                                                        11150001
STEPMSG3 DC    C'        CODE'                   25                     11160001
SPACES   EQU   STEPMSG3,8                                               11170001
MSGCORE3 DC    C'PRIVATE AREA SZ  *****K'        28                     11180001
         ORG   MSGCORE3+16                                              11190001
         DC    X'402020202120'                                          11200001
         ORG                                                            11210001
SM3B     DC    C'  ALLOC TIME'                   28                     11220001
SM3C     DC    C'ELAPSED TIME'                   26                     11230001
SM3D     DC    C'  PGM LOAD'                     23                     11240001
         SPACE 1                                                        11250001
STEPMSG4 DC    C'* PGNO * NR SRV UNITS * ACTIVE TIME *'                 11260001
         DC    C'* PAGES IN *** PAGES OUT ** # SWAPS * PGS SWAP IN * '  11270001
         DC    C'PGS SWAP OUT * VIO PGS IN * VIO PGS OUT *'             11280001
         SPACE 1                                                        11290001
         ORG                                                            11300001
         SPACE 1                                                        11310001
SEPFXFMT DC    C'SE ''(JES2JOB#) /'                                     11320001
         SPACE 1                                                        11330001
WTO001C  WTO   'CCI001C (JS#XXX)/PGMNAMEX/CP:UT:IM.EX/WA:LL:TI/NOXEC/  X11340001
                     /JOBNAMEX',                                       X11350001
               ROUTCDE=(2),DESC=(6),MF=L                                11360001
WTO001CL EQU   *-WTO001C                                                11370001
         SPACE 1                                                        11380001
*    +4        +012345678 901234567 89012345678901234567890123          11390001
WTOSTAE  WTO   'CCI001D ''IEFACTRT'' ESTAE ENV FAILED (RC=XXX)',       X11400001
               ROUTCDE=(2,10),DESC=4,MF=L                               11410001
WTOSTAEL EQU   *-WTOSTAE                                                11420001
         SPACE 1                                                        11430001
TAGNAME  DC    CL8'TELAGRAF'                                            11440001
NOTIFY   DC    CL6'NOTIFY'                                              11450001
TSOTRLR  DC    CL14''',LOGON,USER=('                                    11460001
         TITLE 'IEFACTRT - ESTAE ERROR RECOVERY CODE'                   11470001
         DROP  R13,R10                                                  11480001
         USING ERREXIT,R15                                              11490001
ERREXIT  CH    R0,=H'12'           WAS AN SDWA SUCCESSFULLY OBTAINED?   11500001
         BE    NOSDWA              IF NOT - RETURN QUICKLY              11510001
         STM   R14,R12,12(R13)                                          11520001
         LA    R10,0(R15)                                               11530001
         SH    R10,=AL2(ERREXIT-IEFACTRT)                               11540001
         DROP  R15                                                      11550001
         USING IEFACTRT,R10                                             11560001
         LR    R8,R15              MAKE ME A BASE                       11570001
         USING ERREXIT,R8                                               11580001
         L     R2,0(R1)                                                 11590001
         L     R2,4(R2)                                                 11600001
         USING WORKAREA,R2                                              11610001
         MVC   RUB,=B'0000000000100100'      INDICATE 'REGISTER        X11620001
                              UPDATE BLOCK' CONTAINS VALUES FOR        X11630001
                              REGS 10 AND 13                            11640001
         SETRP REGS=(14,12),RC=4,RETADDR=RETRY1,RETREGS=YES,RUB=RUB,   X11650001
               FRESDWA=YES,RECORD=YES,COMPCOD=(484,USER)                11660001
         DROP  R10,R2                                                   11670001
         USING WORKAREA,R13                                             11680001
         USING IEFACTRT,R10                                             11690001
RETRY1   WTO   MF=(E,ABEWTO)       WRITE MESSAGE DOCUMENTING           X11700001
                              PROGRAM FAILURE --- FETCH THE SOFTWARE   X11710001
                              RECORD FROM LOGREC FOR DETAILS!           11720001
         B     EXIT                                                     11730001
         SPACE 2                                                        11740001
         DROP  R13,R10,R8                                               11750001
         USING ERREXIT,R15                                              11760001
NOSDWA   LA    R0,RETRY2           ADDRESS OF RETRY ROUTINE             11770001
         LA    R15,4               REQUEST SCHEDULING OF RETRY ROUTINE  11780001
         DROP  R15                                                      11790001
         BR    R14                 RETURN                               11800001
         USING RETRY2,R15                                               11810001
RETRY2   L     R10,0(R1)                                                11820001
         L     R13,4(R1)                                                11830001
         DROP  R15                                                      11840001
         USING WORKAREA,R13                                             11850001
         USING IEFACTRT,R10                                             11860001
         B     EXIT                                                     11870001
         SPACE 2                                                        11880001
         LTORG                                                          11890001
ABEWTO   WTO   'CCI001D ''IEFACTRT'' ABEND',                           X11900001
               ROUTCDE=(2,10),DESC=4,MF=L                               11910001
         SPACE 1                                                        11920001
PTCHAREA DC    0D'0',8CL8'PTCHAREA'  *** PATCH AREA ***                 11930001
         TITLE 'IEFACTRT - DSECTS AND WORK AREAS'                       11940001
ACCTABLE DSECT                                                          11950001
UJITBLEN DC    F'0'           LENGTH OF ACCT TABLE                      11960001
UJIHDR   DC    CL8'UJITABLE'  TABLE IDENTIFICATION                      11970001
START1   DC    F'0'                                                     11980001
START2   DC    F'0'                                                     11990001
START3   DC    F'0'                                                     12000001
START4   DC    F'0'                                                     12010001
START5   DC    F'0'                                                     12020001
START6   DC    F'0'                                                     12030001
START7   DC    F'0'                                                     12040001
STARTP   DC    F'0'                                                     12050001
STARTEND DC    X'FF'          END OF ADDRESS TABLE                      12060001
*                                                                       12070001
RVOLS    DC    50XL10'00'     RESIDENT VOLSER LIST                      12080001
RVOLSEND DC    X'FF'          END OF RESIDENT VOLSER LIST               12090001
*                                                                       12100001
MMAP     DC    125X'00'       'M' ACCOUNTS BITMAP                       12110001
CMAP     DC    125X'00'       'C' ACCOUNTS BITMAP                       12120001
EMAP     DC    125X'00'       'E' ACCOUNTS BITMAP                       12130001
FMAP     DC    125X'00'       'F' ACCOUNTS BITMAP                       12140001
IMAP     DC    125X'00'       'I' ACCOUNTS BITMAP                       12150001
HMAP     DC    125X'00'       'H' ACCOUNTS BITMAP                       12160001
RMAP     DC    125X'00'       'R' ACCOUNTS BITMAP                       12170001
TOTABLEN DC    F'0'           TOTAL LEN OF CSA TABLE                    12180001
TBFIX    EQU   *-ACCTABLE     LENGTH OF FIXED PORTION                   12190001
NUMBERS  EQU   *                                                        12200001
         SPACE 3                                                        12210001
EXCPTABL DSECT                                                          12220001
RDRTIME  DS    F              READER START TIME                         12230001
EXCPCNT  DS    F              EXCP COUNT FROM TYPE 40 RECORDS           12240001
         SPACE 3                                                        12250001
WORKAREA DSECT                     GETMAINED WORKAREA                   12260001
SAVEAREA DS    45F            'IEFYS' REQUIRES R13 POINT TO A          X12270001
                              45 WORD WORKAREA                          12280001
         ORG   SAVEAREA+18*4                                            12290001
DBLWORD  DS    D                   *                                    12300001
FULLWORD DS    F                   * WORKAREAS WHOSE CONTENTS ARE       12310001
FULLWRDS DS    2F                  * WORK FOR UCB SCAN &                12320001
AREAADDR DS    F                   * VOLITILE ACROSS CALLS TO 'IEFYS'   12330001
VCORE    DS    CL(VCORELN)         *                                    12340001
         ORG                                                            12350001
SAVE10   DS    F                   SAVE OUR BASE REG                    12360001
SAVE13   DS    F                   SAVE CALLER'S R13                    12370001
DUBLWORK DS    D                                                        12380001
MSVCPU   DS    F                   SAVE STEP CPU TIME X.XX SEC BINARY   12390001
MSVEXCP  DS    F                   SAVE TOTAL EXCPS BINARY              12400001
MSVMEM   DS    F                   SAVE USER MEMORY 1K BYTES BINARY     12410001
WRK$0PAT DS    CL10                WORK AREA FOR EDIT INSTR             12420001
         SPACE 2                                                        12430001
MSGAREA  DS    CL132                                                    12440001
         SPACE 1                                                        12450001
* DEFINE VARIABLE FIELDS IN MSGAREA FOR DOLLAR TOTAL LINE               12460001
BLOCKC1  EQU   MSGAREA+03,7        C'CPU $ ('                           12470001
BLOCKCP$ EQU   MSGAREA+10,9        CPU $-S                              12480001
BLOCKC2  EQU   MSGAREA+19,12       C') + EXCP $ ('                      12490001
BLOCKEX$ EQU   MSGAREA+31,9        EXCP $-S                             12500001
BLOCKC3  EQU   MSGAREA+40,14       C') + MEMORY $ ('                    12510001
BLOCKME$ EQU   MSGAREA+54,9        MEMORY $-S                           12520001
BLOCKC4  EQU   MSGAREA+63,13       C') = TOTAL $ ('                     12530001
BLOCKTO$ EQU   MSGAREA+76,9        TOTAL $-S                            12540001
BLOCKC5  EQU   MSGAREA+85,1        C')'                                 12550001
         SPACE 1                                                        12560001
* DEFINE VARIABLE FIELDS IN TOP ROW OF ASTERISKS FOR FIRST STEP         12570001
* OF JOB                                                                12580001
         ORG   MSGAREA+4                                                12590001
MSGJCRL  DS    C' JOBCARD READ'                                         12600001
MSGJCRDT DS    C' YYDDD HH:MM:SS '                                      12610001
         ORG   MSGJCRDT+1                                               12620001
MSGJCRD  DS    CL5                 JULIAN DATE WHEN JOB ENTERED SYSTEM  12630001
         DS    CL1                                                      12640001
MSGJCRT  DS    CL8                 TIME OF DAY WHEN JOB ENTERED SYSTEM  12650001
         SPACE 1                                                        12660001
* DEFINE VARIABLE FIELDS IN MESSAGE HEADER LINE                         12670001
         ORG   MSGAREA+43                                               12680001
MSGHDR   DS    C'PRC-CCI  370/XXX VS2 RXX.XX XXXX  STEP STATISTICS'     12690001
         ORG   MSGHDR+13                                                12700001
HDRMODNR DS    CL3                 CPU MODEL (E.G., '158')              12710001
         DS    CL6                                                      12720001
HDRNUMB  DS    CL2                 OPERATING SYSTEM RELEASE NUMBER      12730001
         DS    CL1                                                      12740001
HDRSUBNM DS    CL2                 SUBRELEASE NUMBER                    12750001
         DS    CL1                                                      12760001
HDRSID   DS    CL4                 SMF SYSTEM ID CODE                   12770001
         SPACE 1                                                        12780001
* DEFINE VARIABLE FIELDS IN 'STEPMSG1'                                  12790001
         ORG   MSGAREA+14                                               12800001
MSGSTPN  DS    CL8                 JOBSTEP NAME                         12810001
         ORG   MSGAREA+43                                               12820001
MSGUCORE DS    CL5                                                      12830001
         ORG   MSGAREA+66                                               12840001
MSG#TAPE DS    CL3                 # TAPE DEVICES ALLOCATED BY STEP     12850001
         DS    CL1                                                      12860001
MSGTEXCP DS    CL9                                                      12870001
         ORG   MSGAREA+96                                               12880001
MSGSTIME DS    CL8                                                      12890001
         ORG   MSGAREA+118                                              12900001
MSGTCBTM DS    CL11                                                     12910001
         SPACE 1                                                        12920001
* DEFINE VARIABLE FIELDS IN 'STEPMSG2'                                  12930001
         ORG   MSGAREA+14                                               12940001
MSGPGMN  DS    CL8                                                      12950001
         ORG   MSGAREA+43                                               12960001
MSGSCORE DS    CL5                                                      12970001
         ORG   MSGAREA+66                                               12980001
MSG#DISK DS    CL3                 # DASD DEVICES ALLOCATED BY STEP     12990001
         DS    CL1                                                      13000001
MSGDEXCP DS    CL9                                                      13010001
         ORG   MSGAREA+96                                               13020001
MSGPTIME DS    CL8                                                      13030001
         ORG   MSGAREA+118                                              13040001
MSGSRBTM DS    CL11                                                     13050001
         SPACE 1                                                        13060001
* DEFINE VARIABLE FIELDS IN 'STEPMSG3'                                  13070001
         ORG   MSGAREA+4                                                13080001
MSGNOEXC DS    C'-STEP NOT EXECUTED-'                                   13090001
         ORG   MSGAREA+4                                                13100001
MSGTTYPE DS    CL4                                                      13110001
         ORG   MSGAREA+15                                               13120001
MSGATYPE DS    CL1                                                      13130001
         DS    CL1                                                      13140001
MSGCODE4 DS    0CL4                                                     13150001
MSGCODE3 DS    CL3                                                      13160001
         DS    CL1                                                      13170001
         ORG   MSGAREA+43                                               13180001
MSGARESZ DS    CL5                                                      13190001
         DS    CL2                                                      13200001
MSGVRIND DS    CL3                                                      13210001
         ORG   MSGAREA+68                                               13220001
MSGATIME DS    CL8                                                      13230001
         ORG   MSGAREA+96                                               13240001
MSGETIME DS    CL8                                                      13250001
         ORG   MSGAREA+120                                              13260001
MSGLTIME DS    CL8                                                      13270001
         SPACE 1                                                        13280001
* DEFINE VARIABLE FIELDS AT 'STEPMSG5'                                  13290001
         ORG   MSGAREA+4                                                13300001
MSGPGNO  DS    CL3                 PERFORMANCE GROUP NUMBER             13310001
         ORG   MSGAREA+11                                               13320001
MSGSST   DS    CL11                # SERVICE UNITS USED BY STEP         13330001
         ORG   MSGAREA+25                                               13340001
MSGACT   DS    C'HH:MM:SS.TH'      STEP ACTIVE TIME                     13350001
         ORG   MSGAREA+38                                               13360001
MSGPGIN  DS    CL11                                                     13370001
         ORG   MSGAREA+52                                               13380001
MSGPGOT  DS    CL11                                                     13390001
         ORG   MSGAREA+66                                               13400001
MSGNSW   DS    CL7                                                      13410001
         ORG   MSGAREA+76                                               13420001
MSGPSI   DS    CL11                                                     13430001
         ORG   MSGAREA+90                                               13440001
MSGPSO   DS    CL11                                                     13450001
         ORG   MSGAREA+104                                              13460001
MSGVPI   DS    CL11                                                     13470001
         ORG   MSGAREA+118                                              13480001
MSGVPO   DS    CL11                                                     13490001
         SPACE 1                                                        13500001
* DEFINE VARIABLE FIELDS IN BOTTOM BORDER OF ASTERISKS                  13510001
         ORG   MSGAREA+105                                              13520001
MSGVIOLA DS    CL(L'VIOLABL)                                            13530001
MSGVIOCT DS    CL9                 # VIO EXCPS USED IN STEP             13540001
MSGVIOSP DS    CL1                                                      13550001
         SPACE 2                                                        13560001
         ORG                                                            13570001
         SPACE 2                                                        13580001
WTOAREA  DS    0F,CL(WTO001CL)                                          13590001
         ORG   WTOAREA                                                  13600001
         DS    CL(WTOSTAEL)                                             13610001
         SPACE 1                                                        13620001
* DEFINE VARIABLE FIELDS IN STEP STATISTICS SUMMARY WTO MESSAGE         13630001
         ORG   WTOAREA+4+8                                              13640001
STPN001C DS    CL8                 STEPNAME                             13650001
STP#001C EQU   STPN001C+4,3        STEP # IF NO STEP NAME               13660001
         DS    CL1                                                      13670001
PGMN001C DS    CL8                 PROGRAM NAME (FROM JCL 'EXEC' CARD)  13680001
         DS    CL1                                                      13690001
CPUT001C DS    CL11                CPU TIME USED BY STEP                13700001
         DS    CL1                                                      13710001
ETME001C DS    CL8                 ELAPSED TIME FOR STEP                13720001
         DS    CL1                                                      13730001
TERM001C DS    CL1                 * STEP EXECUTION STATUS              13740001
CODE001C DS    CL4                 * AND/OR TERMINATION CODE            13750001
         DS    CL1                                                      13760001
ACCT001C DS    CL4                 JOB ACCOUNTING INFO                  13770001
SUBA001C DS    CL4                                                      13780001
         DS    CL1                                                      13790001
JBN001C  DS    CL8                 JOBNAME                              13800001
         SPACE 1                                                        13810001
* DEFINE VARIABLE FIELDS IN ESTAE-FAILED MESSAGE                        13820001
         ORG   WTOAREA+4+40                                             13830001
STAERC   DS    CL3                 RETURN CODE FROM ESTAE SVC           13840001
         ORG                                                            13850001
         SPACE 2                                                        13860001
#DISKS   DS    D                                                        13870001
DIO      DS    D                                                        13880001
VIO      DS    D                                                        13890001
IOSW     DS    XL1                                                      13900001
         DS    0H *********** REGISTER UPDATE BLOCK (I.E., 'RUB') ---   13910001
RUB      DS    XL2          * 'RUB' MUST BE ON A HALFWORD BOUNDARY     X13920001
                            * THAT IS NOT ALSO A FULLWORD BOUNDARY,    X13930001
                            * AND MUST IMMEDIATELY PRECEED 'STAER10'    13940001
*                           * AND 'STAER13', IN PRECISELY THAT ORDER    13950001
STAEPRMS DS    0F           *                                           13960001
STAER10  DS    F            *                                           13970001
STAER13  DS    F ************                                           13980001
STAELIST DS    CL(STAELN)                                               13990001
         DS    0D                                                       14000001
         ORG                                                            14010001
*  MESSAGE TO TSO USER WITH ACCT=(NOTIFY,USERID) PARAMETER              14020001
TSOSE    DS    F                                                        14030001
TSOSEND  DS    0CL86                                                    14040001
SEND     DS    C'SE'                                                    14050001
SPACE1   DS    X'40'                                                    14060001
QUOTE1   DS    X'7D'                                                    14070001
         DS    C'('                                                     14080001
SEJOB#   DS    CL8'JOB00000'       JES2  JOB/TSU  NUMBER                14090001
         DS    CL2') '                                                  14100001
JBNTSO   DS    CL8                 JOBNAME                              14110001
         DS    C'/'                                                     14120001
PGMNTSO  DS    CL8                 PROGRAM NAME                         14130001
         DS    C'/'                                                     14140001
TERMTSO  DS    CL5                 TERMINATION CODE                     14150001
         DS    C'/'                                                     14160001
CPUTTSO  DS    CL11                CPU TIME USED BY STEP                14170001
         DS    C'/'                                                     14180001
ETIMETSO DS    CL8                 ELAPSED TIME FOR STEP                14190001
         DS    C'/'                                                     14200001
ACCTTSO  DS    CL4                 JOB ACCOUNTING INFORMATION           14210001
SUBATSO  DS    CL4                                                      14220001
         DS    C'/'                                                     14230001
EOSE     EQU   *                                                        14240001
SELEN    EQU   EOSE-SEND                                                14250001
TSOA     DS    CL14                FOR ',LOGON,USER=('                  14260001
TSOU     DS    CL32              FOR '7777777,7777777,7777777,7777777)' 14270001
SEND34   DS    CL18                                                     14280001
SENDSW   DS    C                                                        14290001
AREASIZE EQU   *-WORKAREA                                               14300001
         SPACE 1                                                        14310001
         END                                                            14320001
./ ENDUP                                                                14330001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE01 14340001
//*                                                                     14350001
//SMPASM02 EXEC SMPASM,M=IEFACTRT,COND=(0,NE)                           14360001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -SMPASM02 14370001
//*                                                                     14380001
//SMPAPP03 EXEC SMPAPP,COND=(0,NE)                                      14390001
//SMPPTFIN DD  *                                                        14400001
++USERMOD (JMUM001).                                                    14410001
++VER (Z038) FMID(EBB1102).                                             14420001
++MOD(IEFACTRT) TXLIB(UMODOBJ).                                         14430001
//SMPCNTL  DD  *                                                        14440001
 RECEIVE SELECT(JMUM001).                                               14450001
 APPLY   SELECT(JMUM001) DIS(WRITE).                                    14460001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -SMPAPP03 14470001
//*                                                                     14480001
//IDCAMS04 EXEC PGM=IDCAMS,REGION=4096K                                 14490001
//* ***************************************************************** * 14500001
//* Define Generation Data Group to retain 5 generations of SMF data  * 14510001
//* ***************************************************************** * 14520001
//MODEL    DD  DSN=SYS2.SMF.DATA,DISP=(NEW,KEEP),                       14530001
//             UNIT=3350,VOL=SER=MVSRES,SPACE=(TRK,0),                  14540001
//             DCB=(RECFM=VBS,LRECL=32756,BLKSIZE=32760)                14550001
//SYSPRINT DD  SYSOUT=*                                                 14560001
//SYSIN    DD  *                                                        14570001
                                                                        14580001
  /* DEFINE SYS2.SMF.DATA GDG                               */          14590001
                                                                        14600001
  DEFINE GENERATIONDATAGROUP (                              -           14610001
               NAME(SYS2.SMF.DATA)                          -           14620001
               LIMIT(5)                                     -           14630001
               SCRATCH )                                                14640001
                                                                        14650001
  IF LASTCC = 0 THEN                                        -           14660001
        LISTCAT ALL ENTRIES(SYS2.SMF.DATA)                              14670001
                                                                        14680001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -IDCAMS04 14690001
//*                                                                     14700001
//UPDATE05 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      14710001
//* ***************************************************************** * 14720001
//* Create SMF Dump/Clear Procedure in SYS1.PROCLIB                   * 14730001
//* ***************************************************************** * 14740001
//SYSUT2   DD  DSN=SYS1.PROCLIB,DISP=MOD                                14750001
//SYSPRINT DD  SYSOUT=*                                                 14760001
//SYSIN    DD  DATA,DLM='><'                                            14770001
./ ADD LIST=ALL,NAME=SMFDUMP                                            14780001
./ NUMBER NEW1=10,INCR=10                                               14790001
//*-------------------------------------------------------------------* 14800001
//*                 SMF DATASET DUMP/CLEAR PROCEDURE                  * 14810001
//*-------------------------------------------------------------------* 14820001
//SMFDUMP   PROC CLASS=X,ID=                                            14830001
//DUMP      EXEC PGM=IFASMFDP,REGION=4096K                              14840001
//SYSPRINT  DD  SYSOUT=&CLASS                                           14850001
//DUMPIN    DD  DSN=SYS1.MAN&ID,DISP=SHR                                14860001
//DUMPOUT   DD  DSN=SYS2.SMF.DATA(+1),DISP=(NEW,CATLG),                 14870001
//             UNIT=SYSDA,VOL=SER=SMP000,SPACE=(CYL,(5,1),RLSE)         14880001
//SYSIN     DD  DSN=SYS2.CONTROL(SMFDUMP),DISP=SHR                      14890001
//*  INDD(DUMPIN,OPTIONS(DUMP))                                         14900001
//*-------------------------------------------------------------------* 14910001
//CLEAR     EXEC PGM=IFASMFDP,REGION=4096K,COND=(0,NE,DUMP)             14920001
//SYSPRINT  DD  SYSOUT=&CLASS                                           14930001
//DUMPIN    DD  DSN=SYS1.MAN&ID,DISP=SHR                                14940001
//DUMPOUT   DD  DUMMY                                                   14950001
//SYSIN     DD  DSN=SYS2.CONTROL(SMFCLEAR),DISP=SHR                     14960001
//*  INDD(DUMPIN,OPTIONS(CLEAR))                                        14970001
//*-------------------------------------------------------------------* 14980001
./ ENDUP                                                                14990001
><                                                                      15000001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE05 15010001
//UPDATE06 EXEC PGM=IEBUPDTE,REGION=1024K,PARM=NEW                      15020001
//* ***************************************************************** * 15030001
//* Create SMF Dump and Clear control cards in SYS2.CONTROL           * 15040001
//* ***************************************************************** * 15050001
//*                                                                     15060001
//SYSUT2   DD  DSN=SYS2.CONTROL,DISP=SHR                                15070001
//SYSPRINT DD  SYSOUT=*                                                 15080001
//SYSIN    DD  *                                                        15090001
./ ADD    LIST=ALL,NAME=SMFDUMP                                         15100001
./ NUMBER NEW1=10,INCR=10                                               15110001
     INDD(DUMPIN,OPTIONS(DUMP))                                         15120001
./ ADD    LIST=ALL,NAME=SMFCLEAR                                        15130001
./ NUMBER NEW1=10,INCR=10                                               15140001
     INDD(DUMPIN,OPTIONS(CLEAR))                                        15150001
./ ENDUP                                                                15160001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE06 15170001
//*                                                                     15180001
//UPDATE07 EXEC PGM=IEBUPDTE,PARM=NEW                                   15190001
//SYSPRINT DD  SYSOUT=*                                                 15200001
//SYSUT2   DD  DISP=SHR,DSN=SYS1.UMODSRC                                15210001
//SYSIN    DD  *                                                        15220001
./ ADD NAME=IEFU29                                                      15230001
IEFU29   TITLE 'SMF SWITCH EXIT ROUTINE                        '        15240001
*---------------------------------------------------------------------* 15250001
*                                                                     * 15260001
*             MODULE NAME = IEFU29                                    * 15270001
*                                                                     * 15280001
*                SMF EXIT ROUTINE TO START DUMP OF SMF WHEN SMF       * 15290001
*                DATASET NEEDS TO BE DUMPED.                          * 15300001
*                                                                     * 15310001
*                                                                     * 15320001
*             FUNCTION =                                              * 15330001
*                ISSUES START COMMAND FOR SYS1.PROCLIB(SMFDUMP)       * 15340001
*                PROCEDURE.                                           * 15350001
*                                                                     * 15360001
*                OPERATION =                                          * 15370001
*                   ISSUES COMMAND 'START SMFDUMP,DSN=SMFDSN'         * 15380001
*                   WHERE SMFDSN WILL BE REPLACED BY THE NAME OF      * 15390001
*                   THE SMF DATASET TO BE DUMPED (SYS1.MANX OR        * 15400001
*                   SYS1.MANY) AND WILL ISSUE A WTO THAT THE          * 15410001
*                   START COMMAND HAS BEEN ISSUED.                    * 15420001
*                                                                     * 15430001
*                 REGISTER CONVENTIONS = STANDARD CONVENTIONS.        * 15440001
*                    REGISTERS 0 TO 1  = WORK REGISTERS               * 15450001
*                    REGISTERS 2 TO 11 = UNUSED                       * 15460001
*                    REGISTER  12      = ADDRESSABILITY TO IEFU29     * 15470001
*                                        CSECT                        * 15480001
*                    REGISTER  13      = ADDRESSIBILITY TO DATA DSECT * 15490001
*                    REGISTERS 14,15   = WORK REGISTERS               * 15500001
*                                                                     * 15510001
*             INPUT = REG1 POINTS TO FULLWORD ADDRESS OF DATA SET     * 15520001
*                     NAME (SYS1.MANX/SYS1.MANY) TO BE DUMPED.        * 15530001
*                                                                     * 15540001
*             OUTPUT = NONE                                           * 15550001
*                                                                     * 15560001
*             RETURN CODE = 0004 TO PREVENT SMF FROM ISSUING          * 15570001
*             MESSAGE IEE362A OR IEE362I                              * 15580001
*                                                                     * 15590001
*             MACROS = SAVE, WTO, RETURN, GETMAIN, FREEMAIN           * 15600001
*                                                                     * 15610001
*---------------------------------------------------------------------* 15620001
         EJECT                                                          15630001
IEFU29   CSECT ,                                                        15640001
*                                                                       15650001
@IDENT01 B     @IDENT04(R15)       BRANCH AROUND IDENT CONSTANTS        15660001
         DC    AL1(@IDENT03-@IDENT02)                                   15670001
@IDENT02 DC    C'IEFU29 '                                               15680001
         DC    C'&SYSDATE &SYSTIME - '                                  15690001
         DC    C'SMF DATASET SWITCH USER EXIT'                          15700001
@IDENT03 DS    0H                                                       15710001
@IDENT04 EQU   *-@IDENT01                                               15720001
*                                                                       15730001
         SAVE  (14,12)             SAVE REGISTERS                       15740001
         USING IEFU29,R12          SET UP BASE ADDRESSABILITY           15750001
         USING DATA,R13            SET UP DATA AREA ADDRESSABILITY      15760001
         LR    R12,R15             LOAD BASE REG WITH ENTRY POINT       15770001
         L     R8,0(R1)            SAVE INPUT PARM(DSNAME)              15780001
         GETMAIN R,LV=LENDATA      GET STORAGE                          15790001
         ST    R13,4(R1)           SAVE CALLER'S SAVE AREA ADDR         15800001
         ST    R1,8(R13)           SAVE MY SAVE AREA ADDRESS            15810001
         LR    R13,R1              LOAD SAVE AREA ADDRESS               15820001
*                                                                       15830000
*---------------------------------------------------------------------* 15830100
* CHECK FOR JES RUNNING ... IF NOT, EXIT WITH NO FURTHER ACTION       * 15830200
*---------------------------------------------------------------------* 15830300
         L     R5,16               ADDRESS CVT                    JLM   15830400
         L     R5,296(R5)          ADDRESS JEST                   JLM   15830500
         L     R5,24(R5)           ADDRESS SSCT                   JLM   15830600
         L     R5,16(R5)           ADDRESS SSCT+10                JLM   15830700
         LTR   R5,R5               IS JES AVAILABLE?              JLM   15830800
         BZ    EXITNOW               NO, EXIT WITHOUT DUMP        JLM   15830900
*---------------------------------------------------------08/2004 JLM-* 15831000
*                                                                       15831101
         MVC   ENQLIST(LENQLIST),ENQLSTX LOAD IN MODEL PARM LIST        15840001
         ENQ   MF=(E,ENQLIST)      TEST IF RESOURCE IN USE?             15850001
         LTR   R15,R15             WAS THE RESOURCE AVAILABLE?          15860001
         BNZ   EXITNOW             NO, EXIT W/O STARTING DUMP           15870001
*                                                                       15880001
         MVC   WTOAREA(WTOLEN),WTOL    MOVE IN WTO MESSAGE              15890001
         MVC   WTOAREA+DSNOFF(1),8(R8)  MOVE DSN ID TO MSG              15900001
         MVC   CMDAREA(CMDLEN),CMDL    MOVE IN START COMMAND            15910001
         MVC   CMDAREA+DSNCOFF(1),8(R8)  MOVE DSN ID TO START CMD       15920001
         SLR   R0,R0               CLEAR REG ZERO FOR SVC 34            15930001
         LA    R1,CMDAREA          POINT TO START COMMAND               15940001
         SVC   34                  ISSUE START COMMAND                  15950001
         WTO   MF=(E,WTOAREA)      ISSUE MESSAGE TO OPERATOR            15960001
*                                                                       15970001
EXITNOW  LR    R1,R13              LOAD ADDRESS OF GETMAINED AREA       15980001
         L     R13,4(R13)          RESTORE CALLER'S SAVE AREA           15990001
         FREEMAIN R,LV=LENDATA,A=(R1)  FREE ACQUIRED STORAGE            16000001
         LM    14,12,12(13)        RESTORE CALLER'S REGISTERS           16010001
         LA    15,4                SET RETURN CODE TO 4                 16020001
         BR    R14                 RETURN                               16030001
*                                                                       16040001
         EJECT                                                          16050001
*---------------------------------------------------------------------* 16060001
*                      CONSTANT DATA AND EQUATES                      * 16070001
*---------------------------------------------------------------------* 16080001
SMFQNAME DC    CL8'SMFQUEUE'                                            16090001
SMFRNAME DC    CL7'DATASET'                                             16100001
*                                                                       16110001
CMDL     DS    0F                  START COMMAND FORMAT FOR SVC 34      16120001
         DC    AL2(CMDLEN),AL2(00) LENGTH OF STRING                     16130001
         DC    C'START SMFDUMP,ID=X'  COMMAND                           16140001
*                456789012345678901                                     16150001
CMDLEN   EQU   *-CMDL              LENGTH OF COMMAND                    16160001
DSNCOFF  EQU   21                  OFFSET OF DSN FIELD IN COMMAND       16170001
*                                                                       16180001
WTOL     WTO   'IEFU29 HAS ISSUED COMMAND ''START SMFDUMP,ID=X ''',    C16190001
               ROUTCDE=(1,2,11),MF=L                                    16200001
*               456789012345678901234567890 123456789012345678          16210001
WTOLEN   EQU   *-WTOL              LENGTH OF WTO STRING                 16220001
DSNOFF   EQU   48                  OFFSET OF DSN FIELD IN MESSAGE       16230001
ENQLSTX  ENQ   (SMFQNAME,SMFRNAME,E,,SYSTEM),RET=TEST,MF=L              16240001
*                                                                       16250001
R0       EQU   0                                                        16260001
R1       EQU   1                                                        16270001
R2       EQU   2                                                        16280001
R3       EQU   3                                                        16290001
R4       EQU   4                                                        16300001
R5       EQU   5                                                        16310001
R6       EQU   6                                                        16320001
R7       EQU   7                                                        16330001
R8       EQU   8                                                        16340001
R9       EQU   9                                                        16350001
R10      EQU   10                                                       16360001
R11      EQU   11                                                       16370001
R12      EQU   12                                                       16380001
R13      EQU   13                                                       16390001
R14      EQU   14                                                       16400001
R15      EQU   15                                                       16410001
         EJECT                                                          16420001
*---------------------------------------------------------------------* 16430001
*                   DATA DSECT (ACQUIRED BY GETMAIN)                  * 16440001
*---------------------------------------------------------------------* 16450001
DATA     DSECT                                                          16460001
SAVE     DS    18F                REGISTER SAVE AREA                    16470001
CMDAREA  DS    0F,XL(CMDLEN)      AREA FOR COMMAND                      16480001
WTOAREA  DS    0F,XL(WTOLEN)      AREA FOR WTO PARM LIST                16490001
ENQLIST  ENQ   (SMFQNAME,SMFRNAME,E,,SYSTEM),RET=TEST,MF=L              16500001
LENQLIST EQU   *-ENQLIST          LENGTH OF WTO STRING                  16510001
RESERVED DS    4D                 RESERVED                              16520001
LENDATA  EQU   *-DATA             EQUATE FOR LENGTH OF DATA DSECT       16530001
*                                                                       16540001
         END   IEFU29                                                   16550001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -UPDATE07 16560001
//*                                                                     16570001
//SMPASM08 EXEC SMPASM,M=IEFU29,COND=(0,NE)                             16580001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -SMPASM08 16590001
//*                                                                     16600001
//SMPAPP09 EXEC SMPAPP,COND=(0,NE)                                      16610001
//SMPPTFIN DD  *                                                        16620001
++USERMOD (JMUM002).                                                    16630001
++VER (Z038) FMID(EBB1102).                                             16640001
++MOD(IEFU29) TXLIB(UMODOBJ).                                           16650001
//SMPCNTL  DD  *                                                        16660001
 RECEIVE SELECT(JMUM002).                                               16670001
 APPLY   SELECT(JMUM002) DIS(WRITE).                                    16680001
//* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -SMPASM09 16690001
//                                                                      16700001
