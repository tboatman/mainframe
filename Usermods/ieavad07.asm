         TITLE 'IEAVAD07 - SAVE AREAS                                  *
                        '
IEAVAD07 CSECT ,                                                   0001
@MAINENT DS    0H                                                  0001
         USING *,@15                                               0001
         B     @PROLOG                                             0001
         DC    AL1(24)                                   @UZ60132@ 0001
         DC    CL24'IEAVAD07 82300  UZ60132'             @UZ60132@ 0001
         DROP  @15
@PROLOG  ST    @14,12(,@13)                                        0001
         STM   @00,@12,20(@13)                                     0001
         BALR  @11,0                                               0001
@PSTART  DS    0H                                                  0001
         USING @PSTART,@11                                         0001
RBPRFXLN EQU 64
RBPRFLNA EQU 32
PRBLEN   EQU 136
SIRBLEN  EQU 200
TIRBLEN  EQU 136
IRBLEN   EQU 128
SVRBLEN  EQU 224
*                                                            @G33CPHE*/
*                                                                  0109
*   /*****************************************************************/
*   /*                                                               */
*   /* INITIALIZATION SAVES THE CALLERS REGISTERS IN THE PROVIDED    */
*   /* SAVE AREA, BUILDS A CONDITIONAL GETMAIN PARAMETER LIST IN     */
*   /* ABDGMA FOR SPACE FOR ITS OWN SAVE AREA AND FOR SPACE FOR THE  */
*   /* COMPILERS REQUIREMENTS. IF THE GETMAIN FAILS, REGISTERS ARE   */
*   /* RESTORED, GPR 15 IS SET TO 8 AND CONTROL IS RETURNED TO THE   */
*   /* CALLER. OTHERWISE, THE SAVE AREAS ARE CHAINED TOGETHER IN     */
*   /* STANDARD FASHION.                                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0110
*   ABDARPTR=PARMPTR;               /* ESTABLISH ADDR OF ABDAREA     */
         LR    ABDARPTR,PARMPTR                                    0110
*   TCBPTR=ABDTCB;                  /* INIT ADDR OF DUMPED TCB       */
         L     TCBPTR,ABDTCB(,ABDARPTR)                            0111
*   PARMPTR=ADDR(ABDASIZE);         /* REG 1 POINTS TO PARM LIST FOR
*                                      GETMAIN                       */
*                                                                  0112
         LA    PARMPTR,ABDASIZE(,ABDARPTR)                         0112
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE THE GETMAIN SVC                                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0113
*   GENERATE;                                                      0113
*                                                                  0113
         GETMAIN EC,LV=@ENDDATD-@DATD+72,                              *
               SP=253,                                                 *
               A=ABDGMA(ABDARPTR),                                     *
               MF=(E,(1))
*   /*****************************************************************/
*   /*                                                               */
*   /* IF SPACE IS NOT AVAILABLE ROUTINE CANNOT FUNCTION             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0114
*   IF RETCODE^=0                   /* DID GETMAIN FAIL              */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0114
         BZ    @RF00114                                            0114
*     DO;                                                          0115
*       RETCODE=8;                  /* SET RETURN CODE               */
         LA    RETCODE,8                                           0116
*       RETURN;                     /* RETURN TO IEAVAD01            */
@EL00001 DS    0H                                                  0117
@EF00001 DS    0H                                                  0117
@ER00001 L     @14,12(,@13)                                        0117
         LM    @00,@12,20(@13)                                     0117
         BR    @14                                                 0117
*     END;                                                         0118
*   ELSE                                                           0119
*     ;                             /* CONTINUE PROCESSING           */
@RF00114 DS    0H                                                  0120
*   PARMPTR=ABDAAREA;               /* RETRIEVE ADDR GOTTEN CORE     */
         L     PARMPTR,ABDAAREA(,ABDARPTR)                         0120
*   SAVEREG->NEXTSAVE=PARMPTR;      /* OLD SAVE POINTS TO NEW SAVE 0121
*                                      AREA                          */
         STCM  PARMPTR,7,NEXTSAVE(SAVEREG)                         0121
*   PARMPTR->PREVSAVE=SAVEREG;      /* NEW SAVE POINTS BACK TO OLD 0122
*                                      SAVE AREA                     */
         STCM  SAVEREG,7,PREVSAVE(PARMPTR)                         0122
*   SAVEREG=PARMPTR;                /* ESTABLISH ADDR OF NEW SAVE  0123
*                                      AREA IN REGISTER 13           */
*                                                                  0123
         LR    SAVEREG,PARMPTR                                     0123
*   /*****************************************************************/
*   /*                                                               */
*   /* ESTABLISH BASE ADDR FOR TEMPORARY STORAGE                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0124
*   AUTOPTR=PARMPTR+72;             /* ADDR OF TEMP IS GM AREA+72    */
*                                                                  0124
         LA    AUTOPTR,72                                          0124
         ALR   AUTOPTR,PARMPTR                                     0124
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE A USING FOR THE COMPILED CODE AND CLEAR IT           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0125
*   GENERATE;                                                      0125
*                                                                  0125
         USING @DATD,AUTOPTR           ESTABLISH ADDRESSABILITY
         XC    0(@ENDDATD-@DATD,AUTOPTR),0(AUTOPTR) 0 TEMP SPACE
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE TASK BEING DUMPED IS FLAGGED COMPLETE THERE CAN BE NO  */
*   /* SAVE AREAS TO DUMP. THEREFORE, THE ROUTINE RETURNS CONTROL TO */
*   /* THE CALLER. OTHERWISE THE RB QUEUE IS SCANNED FOR IRBS. FOR   */
*   /* EACH IRB, THE ROUTINE DISPLAYS THE PP SAVE AREA ASSOCIATED    */
*   /* WITH IT. AFTER THE LAST SUCH SAVE AREA HAS BEEN DISPLAYED,    */
*   /* CONTROL PASSES TO THE NEXT SECTION OF THE ROUTINE             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0126
*   SAVE=SAVEMSG1;                  /* SET UP INIT MSG IN DUMP       */
         MVC   SAVE(16,ABDARPTR),SAVEMSG1                          0126
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0127
*   CALL IEAVAD11;                  /* PRINT THE HEADING             */
*                                                                  0128
         L     @15,@CV02411                                        0128
         BALR  @14,@15                                             0128
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE ROUTINE FAILED FOR ANY REASON KILL THE DUMP            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0129
*   IF RETCODE^=0                   /* OUTPUT FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0129
         BNZ   @RT00129                                            0129
*     GOTO DIE;                     /* END OF ROUTINE AND DUMP       */
*   ELSE                                                           0131
*     ;                             /* CONTINUE PROCESSING           */
*                                                                  0131
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE TASK IS FLAGGED COMPLETE RETURN TO CALLER CODE 0       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0132
*   IF TCBFC='1'B                   /* TASK COMPLETE                 */
*     THEN                          /* YES                           */
         TM    TCBFC(TCBPTR),B'10000000'                           0132
         BO    @RT00132                                            0132
*     GOTO AWAY;                    /* START EXITING                 */
*   ELSE                                                           0134
*     ;                             /* CONTINUE                      */
*   RBSECPTR=TCBRBP;                /* ADDR OF TOP RB                */
*                                                                  0135
         L     RBSECPTR,TCBRBP(,TCBPTR)                            0135
*   /*****************************************************************/
*   /*                                                               */
*   /* SEARCH FOR IRB. IGNORE ALL OTHERS                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0136
*TESTIRB:                           /* REENTER FOR EACH IRB          */
*   IF RBFTP^='010'B                /* IS RB AN IRB                  */
*     THEN                          /* NO                            */
TESTIRB  TM    RBFTP(RBSECPTR),B'01000000'                         0136
         BNO   @RT00136                                            0136
         TM    RBFTP(RBSECPTR),B'10100000'                         0136
         BNZ   @RT00136                                            0136
         L     CURSA,RBPPSAV(,RBSECPTR)                  @UZ60132@
         LA    CURSA,0(,CURSA)                           @UZ60132@
         LTR   CURSA,CURSA                               @UZ60132@
         BZ    NEXTRB                                    @UZ60132@
*     GOTO NEXTRB;                  /* IGNORE THIS RB                */
*   ELSE                                                           0138
*     ;                             /* PROCESS THE IRB               */
*   CURSA=RBPPSAV1;                 /* GET ADR PP SAVE YM2693        */
*        SLR   CURSA,CURSA                               @UZ60132@ 0139
*        ICM   CURSA,7,RBPPSAV1(RBSECPTR)                @UZ60132@ 0139
*   IF CURSA=0                      /* DOES IT EXIST YM2693          */
*     THEN                          /* NO YM2693                     */
*        LTR   CURSA,CURSA                               @UZ60132@ 0140
*        BZ    @RT00140                                  @UZ60132@ 0140
*     GOTO NEXTRB;                  /* IGNORE THIS IRB YM2693        */
*   CALL SAPRINT3;                  /* PRINT SAVE AREA               */
*                                                                  0142
         BAL   @14,SAPRINT3                                        0142
*   /*****************************************************************/
*   /*                                                               */
*   /* IF SUBROUTINE FAILED FOR LACK OF SPACE TERMINATE THE TASK     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0143
*   IF RETCODE=4                    /* OUT OF SPACE                  */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0143
         BE    @RT00143                                            0143
*     GOTO DIE;                     /* END OF DUMP                   */
*   ELSE                                                           0145
*     ;                             /* UPR EVENT DOES NOT KILL THE 0145
*                                      DUMP. ONLY THE DISPLAY OF THE
*                                      CURRENT SAVE AREA             */
*                                                                  0145
*   /*****************************************************************/
*   /*                                                               */
*   /* IRB SEARCH LOOP CONTROL                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0146
*NEXTRB:                            /* SKIP RB ENTERS HERE           */
*   IF RBTCBNXT='1'B                /* IS THIS OLDEST RB ON QUEUE    */
*     THEN                          /* YES                           */
NEXTRB   TM    RBTCBNXT(RBSECPTR),B'10000000'                      0146
         BO    @RT00146                                            0146
*     GOTO FORTRC;                  /* TO NEXT DISPLAY               */
*   ELSE                                                           0148
*     ;                             /* CONTINUE RB CHECKING          */
*   RBSECPTR=RBLINKB;               /* ADDR NEXT RB ON QUEUE         */
*        SLR   @10,@10                                   @UZ60132@ 0149
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0149
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0149
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0149
*   GOTO TESTIRB;                   /* LOOP                          */
*                                                                  0150
         B     TESTIRB                                             0150
*   /*****************************************************************/
*   /*                                                               */
*   /* NORMAL COMPLETION OF SAVE AREA DISPLAY                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0151
*AWAY:                                                             0151
*   RETCODE=0;                      /* NORMAL COMPLETION             */
*                                                                  0151
AWAY     SLR   RETCODE,RETCODE                                     0151
*   /*****************************************************************/
*   /*                                                               */
*   /* ABNORMAL COMPLETION OF DISPLAY                                */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0152
*DIE:                                                              0152
*   SAVERET=RETCODE;                /* SAVE RETURN CODE OVER FREEMAIN*/
DIE      LR    SAVERET,RETCODE                                     0152
*   PARMPTR=SAVEREG;                /* ADDR OF SPACE TO BE FREED     */
         LR    PARMPTR,SAVEREG                                     0153
*   SAVEREG=SAVEREG->PREVSAVE;      /* RESTORE CALLERS SAVE AREA   0154
*                                      ADDRESS                       */
*                                                                  0154
*        SLR   @10,@10                                   @UZ60132@ 0154
*        ICM   @10,7,PREVSAVE(SAVEREG)                   @UZ60132@ 0154
         L     @10,SAVEAREA+4(,SAVEREG)                  @UZ60132@
         LA    @10,0(,@10)                               @UZ60132@
         LR    SAVEREG,@10                                         0154
*   /*****************************************************************/
*   /*                                                               */
*   /* FREE THE SAVE AREA AND DYNAMIC STORAGE                        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0155
*   GENERATE;                                                      0155
         LA    0,@ENDDATD-@DATD+72     SIZE OF SPACE TO BE FREED
         ICM   0,8,K253                SUBPOOL NUMBER
         SVC   10                      FREEMAIN SVC
*   RETCODE=SAVERET;                /* RETSORE RETURN CODE           */
         LR    RETCODE,SAVERET                                     0156
*   RETURN;                         /* RETURN TO MAINLINE            */
*                                                                  0157
         B     @EL00001                                            0157
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SECTION OF THE MODULE DISPLAYS THE SAVE AREAS STARTING   */
*   /* WITH THE SAVE AREA ADDRESS IN TCBFSA. EACH ADDRESS IS VALIDITY*/
*   /* CHECKED TO INSURE THAT THE SPACE IS ADDRESSABLE BY THE TASK   */
*   /* BEING DUMPED                                                  */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0158
*FORTRC:                                                           0158
*   LASTSA=0;                       /* INIT FIELD AND INDICATE NO  0158
*                                      BACK CHAIN CHECK              */
FORTRC   SLR   @10,@10                                             0158
         ST    @10,LASTSA                                          0158
*   SANUM=0;                        /* INIT SAVEAREA NO      @ZA01900*/
         STH   @10,SANUM                                           0159
*   FRSTENT='1'B;                   /* SIGNAL FIRST TIME     @ZA01900*/
         OI    FRSTENT,B'10000000'                                 0160
*   EPINMSG='0'B;                   /* NO EP FOUND YET       @ZA03859*/
         NI    EPINMSG,B'01111111'                                 0161
*   CURSA=TCBFSAB;                  /* SET TO FIRST PP SAVE AREA     */
*        SLR   CURSA,CURSA                               @UZ60132@ 0162
*        ICM   CURSA,7,TCBFSAB(TCBPTR)                   @UZ60132@ 0162
         L     CURSA,TCBFSA(,TCBPTR)                     @UZ60132@
         LA    CURSA,0(,CURSA)                           @UZ60132@
*FORLOOP:                           /* ENTERED FOR EACH SAVE AREA ON
*                                      THE FORWARD CHAIN             */
*                                                                  0163
*   /*****************************************************************/
*   /*                                                               */
*   /* TO NEXT SECTION OF THE DISPLAY WHEN THE LAST SAVE AREA ON THE */
*   /* FORWARD QUEUE HAS BEEN DISPLAYED                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0163
*   IF CURSA=0                      /* LAST SAVE AREA DISPLAYED      */
*     THEN                          /* YES                           */
FORLOOP  LTR   CURSA,CURSA                                         0163
         BZ    @RT00163                                            0163
*     GOTO PRBCODE;                 /* NEXT SECTION OF DISPLAY       */
*   ELSE                                                           0165
*     ;                             /* START DISPLAY OF SAVE AREA    */
*   CALL VALID;                     /* VALID CHECK SAVE AREA ADDR    */
*                                                                  0166
         BAL   @14,VALID                                           0166
*   /*****************************************************************/
*   /*                                                               */
*   /* TEST RESULTS OF VALIDITY CHECK. A CODE OF FOUR MEANS THAT AN  */
*   /* INVALID BACK CHAIN WAS DETECTED. A MESSAGE AND THE CONTENTS OF*/
*   /* THE SAVE AREA WILL BE DISPLAYED BUT THAT NO MORE SAVE AREAS   */
*   /* WILL BE DUMPED. A CODE OF EIGHT MEANS THAT THE SAVE AREA IS   */
*   /* NOT ADDRESSABLE AND THE SAVE AREA IS NOT DISPLAYED            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0167
*   IF RETCODE=4                    /* BACK CHAIN INVALID            */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0167
         BE    @RT00167                                            0167
*     GOTO BADBACK;                 /* PROCESS THE CONDITION         */
*   ELSE                                                           0169
*     ;                             /* CONTINUE CHECKS               */
*   IF RETCODE=8                    /* NOT ADDRESSABLE               */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00072                          @UZ60132@ 0170
         BE    @RT00170                                            0170
*     GOTO PRBCODE;                 /* NEXT SECTION OF DISPLAY       */
*   ELSE                                                           0172
*     ;                             /* DISPLAY THE SAVE AREA         */
*   CALL SAPRINT1;                  /* PRINT SAVE AREA               */
*                                                                  0173
         BAL   @14,SAPRINT1                                        0173
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SUBROUTINE RETURNS A CODE OF FOUR IF ABDUMP FAILED FOR   */
*   /* LACK OF STORAGE. IF THIS HAPPENS THE DISPLAY AND THE DUMP ARE */
*   /* TERMINATED. A CODE OF EIGHT IMPLIES THAT THE SAVE AREA        */
*   /* DISAPPEARED WHILE IT WAS BEING DISPLAYED. SINCE THIS IS       */
*   /* EQUIVALENT TO IT NOT BEING ADDRESSABLE BEFORE AN ATTEMPT TO   */
*   /* DISPLAY IT WAS MADE, THE FORWARD TRACE IS ENDED AND THE NEXT  */
*   /* SECTION OF THE DISPLAY IS IS ENTERED                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0174
*   IF RETCODE=4                    /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0174
         BE    @RT00174                                            0174
*     GOTO DIE;                     /* KILL DISPLAY AND EXIT         */
*   ELSE                                                           0176
*     ;                             /* CONTINUE CODE CHECK           */
*   IF RETCODE=8                    /* UPR EVENT HAPPEN              */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00072                          @UZ60132@ 0177
         BE    @RT00177                                            0177
*     GOTO PRBCODE;                 /* END OF FORWARD TRACE          */
*   ELSE                                                           0179
*     ;                             /* EVERYTHING OK. CONTINUE       */
*   LASTSA=CURSA;                   /* LAST ADDR IS CURRENT ADDR     */
         ST    CURSA,LASTSA                                        0180
*   ABDUPRXT=ADDR(PRBCODE);         /* IF UPR THEN NEXT DISPLAY      */
         LA    @10,PRBCODE                                         0181
         ST    @10,ABDUPRXT(,ABDARPTR)                             0181
*   CURSA=NEXTSAVE;                 /* CURRENT SAVE AREA BECOMES THE
*                                      NEXT SAVE AREA                */
*        SLR   @10,@10                                   @UZ60132@ 0182
*        ICM   @10,7,NEXTSAVE(CURSA)                     @UZ60132@ 0182
         L     @10,SAVEAREA+8(,CURSA)                    @UZ60132@
         LA    @10,0(,@10)                               @UZ60132@
         LR    CURSA,@10                                           0182
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
         SLR   @10,@10                                             0183
         ST    @10,ABDUPRXT(,ABDARPTR)                             0183
*   GOTO FORLOOP;                   /* PROCESS NEXT SAVE AREA        */
*                                                                  0184
         B     FORLOOP                                             0184
*   /*****************************************************************/
*   /*                                                               */
*   /* WHEN AN INVALID BACK CHAIN IS DETECTED IN THE SAVE AREA CHAIN */
*   /* IT PROBABLY MEANS THAT THE CURRENT SAVE AREA IS INVALID. A    */
*   /* MESSAGE IS PRINTED INDICATING THE INVALID CHAINING AND THE    */
*   /* CONTENTS OF THE SAVE AREA ARE DISPLAYED HOWEVER, NO ATTEMPT IS*/
*   /* MADE TO PRODUCE THE WAS ENTERED MESSAGE SINCE THIS WOULD MEAN */
*   /* TREATING THE WHATEVER AS A SAVE AREA                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0185
*BADBACK:                                                          0185
*   IBC=IBCMSG;                     /* INCORRECT BACK CHAIN MSG      */
BADBACK  MVC   IBC(19,ABDARPTR),IBCMSG                             0185
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0186
*   CALL IEAVAD11;                  /* GO TO PRINT THE MESSAGE       */
*                                                                  0187
         L     @15,@CV02411                                        0187
         BALR  @14,@15                                             0187
*   /*****************************************************************/
*   /*                                                               */
*   /* IF OUTPUT FAILED THEN KILL THE DISPLAY AND THE DUMP           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0188
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0188
         BNZ   @RT00188                                            0188
*     GOTO DIE;                     /* BEGIN TERMINATION             */
*   ELSE                                                           0190
*     ;                                                            0190
*   CALL SAPRINT2;                  /* DISPLAY SAVE AREA CONTENTS    */
*                                                                  0191
         BAL   @14,SAPRINT2                                        0191
*   /*****************************************************************/
*   /*                                                               */
*   /* ONLY CODE THAT IS MEANINGFUL UNDER THE CURRENT CONDITIONS IS  */
*   /* FOUR WHICH MEANS THAT ABDUMP FAILED. IF THIS HAPPENS THE      */
*   /* DISPLAY AND THE DUMP ARE TERMINATED                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0192
*   IF RETCODE=4                    /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0192
         BE    @RT00192                                            0192
*     GOTO DIE;                     /* KILL DISPLAY AND DUMP         */
*   ELSE                                                           0194
*     ;                             /* ENTER NEXT SECTION OF DISPLAY */
*                                                                  0194
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SECTION OF THE ROUTINE DISPLAYS THE TWO MOST RECENT SAVE */
*   /* AREAS ASSOCIATED WITH THE MOST RECENT PRB ON THE ACTIVE       */
*   /* PROGRAM QUEUE. AGAIN, NO ATTEMPT IS MADE TO DISPLAY THE       */
*   /* SUPPOSED SAVE AREAS IF VALIDITY CHECK FAILS                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0195
*PRBCODE:                                                          0195
*   RBSECPTR=TCBRBP;                /* GET ADDR MOST RECENT RB       */
PRBCODE  L     RBSECPTR,TCBRBP(,TCBPTR)                            0195
*   SANUM=SANUM-1;                  /* DECREMENT TO LST RB@ZA01900   */
         LH    @10,SANUM                                           0196
         BCTR  @10,0                                               0196
         STH   @10,SANUM                                           0196
*RBLOOP:                            /* CONTINUE SEARCH FOR PRB       */
*                                                                  0197
*   /*****************************************************************/
*   /*                                                               */
*   /* SEARCH FOR MOST RECENT PRB ON Q                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0197
*   IF RBFTP^='000'B                /* CURRENT RB A PRB              */
*     THEN                          /* NO                            */
RBLOOP   TM    RBFTP(RBSECPTR),B'11100000'                         0197
         BZ    @RF00197                                            0197
*     DO;                           /* STEP DOWN RB QUEUE            */
*                                                                  0198
*       /*************************************************************/
*       /*                                                           */
*       /* CHECK IF AT END OF QUEUE                                  */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0199
*       IF RBTCBNXT='1'B            /* LAST RB ON READY QUEUE        */
*         THEN                      /* YES                           */
         TM    RBTCBNXT(RBSECPTR),B'10000000'                      0199
         BO    @RT00199                                            0199
*         GOTO AWAY;                /* NORMAL END OF DISPLAY         */
*       ELSE                                                       0201
*         ;                         /* PROCESS THIS RB               */
*       SAVERB=RBSECPTR;            /* SAVE ADDR OF CURRENT RB       */
         ST    RBSECPTR,SAVERB                                     0202
*       RBSECPTR=RBLINK;            /* SET ADDR NEXT RB ON QUEUE AS
*                                      CURRENT RB                    */
         L     RBSECPTR,RBLINK(,RBSECPTR)                          0203
*       GOTO RBLOOP;                /* CONTINUE SEARCH FOR PRB       */
         B     RBLOOP                                              0204
*     END;                                                         0205
*   ELSE                                                           0206
*     ;                             /* PRB FOUND PROCESS IT          */
@RF00197 DS    0H                                                  0207
*   ABDBPTR=ADDR(RBOPSW)+5;         /* EDIT RESUME ADDR INTO MSG     */
         LA    @10,RBOPSW(,RBSECPTR)                               0207
*        AH    @10,@CH00123                              @UZ60132@ 0207
         AL    @10,@CH00123                              @UZ60132@ 0207
         ST    @10,ABDBPTR(,ABDARPTR)                              0207
*   ABDLPTR=ADDR(ABDLINE);          /* ADDR WHERE DATA TO GO         */
         LA    @10,ABDLINE(,ABDARPTR)                              0208
         ST    @10,ABDLPTR(,ABDARPTR)                              0208
*   ABDLLINE=ADDR(LLINE1);          /* SET LAYOUT LINE ADDR          */
         LA    @10,LLINE1                                          0209
         ST    @10,ABDLLINE(,ABDARPTR)                             0209
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0210
*   CALL IEAVAD41;                  /* FORMAT DATA WITH NO         0211
*                                      INDENTATION FACTOR            */
*                                                                  0211
         L     @15,@CV02432                                        0211
         BALR  @14,@15                                             0211
*   /*****************************************************************/
*   /*                                                               */
*   /* IF FORMAT FAILED THEN TERMINATE DISPLAY AND DUMP              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0212
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0212
         BNZ   @RT00212                                            0212
*     GOTO DIE;                     /* START TERMINATION             */
*   ELSE                                                           0214
*     ;                             /* CONTINUE                      */
*   ABDCC='-';                      /* SET TO TRIPLE SPACE LINE      */
         MVI   ABDCC(ABDARPTR),C'-'                                0215
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0216
*   CALL IEAVAD11;                  /* PRINT THE LINE                */
*                                                                  0217
         L     @15,@CV02411                                        0217
         BALR  @14,@15                                             0217
*   /*****************************************************************/
*   /*                                                               */
*   /* IF OUTPUT FAILED THEN TERMINATE DISPLAY AND DUMP              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0218
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0218
         BNZ   @RT00218                                            0218
*     GOTO DIE;                     /* START TERMINATION             */
*   ELSE                                                           0220
*     ;                             /* CONTINUE                      */
*                                                                  0220
*   /*****************************************************************/
*   /*                                                               */
*   /* BACK CHAIN FROM PRB NOT DISPLAYED IF NOT DUMP OF CURRENT TASK */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0221
*   IF ABDTCB^=ABDCTCB              /* DISPLAY CURRENT TASK          */
*     THEN                          /* NO                            */
         CLC   ABDTCB(4,ABDARPTR),ABDCTCB(ABDARPTR)                0221
         BNE   @RT00221                                            0221
*     GOTO AWAY;                    /* NORMAL RETURN                 */
*   ELSE                                                           0223
*     ;                             /* DISPLAY BACK CHAIN            */
*   CURSA=SAVERB->RBGRS13;          /* GET SAVE AREA ADDR FROM RB    */
         L     @10,SAVERB                                          0224
         L     CURSA,RBGRS13(,@10)                                 0224
*   COUNT=2;                        /* SET LOOP CONTROL              */
         MVC   COUNT(2),@CH00114                                   0225
*PRBLOOP:                           /* REENTERED FOR SECOND SAVE AREA
*                                      DISPLAY                       */
*   LASTSA=0;                       /* INDICATE NO BACK CHAIN      0226
*                                      VALIDITY CHECK TO BE DONE     */
PRBLOOP  SLR   @10,@10                                             0226
         ST    @10,LASTSA                                          0226
*   CALL VALID;                     /* CHECK ADDR VALIDITY           */
*                                                                  0227
         BAL   @14,VALID                                           0227
*   /*****************************************************************/
*   /*                                                               */
*   /* ONLY MEANINGFUL RETURN CODE IS EIGHT WHICH IMPLIES THAT THE   */
*   /* SAVE AREA IS NOT ADDRESSABLE. NO DISPLAY IS MADE BUT NORMAL   */
*   /* RETURN IS TAKEN                                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0228
*   IF RETCODE^=0                   /* INVALID ADDRESS               */
*     THEN                          /* YES                           */
         SLR   @14,@14                                             0228
         CR    RETCODE,@14                                         0228
         BNE   @RT00228                                            0228
*     GOTO AWAY;                    /* NORMAL COMPLETION             */
*   ELSE                                                           0230
*     ;                             /* DISPLAY SAVE AREA             */
*   COUNT=COUNT-1;                  /* DECREMENT LOOP CONTROL        */
*                                                                  0231
         LH    @10,COUNT                                           0231
         BCTR  @10,0                                               0231
         STH   @10,COUNT                                           0231
*   /*****************************************************************/
*   /*                                                               */
*   /* BEFORE DISPLAYING FIRST SAVE AREA PRINT THE PROCEEDING BACK   */
*   /* MESSAGE                                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0232
*   IF COUNT^=0                     /* FIRST DISPLAY                 */
*     THEN                          /* YES                           */
         CR    @10,@14                                             0232
         BE    @RF00232                                            0232
*     DO;                           /* PRINT HEADING                 */
*       PB=PBMSG;                   /* SET MESSAGE IN LINE           */
         MVC   PB(27,ABDARPTR),PBMSG                               0234
*       PARMPTR=ABDARPTR;           /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0235
*       CALL IEAVAD11;              /* PRINT THE LINE                */
*                                                                  0236
         L     @15,@CV02411                                        0236
         BALR  @14,@15                                             0236
*       /*************************************************************/
*       /*                                                           */
*       /* IF OUTPUT FAILED KILL DISPLAY AND DUMP                    */
*       /*                                                           */
*       /*************************************************************/
*                                                                  0237
*       IF RETCODE^=0               /* ABDUMP FAILURE                */
*         THEN                      /* YES                           */
         LTR   RETCODE,RETCODE                                     0237
         BNZ   @RT00237                                            0237
*         GOTO DIE;                 /* KILL DUMP AND DISPLAY         */
*       ELSE                                                       0239
*         ;                         /* END OF HEADING LOGIC          */
*     END;                                                         0240
*   ELSE                                                           0241
*     ;                             /* DISPLAY SAVE AREA             */
@RF00232 DS    0H                                                  0242
*   CALL SAPRINT1;                  /* PRINT SAVE AREA DATA          */
*                                                                  0242
         BAL   @14,SAPRINT1                                        0242
*   /*****************************************************************/
*   /*                                                               */
*   /* RETURN CODES ARE AS BEFORE. A FOUR MEANS ABDUMP FAILED        */
*   /* SOMEWHERE. THE DISPLAY AND DUMP WILL BE TERMINATED. EIGHT     */
*   /* MEANS THE SAVE AREA DISAPPEARED. THE DISPLAY WILL BE STOPPED  */
*   /* BUT RETURN TO CALLER IS NORMAL                                */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0243
*   IF RETCODE=4                    /* ABDUMP FAILURE                */
*     THEN                          /* YES                           */
*        CH    RETCODE,@CH00070                          @UZ60132@ 0243
         C     RETCODE,@CH00070                          @UZ60132@ 0243
         BE    @RT00243                                            0243
*     GOTO DIE;                     /* KILL DISPLAY AND DUMP         */
*   ELSE                                                           0245
*     ;                             /* CHECK FOR BAD SAVE AREA       */
*   IF RETCODE=8                    /* SAVE AREA BECOME BAD          */
*     THEN                          /* YES                           */
*        CH    RETCODE,@CH00072                          @UZ60132@ 0246
         C     RETCODE,@CH00072                          @UZ60132@ 0246
         BE    @RT00246                                            0246
*     GOTO AWAY;                    /* NORMAL END OF ROUTINE         */
*   ELSE                                                           0248
*     ;                             /* CONTINUE PROCESSING           */
*   ABDUPRXT=ADDR(AWAY);            /* EXIT NORMAL IF UPR HAPPENS    */
         LA    @10,AWAY                                            0249
         ST    @10,ABDUPRXT(,ABDARPTR)                             0249
*   CURSA=PREVSAVE;                 /* SET CURRENT SAVE AREA TO SAVE
*                                      AREA BACK CHAINED TO CURRENT
*                                      SAVE AREA                     */
*        SLR   @10,@10                                   @UZ60132@ 0250
*        ICM   @10,7,PREVSAVE(CURSA)                     @UZ60132@ 0250
         L     @10,SAVEAREA+4(,CURSA)                    @UZ60132@ 0250
         LA    @10,0(,@10)                               @UZ60132@ 0250
         LR    CURSA,@10                                           0250
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
*                                                                  0251
         SLR   @10,@10                                             0251
         ST    @10,ABDUPRXT(,ABDARPTR)                             0251
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK IF THIS WAS DISPLAY OF SECOND SAVE AREA. IF SO RETURN TO*/
*   /* CALLER. OTHERWISE REPEAT FOR SECOND SAVE AREA                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0252
*   IF COUNT=0                      /* TWO SAVE AREAS DISPLAYED      */
*     THEN                          /* YES                           */
         CH    @10,COUNT                                           0252
         BE    @RT00252                                            0252
*     GOTO AWAY;                    /* NORMAL EXIT                   */
*   ELSE                            /* MORE TO DO                    */
*     GOTO PRBLOOP;                 /* REPEAT ONCE MORE              */
         B     PRBLOOP                                             0254
*                                                                  0255
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SUBROUTINE CONTAINS THREE ENTRY POINTS WHICH PERFORM THE */
*   /* FUNCTIONS OUTLINED BELOW SAPRINT1 CONSTRUCT AND PRINT THE     */
*   /* STANDARD MESSAGES PRECEDING THE DISPLAY OF SAVE AREA. PASS    */
*   /* CONTROL TO SAPRINT2. SAPRINT2 CHECK IF CALLER REQUESTED THAT  */
*   /* ONLY THE HEADINGS BE DISPLAYED. IF SO RETURN TO THE CALLER    */
*   /* WITH A CODE OF 0. OTHERWISE, PASS CONTROL TO SAPRINT3.        */
*   /* SAPRINT3 DISPLAY THE CONTENTS OF THE CURRENT SAVE AREA        */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0255
*SAPRINT1:                          /* START OF SUBROUTINE           */
*   PROCEDURE OPTIONS(NOSAVEAREA,   /* DO NOT OBTAIN SAVE AREA       */
*       DONTSAVE);                  /* DO NOT SAVE REGISTERS         */
SAPRINT1 DS    0H                                                  0256
*   SA1RET=RETREG;                  /* SAVE RETURN ADDR              */
         ST    RETREG,SA1RET                                       0256
*   ABDUPRXT=ADDR(SAEXIT8);         /* ESTABLISH UPR ROUTINE         */
         LA    @10,SAEXIT8                                         0257
         ST    @10,ABDUPRXT(,ABDARPTR)                             0257
*   SA15=R15A;                      /* GET POSSIBLE EP ADDR          */
*        SLR   @10,@10                                   @UZ60132@ 0258
*        ICM   @10,7,R15A(CURSA)                         @UZ60132@ 0258
         L     @10,SAVEAREA+16(,CURSA)                   @UZ60132@ 0258
         LA    @10,0(,@10)                               @UZ60132@ 0258
         ST    @10,SA15                                            0258
*   ABDUPRXT=0;                     /* CANCEL UPR                    */
         SLR   @15,@15                                             0259
         ST    @15,ABDUPRXT(,ABDARPTR)                             0259
*   HEAD=STANDMSG;                  /* SET HEADING INTO LINE         */
*                                                                  0260
         MVC   HEAD(20,ABDARPTR),STANDMSG                          0260
*   /*****************************************************************/
*   /*                                                               */
*   /* POSSIBLE ENTRY POINT MUST LIE WITHIN THE BOUNDARIES OF THE    */
*   /* MACHINE                                                       */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0261
*   IF SA15=0|                      /* ADDRESS ZER                   */
*       SA15>CVTMZ00                /* VALUE HIGHER THAN MAX ADDR    */
*     THEN                          /* YES                           */
         CR    @10,@15                                             0261
         BE    @RT00261                                            0261
         L     @01,CVTPTR                                          0261
         C     @10,CVTMZ00(,@01)                                   0261
         BH    @RT00261                                            0261
*     GOTO SA1EXITA;                /* CANNOT USE ADDR AND STANDARD
*                                      HEADING WILL BE DISPLAYED     */
*   ELSE                                                           0263
*     ;                             /* ATTEMPT TO DISPLAY MORE     0263
*                                      MEANINGFUL HEADING            */
*   ABDUPRXT=ADDR(SA1EXITA);        /* ESTABLISH UPR ROUTINE         */
*                                                                  0264
         LA    @14,SA1EXITA                                        0264
         ST    @14,ABDUPRXT(,ABDARPTR)                             0264
*   /*****************************************************************/
*   /*                                                               */
*   /* CONSIDERING THE VALUE IN SA15 AS THE ENTRY POINT OF THE       */
*   /* MODULE, DETERMINE IF STANDARD MODULE IDS ARE USED             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0265
*   IF EPI^='47F0'X                 /* INSTRUCTION AT SA15 A BRANCH  */
*     THEN                          /* NO                            */
         CLC   EPI(2,@10),@CB02440                                 0265
         BNE   @RT00265                                            0265
         IC    @14,CNT(,@10)        /* L'EYECATCHER      @UZ60132@   */
         STC   @14,LNG
*     GOTO MSGDONE;                 /* LEAVE EP STUFF OUT            */
*   ELSE                                                           0267
*     ;                             /* TRY TO PUT EP IN MSG          */
*   LNG=CNT;                        /* GET LENGTH OF EP IDENT        */
*        MVC   LNG(1),CNT(@10)                           @UZ60132@ 0268
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
*                                                                  0269
         ST    @15,ABDUPRXT(,ABDARPTR)                             0269
*   /*****************************************************************/
*   /*                                                               */
*   /* IF LENGTH OF ID IS 0 THEN MESSAGE IS COMPLETE                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0270
*   IF LNG=0                        /* MODULE ID 0 LONG              */
*     THEN                          /* YES                           */
         CLI   LNG,0                                               0270
         BE    @RT00270                                            0270
*     GOTO MSGDONE1;                /* NOTHING TO MOVE               */
*   ELSE                                                           0272
*     ;                             /* MOVE ID                       */
*                                                                  0272
*   /*****************************************************************/
*   /*                                                               */
*   /* MAX ID LENGTH FOR MESSAGE IS 70                               */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0273
*   IF LNG>70                       /* OVER SIZE ID                  */
*     THEN                          /* YES                           */
         CLI   LNG,70                                              0273
         BNH   @RF00273                                            0273
*     LNG=70;                       /* SET MAX SIZE                  */
         MVI   LNG,X'46'                                           0274
*   ELSE                                                           0275
*     ;                             /* MOVE ID                       */
@RF00273 DS    0H                                                  0276
*   ATEPPART='AT EP';               /* SET UP MSG                    */
         MVC   ATEPPART(5,ABDARPTR),@CC02443                       0277
*   ABDUPRXT=ADDR(SA1EXITA);        /* ESTABLISH UPR ROUTINE         */
         LA    @10,SA1EXITA                                        0278
         ST    @10,ABDUPRXT(,ABDARPTR)                             0278
*   EPINMSG='1'B;                   /* EP FOUND FOR MSG      @ZA03859*/
*                                                                  0279
         OI    EPINMSG,B'10000000'                                 0279
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
         SLR   @10,@10                                             0281
         ST    @10,ABDUPRXT(,ABDARPTR)                             0281
*                                                        @UZ60132@
         TESTAUTH FCTN=1            /* APF AUTHORIZED ?  @UZ60132@
*                                                        @UZ60132@
         SLR   @14,@14                                   @UZ60132@
         CR    @15,@14              /* TEST RETURN CODE  @UZ60132@   */
         BE    BRAUTH               /* BRANCH IF AUTH    @UZ60132@   */
         CLC   TCBPKF(,TCBPTR),SPKEY8 /* PROB PROG KEY ? @UZ60132@   */
         BL    BRAUTH               /* BRANCH IF KEY < 8 @UZ60132@   */
         SLR   @15,@15                                   @UZ60132@
         LA    @10,SA1EXITB         /* SUBROUTINE ADDR   @UZ60132@   */
         ST    @10,ABDUPRXT(,ABDARPTR)                   @UZ60132@
         SLR   @10,@10                                   @UZ60132@
         IC    @10,TCBPKF(,TCBPTR)  /* GET CALLERS KEY   @UZ60132@   */
         SPKA  0(@10)               /* SET CALLERS KEY   @UZ60132@   */
         SLR   @01,@01                                   @UZ60132@
         IC    @01,LNG              /* GET L'EYECATCHER  @UZ60132@   */
         L     @15,SA15             /* R15 FROM SAVEAREA @UZ60132@   */
         SLR   @10,@10                                   @UZ60132@
         IC    @10,4(@01,@15)                            @UZ60132@
         SPKA  0(@14)               /* RESTORE KEY 0     @UZ60132@   */
BRAUTH   EQU   *                                         @UZ60132@
         LA    @10,SA1EXITA         /* SUBROUTINE ADDR   @UZ60132@   */
         ST    @10,ABDUPRXT(,ABDARPTR)                   @UZ60132@
*   /*****************************************************************/
*   /*                                                               */
*   /* MOVE AND TRANSLATE EYECATCHER                                 */
*   /*                                                               */
*   /*****************************************************************/
*
*   LNG=LNG-1;                      /* DECREMENT FOR EXEC OF MVC     */
         SLR   @10,@10                                             0276
         IC    @10,LNG                                             0276
         BCTR  @10,0                                               0276
         L     @01,SA15                                  @UZ60132@ 0276
         EX    @10,MOVEID           /* MOVE EYECATCHER   @UZ60132@   */
*                                   /* TO OUTPUT LINE    @UZ60132@   */
         STC   @10,LNG              /* STORE DECR LENGTH @UZ60132@   */
         IC    @15,LNG              /* L'EYECATCHER-1 R15@UZ60132@   */
         EX    @15,EXTRAN           /* TRANSLATE         @UZ60132@   */
         B     MSGDONE              /* BRANCH OVER TR TAB@UZ60132@   */
EXTRAN   TR    DESC(1,ABDARPTR),TRANTAB /* EXECUTED INS  @UZ60132@   */
*   /****************************************************@UZ60132@****/
*   /*                                                   @UZ60132@   */
*   /* TRANSLATE TABLE TO REPLACE UNPRINTABLE CHARS      @UZ60132@   */
*   /*                                                   @UZ60132@   */
*   /****************************************************@UZ60132@****/
*                                                        @UZ60132@
TRANTAB  DC    C'................................................'
         DC    C'................................................'
         DC    C'................................................'
         DC    C'................................................'
         DC    C'.ABCDEFGHI.......JKLMNOPQR........STUVWXYZ......'
         DC    C'0123456789......'                       @UZ60132@
*
*MSGDONE:                           /* ENTERED IF NO UPR             */
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
MSGDONE  SLR   @10,@10                                             0281
         ST    @10,ABDUPRXT(,ABDARPTR)                             0281
*MSGDONE1:                          /* ENTERED WHEN UPR OCCURRED     */
*   RBSECPTR=TCBRBP;                /* ADDR OF TOP RB                */
MSGDONE1 L     RBSECPTR,TCBRBP(,TCBPTR)                            0282
*CKRBLOOP:                          /* ENTERED FOR EACH RB           */
*   CALL TESTEP1;                   /* CHECK RB                      */
*                                                                  0283
CKRBLOOP BAL   @14,TESTEP1                                         0283
*   /*****************************************************************/
*   /*                                                               */
*   /* A RETURN CODE OF 4 IMPLIES THAT ABDUMP FAILED. A RETURN CODE  */
*   /* OF EIGHT IMPLIES AN UNEXPECTED UPR. IN EITHER CASE THE DISPLAY*/
*   /* AND THE DUMP ARE TERMINATED                                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0284
*   IF RETCODE=4|                   /* ABDUMP FAIL                   */
*       RETCODE=8                   /* UPR                           */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0284
         BE    @RT00284                                            0284
         C     RETCODE,@CH00072                          @UZ60132@ 0284
         BE    @RT00284                                            0284
*     GOTO TERMINAL;                /* END OF DUMP                   */
*   ELSE                                                           0286
*     ;                             /* CHECK IF DESIRED RB FND       */
*                                                                  0286
*   /*****************************************************************/
*   /*                                                               */
*   /* IF RIGHT RB FOUND THEN BREAK LOOP                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0287
*   IF RETCODE=0                    /* RIGHT RB FOUND                */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0287
         BZ    @RT00287                                            0287
*     GOTO CKHEAD;                  /* BREAK LOOP                    */
*   ELSE                                                           0289
*     ;                             /* CONTINUE SEARCH               */
*                                                                  0289
*   /*****************************************************************/
*   /*                                                               */
*   /* IF LAST RB CHECKED GO TRY LLE                                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0290
*   IF RBTCBNXT='1'B                /* END OF RB CHAIN REACHED       */
*     THEN                          /* YES                           */
         TM    RBTCBNXT(RBSECPTR),B'10000000'                      0290
         BO    @RT00290                                            0290
*     GOTO TRYLLE;                  /* SEARCH LLE FOR ENTRY POINT    */
*   ELSE                                                           0292
*     ;                             /* TRY NEXT RB                   */
*   RBSECPTR=RBLINKB;               /* UPDATE RB POINTER             */
*        SLR   @10,@10                                   @UZ60132@ 0293
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0293
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0293
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0293
*   GOTO CKRBLOOP;                  /* CONTINUE SEARCH               */
         B     CKRBLOOP                                            0294
*SAEXIT8:                           /* A UPR DID NOT OCCUR           */
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
SAEXIT8  SLR   @10,@10                                             0295
         ST    @10,ABDUPRXT(,ABDARPTR)                             0295
*   RETCODE=8;                      /* INDICATE UPR EVENT            */
         LA    RETCODE,8                                           0296
*TERMINAL:                          /* END OF SUBROUTINE             */
*   RETREG=SA1RET;                  /* RESTORE RETURN ADDR           */
TERMINAL L     RETREG,SA1RET                                       0297
*   RETURN;                         /* EXIT TO CALLER                */
@EL00002 DS    0H                                                  0298
@EF00002 DS    0H                                                  0298
@ER00002 BR    @14                                                 0298
*TRYLLE:                            /* LLE SEARCH FOR ENTRY POINT    */
*   LLEPTR=TCBLLS;                  /* ADDR OF MOST RECENT LLE       */
TRYLLE   L     LLEPTR,TCBLLS(,TCBPTR)                              0299
*LLELOOP:                           /* REENTER FOR EACH LLE          */
*                                                                  0300
*   /*****************************************************************/
*   /*                                                               */
*   /* WHEN LAST LLE HAS BEEN CHECKED THEN EP CANNOT BE FOUND AND NO */
*   /* HEADING IS DISPLAYED                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0300
*   IF LLEPTR=0                     /* AT END OF LLE QUEUE           */
*     THEN                          /* YES                           */
LLELOOP  LTR   LLEPTR,LLEPTR                                       0300
         BNZ   @RF00300                                            0300
*     DO;                           /* LOAD MODULE UNKNOW    @ZA03859*/
*       IF EPINMSG='1'B THEN        /* IF EP WAS FOUND       @ZA03859*/
         TM    EPINMSG,B'10000000'                                 0302
         BNO   @RF00302                                            0302
*         DO;                       /* THEN PUT OUT MSG      @ZA03859*/
*           MODNAME='UNKNOWN ';     /* SET NAME IN MSG       @ZA03859*/
         MVC   MODNAME(8,ABDARPTR),@CC02456                        0304
*           EPINMSG='0'B;           /* TURN OFF INDIC.       @ZA03859*/
         NI    EPINMSG,B'01111111'                                 0305
*           GOTO WRITE;             /* GO PUT OUT MSG        @ZA03859*/
         B     WRITE                                               0306
*         END;                      /* END EP FOUND          @ZA03859*/
*       ELSE                        /* OTHERWISE, NO MSG     @ZA03859*/
*         GOTO SA1EXITA;            /* BYPASS MESSAGE        @ZA03859*/
*     END;                          /* END LLEPTR ZERO       @ZA03859*/
*   ELSE                                                           0310
*     ;                             /* CHECK LLE FOR EP              */
@RF00300 DS    0H                                                  0311
*   CALL TESTEP2;                   /* TEST LLE                      */
*                                                                  0311
         BAL   @14,TESTEP2                                         0311
*   /*****************************************************************/
*   /*                                                               */
*   /* SUBROUTINE EXITS FOR RETURN CODE OF 4 OR 8 WHICH MEAN THAT    */
*   /* EITHER ABDUMP FAILED OR AN UNEXPECTED UPR HAPPENED            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0312
*   IF RETCODE=4|                   /* ABDUMP FAIL                   */
*       RETCODE=8                   /* UNEXPECTED UPR                */
*     THEN                          /* YES                           */
         C     RETCODE,@CH00070                          @UZ60132@ 0312
         BE    @RT00312                                            0312
         C     RETCODE,@CH00072                          @UZ60132@ 0312
         BE    @RT00312                                            0312
*     GOTO TERMINAL;                /* END OF SUBROUTINE             */
*   ELSE                                                           0314
*     ;                             /* CONTINUE CHECKS               */
*                                                                  0314
*   /*****************************************************************/
*   /*                                                               */
*   /* A RETURN CODE OF ZERO IMPLIES RIGHT LLE FOUND AND HEADING WAS */
*   /* DISPLAYED                                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0315
*   IF RETCODE=0                    /* LLE FOUND                     */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0315
         BZ    @RT00315                                            0315
*     GOTO CKHEAD;                  /* HEADING DONE AND DISPLAYED    */
*   ELSE                                                           0317
*     ;                             /* CONTINUE SEARCH               */
*   LLEPTR=LLECHN;                  /* NEXT LLE ON QUEUE             */
         L     LLEPTR,LLECHN(,LLEPTR)                              0318
*   GOTO LLELOOP;                   /* CONTINUE SEARCH               */
         B     LLELOOP                                             0319
*                                                        @UZ60132@
*   RESET PSW KEY TO ZERO PRIOR TO SA1EXITA              @UZ60132@
*                                                        @UZ60132@
SA1EXITB SLR   @10,@10                                   @UZ60132@
         SPKA  0(@10)                                    @UZ60132@
*
*SA1EXITA:                          /* NO HEADING FOR REASON         */
*   ABDLINEA=' ';                   /* BLANK OUTPUT LINE             */
SA1EXITA MVI   ABDLINEA+1(ABDARPTR),C' '                           0320
         MVC   ABDLINEA+2(203,ABDARPTR),ABDLINEA+1(ABDARPTR)       0320
         MVI   ABDLINEA(ABDARPTR),C' '                             0320
         SLR   @10,@10                                   @UZ60132@
         ST    @10,ABDUPRXT(,ABDARPTR)                   @UZ60132@
*CKHEAD:                            /* HEADING HAS BEEN PRODUCED     */
*                                                                  0321
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE USER REQUESTED THAT ONLY THE HEADINGS BE PRODUCED THE  */
*   /* SUBROUTINE HAS COMPLETED NORMALLY                             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0321
*   IF APFSAVE2='1'B                /* HEADINGS ONLY                 */
*     THEN                          /* YES                           */
CKHEAD   TM    APFSAVE2(ABDARPTR),B'01000000'                      0321
         BNO   @RF00321                                            0321
*     DO;                           /* END SUBROUTINE                */
*       RETCODE=0;                  /* SET NORMAL END                */
         SLR   RETCODE,RETCODE                                     0323
*       GOTO TERMINAL;              /* START RETURN                  */
         B     TERMINAL                                            0324
*     END;                                                         0325
*   ELSE                                                           0326
*     ;                             /* DISPLAY CONTENTS OF SA        */
@RF00321 DS    0H                                                  0327
*EDITSA:                            /* DISPLAY SA CONTENTS           */
*   ABDCC='0';                      /* DOUBLE SPACING                */
EDITSA   MVI   ABDCC(ABDARPTR),C'0'                                0327
*   WORK=CURSA;                     /* STORE DATA ADDR               */
         ST    CURSA,WORK(,ABDARPTR)                               0328
*   ABDBPTR=ADDR(WORK)+1;           /* FORM ADDR OF SA INTO LINE     */
         LA    @10,WORK(,ABDARPTR)                                 0329
         AL    @10,@CF00065                              @UZ60132@ 0329
         ST    @10,ABDBPTR(,ABDARPTR)                              0329
*   ABDLPTR=ADDR(ABDLINE);          /* ADDR WHERE DATA TO GO         */
         LA    @10,ABDLINE(,ABDARPTR)                              0330
         ST    @10,ABDLPTR(,ABDARPTR)                              0330
*   ABDLLINE=ADDR(SAHDLINE);        /* SET LAYOUT LINE ADDR          */
         LA    @10,SAHDLINE                                        0331
         ST    @10,ABDLLINE(,ABDARPTR)                             0331
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0332
*   CALL IEAVAD41;                  /* FIRST SAVE AREA LINE          */
*                                                                  0333
         L     @15,@CV02432                                        0333
         BALR  @14,@15                                             0333
*   /*****************************************************************/
*   /*                                                               */
*   /* IF ABDUMP FAILED RETURN TO CALLER WITH CODE                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0334
*   IF RETCODE^=0                   /* ABDUMP FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0334
         BNZ   @RT00334                                            0334
*     GOTO TERMINAL;                /* START EXIT                    */
*   ELSE                                                           0336
*     ;                             /* CONTINUE DISPLAY              */
*   LINCNT=3;                       /* NUMBER OF LINES IN DISPLAY    */
         MVI   LINCNT,X'03'                                        0337
*   ABDBPTR=CURSA;                  /* FORMAT CURRENT SAVE AREA      */
         ST    CURSA,ABDBPTR(,ABDARPTR)                            0338
*   ABDLLINE=ADDR(SALLINE1);        /* ADDR OF FIRST LAYOUT LINE     */
         LA    @10,SALLINE1                                        0339
         ST    @10,ABDLLINE(,ABDARPTR)                             0339
*FORMLOOP:                          /* ENTER FOR EA DISPLAY LINE     */
*   UPRFMAT='1'B;                   /* UPR ALLOWABLE IN FORMAT       */
FORMLOOP OI    UPRFMAT(ABDARPTR),B'10000000'                       0340
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0341
*   CALL IEAVAD31;                  /* FIRST SAVE AREA LINE          */
*                                                                  0342
         L     @15,@CV02466                                        0342
         BALR  @14,@15                                             0342
*   /*****************************************************************/
*   /*                                                               */
*   /* IF CALL NOT OK RETURN TO CALLER WITH CODE                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0343
*   IF RETCODE^=0                   /* RESULTS OK                    */
*     THEN                          /* NO                            */
         LTR   RETCODE,RETCODE                                     0343
         BNZ   @RT00343                                            0343
*     GOTO TERMINAL;                /* BACK TO CALLER                */
*   ELSE                                                           0345
*     ;                             /* CONTINUE DISPLAY              */
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0346
*   CALL IEAVAD11;                  /* OUTPUT THE LINE               */
*                                                                  0347
         L     @15,@CV02411                                        0347
         BALR  @14,@15                                             0347
*   /*****************************************************************/
*   /*                                                               */
*   /* IF OUTPUT FAILED RETURN TO CALLER WITH CODE                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0348
*   IF RETCODE^=0                   /* OUTPUT FAIL                   */
*     THEN                          /* YES                           */
         LTR   RETCODE,RETCODE                                     0348
         BNZ   @RT00348                                            0348
*     GOTO TERMINAL;                /* BACK TO CALLER                */
*   ELSE                                                           0350
*     ;                             /* CONTINUE DISPLAY              */
*   LINCNT=LINCNT-1;                /* DECR COUNT BY ONE             */
*                                                                  0351
         SLR   @10,@10                                             0351
         IC    @10,LINCNT                                          0351
         BCTR  @10,0                                               0351
         STC   @10,LINCNT                                          0351
*   /*****************************************************************/
*   /*                                                               */
*   /* SPLIT LOGIC FOR SECOND AND THIRD LINES OF DISPLAY             */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0352
*   IF LINCNT=2                     /* SECOND LINE TO BE DISPLAYED   */
*     THEN                          /* YES                           */
         CLI   LINCNT,2                                            0352
         BE    @RT00352                                            0352
*     GOTO SAL2;                    /* SECOND LINE LOGIC             */
*   ELSE                                                           0354
*     ;                             /* CONTINUE                      */
*   IF LINCNT=1                     /* DOING THIRD LINE              */
*     THEN                          /* YES                           */
         CLI   LINCNT,1                                            0355
         BE    @RT00355                                            0355
*     GOTO SAL3;                    /* THIRD LINE LOGIC              */
*   ELSE                                                           0357
*     ;                             /* DISPLAY IS DONE               */
*   RETCODE=0;                      /* COMPLETED PROCESSING          */
         SLR   RETCODE,RETCODE                                     0358
*   GOTO TERMINAL;                  /* EXIT TO CALLER                */
         B     TERMINAL                                            0359
*SAL2:                              /* SECOND LINE LOGIC             */
*   ABDBPTR=CURSA+24;               /* NEXT SECTION OF SAVE AREA     */
SAL2     LA    @10,24                                              0360
         ALR   @10,CURSA                                           0360
         ST    @10,ABDBPTR(,ABDARPTR)                              0360
*   ABDLLINE=ADDR(SALLINE2);        /* LINE 2 OF DUMP                */
         LA    @10,SALLINE2                                        0361
         ST    @10,ABDLLINE(,ABDARPTR)                             0361
*   GOTO FORMLOOP;                  /* NEXT DISPLAY LINE             */
         B     FORMLOOP                                            0362
*SAL3:                              /* THIRD LINE LOGIC              */
*   ABDBPTR=CURSA+48;               /* NEXT SECTION OF SAVE AREA     */
SAL3     LA    @10,48                                              0363
         ALR   @10,CURSA                                           0363
         ST    @10,ABDBPTR(,ABDARPTR)                              0363
*   ABDLLINE=ADDR(SALLINE3);        /* LINE 2 OF DUMP                */
         LA    @10,SALLINE3                                        0364
         ST    @10,ABDLLINE(,ABDARPTR)                             0364
*   GOTO FORMLOOP;                  /* NEXT DISPLAY LINE             */
         B     FORMLOOP                                            0365
*SAPRINT2:                          /* SAPRINT2 STARTS HERE          */
*   ENTRY;                                                         0366
SAPRINT2 DS    0H                                                  0367
*   SA1RET=RETREG;                  /* SAVE RETURN ADDRESS           */
         ST    RETREG,SA1RET                                       0367
*   GOTO CKHEAD;                    /* ENTER CODE AT PROPER POINT    */
         B     CKHEAD                                              0368
*SAPRINT3:                          /* SAPRINT3 START                */
*   ENTRY;                                                         0369
SAPRINT3 DS    0H                                                  0370
*   SA1RET=RETREG;                  /* SAVE RETURN ADDRESS           */
         ST    RETREG,SA1RET                                       0370
*   GOTO EDITSA;                    /* ENTER CODE AT PROPER POINT    */
         B     EDITSA                                              0371
*   END SAPRINT1;                   /* END OF SUBROUTINE             */
*                                                                  0373
*   /*****************************************************************/
*   /*                                                               */
*   /* THIS SUBROUTINE DETERMINES IF THE SUPPOSED ENTRY POINT, THE   */
*   /* ADDRESS FROM R15 IN THE SAVE AREA, IS CONTAINED WITHIN THE    */
*   /* LIMITS OF THE PROGRAM DESCRIBED BY THE RB OR THE LLE SUPPLIED */
*   /* BY THE CALLER. IF NOT, RETURN TO THE CALLER WITH A CODE.      */
*   /* OTHERWISE, MOVE THE MODULE NAME TO THE MESSAGE BEING BUILT,   */
*   /* COMPLETE THE INFORMATION IN THE MESSAGE PERTAINING TO HOW     */
*   /* ENTERED AND WRITE THE HEADING. SET RETURN CODE TO CALLER TO   */
*   /* ZERO                                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0373
*TESTEP1:                           /* START OF SUBROUTINE           */
*   PROCEDURE OPTIONS(NOSAVEAREA,   /* DO NOT OBTAIN SAVE AREA       */
*       DONTSAVE);                  /* DO NOT SAVE ANY REGISTERS     */
TESTEP1  DS    0H                                                  0374
*   TESTR14=RETREG;                 /* SAVE RETURN ADDRESS           */
*                                                                  0374
         ST    RETREG,TESTR14                                      0374
*   /*****************************************************************/
*   /*                                                               */
*   /* IF SUPPLIED RB IS NOT PRB BACK TO CALLER                      */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0375
*   IF RBFTP^='000'B                /* RB A PRB                      */
*     THEN                          /* NO                            */
         TM    RBFTP(RBSECPTR),B'11100000'                         0375
         BNZ   @RT00375                                            0375
*     GOTO RC12;                    /* BACK TO CALLER                */
*   ELSE                                                           0377
*     ;                             /* CONTINUE CHECKS               */
*   CDENTPTR=RBCDE1;                /* GET ADDR OF CDE FOR PGM       */
*                                                                  0378
*        SLR   CDENTPTR,CDENTPTR                         @UZ60132@ 0378
*        ICM   CDENTPTR,7,RBCDE1(RBSECPTR)               @UZ60132@ 0378
         L     CDENTPTR,RBCDE(,RBSECPTR)                 @UZ60132@ 0378
         LA    CDENTPTR,0(,CDENTPTR)                     @UZ60132@
*   /*****************************************************************/
*   /*                                                               */
*   /* IF PRB HAS NO CDE THEN IT CANNOT BE PROGRAM FOR EP            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0379
*   IF CDENTPTR^=0                  /* CDE WITH THE PRB              */
*     THEN                          /* YES                           */
         LTR   CDENTPTR,CDENTPTR                                   0379
         BNZ   @RT00379                                            0379
*     GOTO TEST2A;                  /* CONTINUE CHECKS               */
*   ELSE                                                           0381
*     ;                             /* BACK TO CALLER                */
*RC12:                              /* SET WRONG PRB OR LLE CODE     */
*   RETCODE=12;                     /* SET CODE TO CALLER            */
RC12     LA    RETCODE,12                                          0382
*TESTEXIT:                          /* COMMON RETURN LOGIC           */
*   RETREG=TESTR14;                 /* RESTORE RETURN ADDR           */
TESTEXIT L     RETREG,TESTR14                                      0383
*   RETURN;                         /* BACK TO CALLER                */
*                                                                  0384
@EL00003 DS    0H                                                  0384
@EF00003 DS    0H                                                  0384
@ER00003 BR    @14                                                 0384
*   /*****************************************************************/
*   /*                                                               */
*   /* ENTRY POINT TO SUBROUTINE IF LLE SUPPLIED                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0385
*TESTEP2:                           /* START LOGIC                   */
*   ENTRY;                                                         0385
TESTEP2  DS    0H                                                  0386
*   TESTR14=RETREG;                 /* SAVE RET ADDR                 */
         ST    RETREG,TESTR14                                      0386
*   CDENTPTR=LLECDPT;               /* ESTAB CDE ADDR FROM LLE       */
         L     CDENTPTR,LLECDPT(,LLEPTR)                           0387
*TEST2A:                            /* LOGIC FOR BOTH ENRTIES        */
*                                                                  0388
*   /*****************************************************************/
*   /*                                                               */
*   /* IF MODULE IS BEING LOADED IT CANNOT BE DESIRED ONE            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0388
*   IF CDNIC='1'B                   /* PARTIALLY LOADED              */
*       |(CDMIN='0'B&CDXLE='0'B)    /* OR CDE BUILD BY INDENT.     0388
*                                                            @ZA09231*/
*     THEN                          /* YES                           */
TEST2A   TM    CDNIC(CDENTPTR),B'01000000'                         0388
         BO    @RT00388                                            0388
         TM    CDMIN(CDENTPTR),B'00000100'                         0388
         BNZ   @RF00388                                            0388
         TM    CDXLE(CDENTPTR),B'00100000'                         0388
         BZ    @RT00388                                            0388
*     GOTO RC12;                    /* SET RET COD AND RETURN        */
*   ELSE                                                           0390
*     ;                             /* CHECK IF CDE FOR RIGHT MOD    */
*                                                                  0390
@RF00388 DS    0H                                                  0391
*   /*****************************************************************/
*   /*                                                               */
*   /* REPLACE MINOR WITH ADDR OF MAJOR                              */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0391
*   IF CDMIN='1'B                   /* CDE FOR MINOR                 */
*     THEN                          /* YES                           */
         TM    CDMIN(CDENTPTR),B'00000100'                         0391
         BNO   @RF00391                                            0391
*     CDENTPTR=CDXLMJP;             /* REPLACE WITH ADDR OF MAJOR    */
         L     CDENTPTR,CDXLMJP(,CDENTPTR)                         0392
*   ELSE                                                           0393
*     ;                             /* CONTINUE CHECK                */
@RF00391 DS    0H                                                  0394
*   XTLSTPTR=CDXLMJP;               /* GET ADDR OF EXTENT LIST       */
         L     XTLSTPTR,CDXLMJP(,CDENTPTR)                         0394
*   NOENTSP=XTLNRFAC;               /* GET NUMBER OF LIST ENTRIES    */
*                                                                  0395
         L     NOENTSP,XTLNRFAC(,XTLSTPTR)                         0395
*   /*****************************************************************/
*   /*                                                               */
*   /* CALC ADDR OF PTR TO BEGINNING OF BLOCK FIELDS                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0396
*   EXSTARTP=ADDR(XTLMSBAA)+(NOENTS-1)*4;                          0396
         LA    @10,XTLMSBAA(,XTLSTPTR)                             0396
         LR    @15,NOENTS                                          0396
         BCTR  @15,0                                               0396
         SLA   @15,2                                               0396
         ALR   @10,@15                                             0396
         ST    @10,EXSTARTP                                        0396
*   EXENDP=ADDR(XTLMSBLA);          /* ADDR OF SIZE OF BLOCK         */
*                                                                  0397
         LA    @10,XTLMSBLA(,XTLSTPTR)                             0397
         ST    @10,EXENDP                                          0397
*   /*****************************************************************/
*   /*                                                               */
*   /* IF ALL EXTENTS PROCESSED THEN NOT DESIRED CDE                 */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0398
*NEXTEXT:                           /* REENTER FOR EACH EXTENT       */
*   IF NOENTS=0                     /* LAST EXTENT PROCESSED         */
*     THEN                          /* YES                           */
NEXTEXT  LTR   NOENTS,NOENTS                                       0398
         BZ    @RT00398                                            0398
*     GOTO RC12;                    /* BACK TO CALLER                */
*   ELSE                                                           0400
*     ;                             /* CHECK CURRENT EXTENT          */
*   EXEND=EXSTART+EXSIZE;           /* CALC END ADDR OF EXTENT       */
*                                                                  0401
         L     @10,EXSTARTP                                        0401
*        SLR   @15,@15                                   @UZ60132@ 0401
*        ICM   @15,7,EXSTART(@10)                        @UZ60132@ 0401
*        L     @10,EXENDP                                @UZ60132@ 0401
*        SLR   @14,@14                                   @UZ60132@ 0401
*        ICM   @14,7,EXSIZE(@10)                         @UZ60132@ 0401
*        ALR   @14,@15                                   @UZ60132@ 0401
*        ST    @14,EXEND                                 @UZ60132@ 0401
         L     @10,0(,@10)                               @UZ60132@
         LA    @10,0(,@10)                               @UZ60132@
         L     @01,EXENDP                                @UZ60132@
         L     @15,0(,@01)                               @UZ60132@
         LA    @15,0(,@15)                               @UZ60132@
         ALR   @15,@10                                   @UZ60132@
         ST    @15,EXEND                                 @UZ60132@
         L     @14,SA15                                  @UZ60132@
         CR    @14,@10                                   @UZ60132@
         BL    @RF00402                                  @UZ60132@
         CR    @14,@15                                   @UZ60132@
         BL    FOUND                                     @UZ60132@
*   /****************************************************@UZ60132@****/
*   /*                                                   @UZ60132@   */
*   /* IF POSSIBLE ENTRY POINT IS ENTIRELY WITHIN EXTENT @UZ60132@   */
*   /* DESIRED CDE HAS BEEN FOUND                        @UZ60132@   */
*   /*                                                   @UZ60132@   */
*   /****************************************************@UZ60132@****/
*                                                        @UZ60132@ 0402
*   IF SA15>=EXSTART&               /* ENTRY ABOVE LOW EN@UZ60132@   */
*       SA15<EXEND                  /* AND BELOW HIGH END@UZ60132@   */
*     THEN                          /* YES               @UZ60132@   */
*        L     @10,SA15                                  @UZ60132@ 0402
*        CR    @10,@15                                   @UZ60132@ 0402
*        BL    @RF00402                                  @UZ60132@ 0402
*        CR    @10,@14                                   @UZ60132@ 0402
*        BL    @RT00402                                  @UZ60132@ 0402
*     GOTO FOUND;                   /* DESIRED CDE LOCATE@UZ60132@   */
*   ELSE                                                 @UZ60132@ 0404
*     ;                             /* CHECK FOR MORE EXTENTS        */
@RF00402 DS    0H                                                  0405
*   NOENTS=NOENTS-1;                /* DECR NUMBER OF EXTENTS        */
         BCTR  NOENTS,0                                            0405
*   EXSTARTP=EXSTARTP+4;            /* ADDR OF NXT EXT LIST ENTRY    */
         LA    @10,4                                               0406
         L     @15,EXSTARTP                                        0406
         ALR   @15,@10                                             0406
         ST    @15,EXSTARTP                                        0406
*   EXENDP=EXENDP+4;                /* ADDR NXT EXT LIST ENTRY       */
         AL    @10,EXENDP                                          0407
         ST    @10,EXENDP                                          0407
*   GOTO NEXTEXT;                   /* CHECK NXT EXT LIST ENTRY      */
         B     NEXTEXT                                             0408
*FOUND:                             /* PROCESS THE CORRECT CDE       */
*   MODNAME=CDNAME;                 /* SET MODULE NAME INTO MSG      */
FOUND    MVC   MODNAME(8,ABDARPTR),CDNAME(CDENTPTR)                0409
*   ABDUPRXT=ADDR(TESTEX8);         /* ESTAB UPR ROUTINE             */
         LA    @10,TESTEX8                                         0410
         ST    @10,ABDUPRXT(,ABDARPTR)                             0410
*   SA14=R14A;                      /* GET SUPPOSED RET ADDR FROM  0411
*                                      SAVE AREA                     */
*        SLR   @10,@10                                   @UZ60132@ 0411
*        ICM   @10,7,R14A(CURSA)                         @UZ60132@ 0411
         L     @10,R14(,CURSA)                           @UZ60132@ 0411
         LA    @10,0(,@10)                               @UZ60132@
         ST    @10,SA14                                            0411
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
*                                                                  0412
         SLR   @15,@15                                             0412
         ST    @15,ABDUPRXT(,ABDARPTR)                             0412
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK IF SUPPOSED RETURN ADDR FROM SAVE AREAS R14 SLOT IS     */
*   /* ADDRESSABLE                                                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0413
*   IF SA14=0|                      /* POSSIBLE ADDR ZERO            */
*       SA14>CVTMZ00                /* OR ABOVE HIGHEST ADDR         */
*     THEN                          /* YES                           */
         CR    @10,@15                                             0413
         BE    @RT00413                                            0413
         L     @01,CVTPTR                                          0413
         C     @10,CVTMZ00(,@01)                                   0413
         BNH   @RF00413                                            0413
@RT00413 DS    0H                                                  0414
*     DO;                           /* NOT A POSSIBLE ADDR           */
*       VIACALL=' ';                /* BLANK ENTERED BY PART OF    0415
*                                      MESSAGE                       */
         MVI   VIACALL+1(ABDARPTR),C' '                            0415
         MVC   VIACALL+2(6,ABDARPTR),VIACALL+1(ABDARPTR)           0415
         MVI   VIACALL(ABDARPTR),C' '                              0415
*       GOTO WRITE;                 /* OUTPUT HEADING                */
         B     WRITE                                               0416
*     END;                                                         0417
*   ELSE                                                           0418
*     ;                             /* BUILD REST OF HEADING         */
@RF00413 DS    0H                                                  0419
*   ABDUPRXT=ADDR(WRITE);           /* ESTABLISH UPR ROUTINE         */
*                                                                  0419
         LA    @10,WRITE                                           0419
         ST    @10,ABDUPRXT(,ABDARPTR)                             0419
*   /*****************************************************************/
*   /*                                                               */
*   /* IF THE RETURN INSTR IS AN SVC 3 THEN ENTRY WAS BY SYSTEM LINK */
*   /* FUNCTION. THE MESSAGE IS CHANGED TO WAS ENTERED VIA LINK AND A*/
*   /* BRANCH IS MADE TO WRITE THE HEADING                           */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0420
*   IF EXITI='0A03'X                /* EXIT INSTRUCTION SVC 3        */
*     THEN                          /* YES                           */
         L     @10,SA14                                            0420
         CLC   EXITI(2,@10),@CB02493                               0420
         BNE   @RF00420                                            0420
*     DO;                           /* ALTER HEADING FOR LINK        */
*       CALLFLD='LINK';             /* CHANGE CALL TO LINK           */
         MVC   CALLFLD(4,ABDARPTR),@CC02494                        0422
*       IF FRSTENT='1'B THEN                                       0423
         TM    FRSTENT,B'10000000'                                 0423
         BNO   @RF00423                                            0423
*         DO;                       /* IF FIRST ENTRY@ZA01900        */
*           FRSTENT='0'B;           /* NO LONGER FRSTENT     @ZA01900*/
         NI    FRSTENT,B'01111111'                                 0425
*           GOTO WRITE;             /* IGNORE FIRST SA       @ZA01900*/
         B     WRITE                                               0426
*         END;                      /* END FRSTENT PROC      @ZA01900*/
*       SANUM=SANUM+1;              /* UP SA COUNT           @ZA01900*/
@RF00423 LH    @10,SANUM                                           0428
         LA    @10,1(,@10)                                         0428
         STH   @10,SANUM                                           0428
*       CALL RBSEARCH;              /* FIND CORRECT RB       @ZA01900*/
         BAL   @14,RBSEARCH                                        0429
*       OPSWSAVE=RBOPSW(5:8);       /* GET RESUME PSW AD     @ZA01900*/
         MVC   OPSWSAVE(4),RBOPSW+4(RBSECPTR)                      0430
*       IF EXITB='4700'X THEN       /* CHECK FOR ID          @ZA01900*/
         L     @10,OPSWSAVE                                        0431
         CLC   EXITB(2,@10),@CH02496                               0431
         BNE   @RF00431                                            0431
*         DO;                       /* SETUP TO CONVERT      @ZA01900*/
*           REG15=OPSWSAVE;         /* SET ADDR IN R15       @ZA01900*/
         L     REG15,OPSWSAVE                                      0433
*           GOTO CNVTENT;           /* GO CONVERT ID         @ZA01900*/
         B     CNVTENT                                             0434
*         END;                      /* END LINKID PROC       @ZA01900*/
*     END;                                                         0436
*   ELSE                                                           0437
*     ;                             /* DO EP NONSENSE                */
*                                                                  0437
@RF00420 DS    0H                                                  0438
*   /*****************************************************************/
*   /*                                                               */
*   /* IF NO CALL ID THEN OUTPUT THE HEADING. OTHERWISE PUT CALL ID  */
*   /* IN HEADING                                                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0438
*   IF EXITI^='4700'X               /* CALL ID EXIST                 */
*     THEN                          /* NO                            */
@RC00420 L     @10,SA14                                            0438
         CLC   EXITI(2,@10),@CB02496                               0438
         BNE   @RT00438                                            0438
*     GOTO WRITE;                   /* OUTPUT HEADING                */
*   ELSE                                                           0440
*     ;                             /* PROCESS ID                    */
*                                                                  0440
*   /*****************************************************************/
*   /*                                                               */
*   /* CONVERT CALL ID TO DECIMAL                                    */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0441
*   GENERATE SETS(ABDWORK);                                        0441
         L     15,SA14        ADDR OF EXIT FROM MODULE
CNVTENT  LH    15,2(15)       ID BUILT BY CALL OR 'LINK' MACRO @ZA01900
         CVD   15,ABDWORK+8(ABDARPTR) CONVERT ID TO DECIMAL
* UNPACK CONVERTED ID INTO WORK AREA YIELDING 8 CHARACTER VALUE
         UNPK  ABDWORK+3(8,ABDARPTR),ABDWORK+12(4,ABDARPTR)
         OI    ABDWORK+10(ABDARPTR),X'F0'   FIX UP INVALID SIGN
* MOVE ID VALUE INTO OUTPUT LINE
         MVC   ABDLINE+31(5,ABDARPTR),ABDWORK+6(ABDARPTR)
*WRITE:                             /* OUTPUT THE HEADING            */
*   ABDUPRXT=0;                     /* DISALLOW UPR INTERRUPTS       */
WRITE    SLR   @10,@10                                             0442
         ST    @10,ABDUPRXT(,ABDARPTR)                             0442
*   ABDCC='0';                      /* DOUBLE SPACE                  */
         MVI   ABDCC(ABDARPTR),C'0'                                0443
*   PARMPTR=ABDARPTR;               /* SET ADDRESS OF PARAM LIST     */
         LR    PARMPTR,ABDARPTR                                    0444
*   CALL IEAVAD11;                  /* OUTPUT MSG LINE               */
         L     @15,@CV02411                                        0445
         BALR  @14,@15                                             0445
*   GOTO TESTEXIT;                  /* BACK TO CALLER                */
         B     TESTEXIT                                            0446
*TESTEX8:                           /* UPR OCCURED BACK I GO         */
*   RETCODE=8;                      /* INDICATE UPR TO CALLER        */
TESTEX8  LA    RETCODE,8                                           0447
*   GOTO TESTEXIT;                  /* RETURN TO CALLER              */
         B     TESTEXIT                                            0448
*   END TESTEP1;                    /* END OF SUBROUTINE             */
*                                                                  0450
*/* SUBROUTINE RBSEARCH ENTERED TO FIND THE ADDRESS OF A PRB @ZA01900*/
*/* WITH INTERRUPT CODE OF 0006 OR 000C SHOWING THAT THE PRB @ZA01900*/
*/* ISSUED EITHER A LINK OR A SYNCH SVC.                     @ZA01900*/
*/* THE ONE INPUT PARAMETER IS SANUM, WHICH TELLS US HOW OLD @ZA01900*/
*/* THE PRB WE ARE SEARCHING FOR IS.  E.G. IF SANUM IS ONE,  @ZA01900*/
*/* THEN WE ARE LOOKING FOR THE OLDEST PRB WITH INTERRUPT    @ZA01900*/
*/* CODE OF 6 OR C.  IF SANUM IS 2 THEN WE WANT THE SECOND-  @ZA01900*/
*/* OLDEST, ETC.                                             @ZA01900*/
*/* ALL REGISTERS EXCEPT 8 (RBSECPTR) ARE SAVED AND RESTORED @ZA01900*/
*/* THE OUTPUT IS THE RB PTR IN REG8 - RBSECPTR.             @ZA01900*/
*                                                                  0450
*RBSEARCH:                                                         0450
*   PROC OPTIONS(DONTSAVE(8));      /* SEARCH FOR LINKPRB    @ZA01900*/
RBSEARCH STM   @14,@07,12(@13)                                     0450
         STM   @09,@12,56(@13)                                     0450
*   RBSECPTR=TCBRBP;                /* GET TOP RB ADDRESS    @ZA01900*/
*                                                                  0451
         L     RBSECPTR,TCBRBP(,TCBPTR)                            0451
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK FOR PRB AND IC OF 6 FOR LINK OR C FOR SYNCH     @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0452
*   IF RBFTP='000'B&(RBINTCOD='0006'X/*                      @ZA01900*/
*       �RBINTCOD='000C'X) THEN     /* IF ONE OF OURS THEN   @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0452
         BNZ   @RF00452                                            0452
         LR    @10,RBSECPTR                                        0452
         SL    @10,@CF01199                              @UZ60132@ 0452
         CLC   RBINTCOD(2,@10),@CB02502                            0452
         BE    @RT00452                                            0452
         CLC   RBINTCOD(2,@10),@CB02503                            0452
         BNE   @RF00452                                            0452
@RT00452 DS    0H                                                  0453
*     LINKPRBS=1;                   /* SET COUNT TO ONE      @ZA01900*/
         MVC   LINKPRBS(4),@CF00065                                0453
*   ELSE                                                           0454
*     LINKPRBS=0;                   /* ELSE ZERO COUNT       @ZA01900*/
*                                                                  0454
         B     @RC00452                                            0454
@RF00452 SLR   @10,@10                                             0454
         ST    @10,LINKPRBS                                        0454
*   /*****************************************************************/
*   /*                                                               */
*   /* LOOP OVER RBS TO GET COUNT OF LINKS & SYNCHES         @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0455
*   DO WHILE(RBTCBNXT^='1'B);       /* LOOP OVER RB CHAIN    @ZA01900*/
@RC00452 B     @DE00455                                            0455
@DL00455 DS    0H                                                  0456
*     RBSECPTR=RBLINKB;             /* GET NEXT RB           @ZA01900*/
*        SLR   @10,@10                                   @UZ60132@ 0456
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0456
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0456
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0456
*     IF RBFTP='000'B&(RBINTCOD='0006'X/*                    @ZA01900*/
*         |RBINTCOD='000C'X) THEN   /* IF ONE OF OURS        @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0457
         BNZ   @RF00457                                            0457
         LR    @10,RBSECPTR                                        0457
         SL    @10,@CF01199                              @UZ60132@ 0457
         CLC   RBINTCOD(2,@10),@CB02502                            0457
         BE    @RT00457                                            0457
         CLC   RBINTCOD(2,@10),@CB02503                            0457
         BNE   @RF00457                                            0457
@RT00457 DS    0H                                                  0458
*       LINKPRBS=LINKPRBS+1;        /* THEN UP THE COUNT     @ZA01900*/
         LA    @10,1                                               0458
         AL    @10,LINKPRBS                                        0458
         ST    @10,LINKPRBS                                        0458
*   END;                            /* END RB COUNT LOOP     @ZA01900*/
@RF00457 DS    0H                                                  0459
@DE00455 TM    RBTCBNXT(RBSECPTR),B'10000000'                      0459
         BNO   @DL00455                                            0459
*   RBSECPTR=TCBRBP;                /* START AT TOP AGAIN    @ZA01900*/
*                                                                  0460
         L     RBSECPTR,TCBRBP(,TCBPTR)                            0460
*   /*****************************************************************/
*   /*                                                               */
*   /* FIGURE INDEX INTO RB CHAIN FOR THE PRB WE ARE         @ZA01900*/
*   /* LOOKING FOR. INPUT IS SANUM WHICH TELLS US THAT WE    @ZA01900*/
*   /* WANT THE ITH OLDEST PRB WITH IC OF 6 OR C, AND        @ZA01900*/
*   /* LINKPRBS WHICH TELLS US HOW MANY SUCH PRBS ARE IN TH@@ZA01900 */
*   /* RB CHAIN. SO WE REVERSE INDEX WITH SANUM TO FIND      @ZA01900*/
*   /* THE INDEX INTO THE RB CHAIN.                          @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0461
*   J=LINKPRBS-(SANUM-1);           /* FIGURE INDEX          @ZA01900*/
*                                                                  0461
         LA    @10,1                                               0461
         SH    @10,SANUM                                           0461
         AL    @10,LINKPRBS                                        0461
         ST    @10,J                                               0461
*   /*****************************************************************/
*   /*                                                               */
*   /* NOW LOOP OVER RBS TO FIND THE JTH ONE WITH IC OF 6    @ZA01900*/
*   /* OR C WHICH IS A PRB.                                  @ZA01900*/
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0462
*   I=1;                            /* SET INDEX             @ZA01900*/
         MVC   I(4),@CF00065                                       0462
*   IF RBFTP='000'B&(RBINTCOD='0006'X/* PRB & LINK           @ZA01900*/
*       |RBINTCOD='000C'X) THEN     /* OR PRB & SYNCH THEN   @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0463
         BNZ   @RF00463                                            0463
         LR    @10,RBSECPTR                                        0463
         SL    @10,@CF01199                              @UZ60132@ 0463
         CLC   RBINTCOD(2,@10),@CB02502                            0463
         BE    @RT00463                                            0463
         CLC   RBINTCOD(2,@10),@CB02503                            0463
         BNE   @RF00463                                            0463
@RT00463 DS    0H                                                  0464
*     IF I=J THEN                                                  0464
         L     @10,J                                               0464
         C     @10,I                                               0464
         BE    @RT00464                                            0464
*       GOTO LOOPOUT;               /* IF ONE WE WANT GETOUT @ZA01900*/
*     ELSE                                                         0466
*       J=J-1;                      /* ELSE DECRE. INDEX     @ZA01900*/
         BCTR  @10,0                                               0466
         ST    @10,J                                               0466
*   DO I=1 TO J WHILE(RBTCBNXT^='1'B);/*                     @ZA01900*/
@RF00463 LA    @10,1                                               0467
         B     @DE00467                                            0467
@DL00467 TM    RBTCBNXT(RBSECPTR),B'10000000'                      0467
         BO    @DC00467                                            0467
*     RBSECPTR=RBLINKB;             /* GET NEXT RB           @ZA01900*/
*        SLR   @10,@10                                   @UZ60132@ 0468
*        ICM   @10,7,RBLINKB(RBSECPTR)                   @UZ60132@ 0468
         L     @10,RBLINK(,RBSECPTR)                     @UZ60132@ 0468
         LA    @10,0(,@10)                               @UZ60132@
         LR    RBSECPTR,@10                                        0468
*     IF RBFTP^='000'B|(RBINTCOD^='0006'X/*                  @ZA01900*/
*         &RBINTCOD^='000C'X) THEN  /* IF NOT OURS THEN      @ZA01900*/
         TM    RBFTP(RBSECPTR),B'11100000'                         0469
         BNZ   @RT00469                                            0469
         LR    @10,RBSECPTR                                        0469
         SL    @10,@CF01199                              @UZ60132@ 0469
         CLC   RBINTCOD(2,@10),@CB02502                            0469
         BE    @RF00469                                            0469
         CLC   RBINTCOD(2,@10),@CB02503                            0469
         BE    @RF00469                                            0469
@RT00469 DS    0H                                                  0470
*       I=I-1;                      /* FORCE THROUGH LOOP    @ZA01900*/
         L     @10,I                                               0470
         BCTR  @10,0                                               0470
         ST    @10,I                                               0470
*     ELSE                                                         0471
*       IF I=J THEN                                                0471
         B     @RC00469                                            0471
@RF00469 CLC   I(4),J                                              0471
         BE    @RT00471                                            0471
*         GOTO LOOPOUT;             /* ELSE GET OUT          @ZA01900*/
*   END;                            /* END RBSEARCH LOOP     @ZA01900*/
@RC00469 LA    @10,1                                               0473
         AL    @10,I                                               0473
@DE00467 ST    @10,I                                               0473
         C     @10,J                                               0473
         BNH   @DL00467                                            0473
@DC00467 DS    0H                                                  0474
*LOOPOUT:                                                          0474
*   RETURN;                         /* RETURN TO CALLER      @ZA01900*/
@EL00004 DS    0H                                                  0474
@EF00004 DS    0H                                                  0474
@ER00004 LM    @14,@07,12(@13)                                     0474
         LM    @09,@12,56(@13)                                     0474
         BR    @14                                                 0474
*   END RBSEARCH;                   /* END SUBROUTINE        @ZA01900*/
         B     @EL00004                                            0475
*                                                                  0476
*   /*****************************************************************/
*   /*                                                               */
*   /* SUBROUTINE VALID VALIDITY CHECKS THE ADDRESS OF A POSSIBLE    */
*   /* SAVE AREA. AFTER DISABLING, IEA0VL00 IS CALLED TO CHECK THE   */
*   /* INITIAL AND LAST ADDRESSES OF THE SAVE AREA TO INSURE THAT THE*/
*   /* SPACE WOULD BE ADDRESSABLE BY THE USER. IF NOT, THE ROUTINE   */
*   /* RETURNS TO THE CALLER WITH A CODE OF EIGHT. THE ROUTINE THEN  */
*   /* CHECKS IF THE SAVE AREA FORWARD POINTER IS EQUAL TO THE SAVE  */
*   /* AREA BACK POINTER. IF SO, A CODE OF EIGHT IS RETURNED TO THE  */
*   /* CALLER. THEN, IF THE CALLER REQUESTED BACK CHAINING BE        */
*   /* CHECKED, THE ROUTINE CHECKS IF THE CURRENT SAVE AREA'S BACK   */
*   /* POINTER IS TO THE SAVE AREA POINTED TO BY LASTSA. IF NOT A    */
*   /* RETURN CODE OF FOUR IS PASSED TO THE CALLER. OTHERWISE A CODE */
*   /* OF ZERO IS GIVEN THE CALLER                                   */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0476
*VALID:                             /* START OF SUBROUTINE           */
*   PROCEDURE OPTIONS(NOSAVEAREA,   /* PROVIDE NO SAVE AREA          */
*       DONTSAVE);                  /* SAVE NO REGISTERS             */
VALID    DS    0H                                                  0477
*   VALIDR14=RETREG;                /* SAVE RETURN ADDRESS           */
         ST    RETREG,VALIDR14                                     0477
*   GENERATE REFS(PSALITA,ABDWORK);                                0478
*                                        /*                   @Y02705*/
         STM   11,14,ABDWORK(ABDARPTR)                          @Y02705
         SETLOCK OBTAIN,TYPE=LOCAL,MODE=UNCOND,                 @Y02705C
               RELATED=(SAVEAREA,IEAVAD07(INVALID))             @Y02705
         LM    11,14,ABDWORK(ABDARPTR)                          @Y02705
*   RFY                                                            0479
*    (TCBADDR,                                                     0479
*     INADDR,                                                      0479
*     SVWORK) RSTD;                 /*                       @YM05065*/
*   INADDR=CURSA;                   /* SET INPUT ADDR FOR 0VL00      */
         LR    INADDR,CURSA                                        0480
*   SVWORK=ABDARPTR;                /* SAVE ADDR OF WKAREA    @Y02705*/
         LR    SVWORK,ABDARPTR                                     0481
*   LSTADDR=0;                      /* LAST ADDR 0            @Y02705*/
         SLR   LSTADDR,LSTADDR                                     0482
*   TCBADDR=TCBPTR;                 /* SAVE TCB ADDR          @Y02705*/
         LR    TCBADDR,TCBPTR                                      0483
*   TCBPTR=0;                       /* LET RTN FIND TCB       @Y02705*/
         SLR   TCBPTR,TCBPTR                                       0484
*   VL00ADD=CVT0VL00;               /* SET ADDR OF ROUTINE           */
*                                                                  0485
         L     @14,CVTPTR                                          0485
         L     VL00ADD,CVT0VL00(,@14)                              0485
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE CALL TO IEA0VL00                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0486
*   GENERATE FLOWS(RET8);                                          0486
*                                      /*                     @Y02705*/
         BALR  14,15            BRNCH TO IEA0VL00               @Y02705
         BNZ   RET8             INVALID ADDRESS
*   INADDR=CURSA+71;                /* CALC ADDR LAST BYTE OF SAVE 0487
*                                      AREA                          */
         LA    INADDR,71                                           0487
         ALR   INADDR,CURSA                                        0487
*   TCBPTR=0;                       /* RESET TCB ADDR         @Y02705*/
         SLR   TCBPTR,TCBPTR                                       0488
*   VL00ADD=CVT0VL00;               /* SET ADDR OF ROUTINE           */
*                                                                  0489
         L     @14,CVTPTR                                          0489
         L     VL00ADD,CVT0VL00(,@14)                              0489
*   /*****************************************************************/
*   /*                                                               */
*   /* GENERATE CALL TO IEA0VL00                                     */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0490
*NVALID:                                                           0490
*   GENERATE FLOWS(RET8);                                          0490
NVALID   DS    0H                                                  0490
*                                      /*                     @Y02705*/
         BALR  14,15            BRNCH TO IEA0VL00               @Y02705
         BNZ   RET8             INVALID ADDRESS
*   ABDARPTR=SVWORK;                /* RESTORE WKAREA ADDR    @Y02705*/
         LR    ABDARPTR,SVWORK                                     0491
*   TCBPTR=TCBADDR;                 /* RESTORE TCB ADDR       @Y02705*/
         LR    TCBPTR,TCBADDR                                      0492
*   GENERATE;                                                      0493
*                                                                  0493
*                                      /*                     @Y02705*/
         STM   11,14,ABDWORK(ABDARPTR)                          @Y02705
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=(SAVEAREA,IEAVAD07(VALID))
         LM    11,14,ABDWORK(ABDARPTR)                          @Y02705
*   /*****************************************************************/
*   /*                                                               */
*   /* CHECK IF FORWARD AND BACK SAVE AREA PTRS ARE EQUAL            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0494
*   ABDUPRXT=ADDR(RET8A);           /* IF UPR THEN NOT ADDRSSABLE    */
         LA    @00,RET8A                                           0494
         ST    @00,ABDUPRXT(,ABDARPTR)                             0494
*   IF PREVSAVE^=NEXTSAVE           /* FORWARD AND BACK PTRS SAME    */
*     THEN                          /* NO                            */
         CLC   PREVSAVE(3,CURSA),NEXTSAVE(CURSA)                   0495
         BNE   @RT00495                                            0495
*     GOTO BACKCK;                  /* TEST OK CONTINUE CHECKS       */
*   ELSE                                                           0497
*     ;                             /* FAILED CHECK RC OF 8          */
*RET8A:                             /* CHECK FAIL WITH CODE OF 8     */
*   ABDARPTR=SVWORK;                /* RESTORE WKAREA ADDR    @Y02705*/
RET8A    LR    ABDARPTR,SVWORK                                     0498
*   TCBPTR=TCBADDR;                 /* RESTORE TCB ADDR       @Y02705*/
         LR    TCBPTR,TCBADDR                                      0499
*   RETCODE=8;                      /* SET RETURN CODE TO EIGHT      */
         LA    RETCODE,8                                           0500
*   RETREG=VALIDR14;                /* RESTORE RETURN ADDR           */
         L     RETREG,VALIDR14                                     0501
*   RETURN;                         /* BACK TO CALLER                */
@EL00005 DS    0H                                                  0502
@EF00005 DS    0H                                                  0502
@ER00005 BR    @14                                                 0502
*RET8:                              /*                        @Y02705*/
*   ABDARPTR=SVWORK;                /* RESTORE WKAREA ADDR    @Y02705*/
RET8     LR    ABDARPTR,SVWORK                                     0503
*   TCBPTR=TCBADDR;                 /* RESTORE TCB ADDR       @Y02705*/
         LR    TCBPTR,TCBADDR                                      0504
*   GENERATE REFS(PSALITA,ABDWORK);                                0505
*                                        /*                   @Y02705*/
         STM   11,14,ABDWORK(ABDARPTR)                          @Y02705
         SETLOCK RELEASE,TYPE=LOCAL,RELATED=(SAVEAREA,IEAVAD07(VALID))
         LM    11,14,ABDWORK(ABDARPTR)                          @Y02705
*   GOTO RET8A;                     /* EXIT TO CALLER CODE OF 8      */
*                                                                  0506
         B     RET8A                                               0506
*   /*****************************************************************/
*   /*                                                               */
*   /* IF USER DID NOT REQUEST BACK CHAINING TEST RETURN WITH CODE OF*/
*   /* ZERO                                                          */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0507
*BACKCK:                            /* MAKE LAST CHECK               */
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
BACKCK   SLR   @15,@15                                             0507
         ST    @15,ABDUPRXT(,ABDARPTR)                             0507
*   IF LASTSA=0                     /* NO PREVIOUS SA AREA GIVEN     */
*     THEN                          /* YES                           */
         L     @14,LASTSA                                          0508
         CR    @14,@15                                             0508
         BE    @RT00508                                            0508
*     GOTO NORM;                    /* START NORMAL EXIT             */
*   ELSE                                                           0510
*     ;                             /* MAKE BACK CHAIN CHECK         */
*                                                                  0510
*   /*****************************************************************/
*   /*                                                               */
*   /* MAKE CHECK TO INSURE THAT CURRENT SAVE AREA POINTS BACK TO    */
*   /* PREVIOUS SAVE AREA                                            */
*   /*                                                               */
*   /*****************************************************************/
*                                                                  0511
*   ABDUPRXT=ADDR(VALRC4);          /* SET RC TO 4 IF UPR HAPPENS    */
         LA    @00,VALRC4                                          0511
         ST    @00,ABDUPRXT(,ABDARPTR)                             0511
*   IF PREVSAVE=LASTSA              /* CURRENT POINT TO LAST         */
*     THEN                          /* YES                           */
*        SLR   @00,@00                                   @UZ60132@ 0512
         L     @00,4(,@03)                               @UZ60132@
         ICM   @00,8,@CB02502                            @UZ60132@ 0512
         CR    @14,@00                                   @UZ60132@ 0512
         BE    NORM                                                0512
*     GOTO NORM;                    /* START NORMAL EXIT             */
*   ELSE                                                           0514
*     ;                             /* RC 4                          */
*VALRC4:                            /* ENTERED IF UPR HAPPENS        */
*   RETCODE=4;                      /* OTHERWISE GIVE CODE           */
VALRC4   LA    RETCODE,4                                           0515
*   RETREG=VALIDR14;                /* RESTORE RETURN ADDR           */
         L     RETREG,VALIDR14                                     0516
*   RETURN;                         /* EXIT TO CALLER                */
         B     @EL00005                                            0517
*NORM:                              /* NORMAL EXIT                   */
*   ABDUPRXT=0;                     /* DISALLOW UPR                  */
NORM     SLR   @00,@00                                             0518
         ST    @00,ABDUPRXT(,ABDARPTR)                             0518
*   RETCODE=0;                      /* VALIDITY CHECKS ALL OK        */
         SLR   RETCODE,RETCODE                                     0519
*   RETREG=VALIDR14;                /* RESTORE RETURN ADDR           */
         L     RETREG,VALIDR14                                     0520
*   RFY                                                            0521
*    (TCBADDR,                                                     0521
*     INADDR,                                                      0521
*     SVWORK) UNRSTD;               /*                       @YM05065*/
*   END VALID;                      /* END OF SUBROUTINE             */
         B     @EL00005                                            0522
*   DCL                                                            0523
*     PATCHLOC FIXED(31) STATIC;                                   0523
*   GEN DATA DEFS(PATCHLOC);                                       0524
*   DECLARE                         /* COMMON VARIABLES              */
*     I256C CHAR(256) BASED,                                       0525
*     I031F FIXED(31) BASED,                                       0525
*     I031P PTR(31) BASED,                                         0525
*     I015F FIXED(15) BASED,                                       0525
*     I015P PTR(15) BASED,                                         0525
*     I008P PTR(8) BASED,                                          0525
*     I001C CHAR(1) BASED;          /*                        @Y02705*/
*   END IEAVAD07                    /* END OF MODULE                 */
*                                                                  0526
*/* THE FOLLOWING INCLUDE STATEMENTS WERE FOUND IN THIS PROGRAM.     */
*/*%INCLUDE SYSLIB  (IHAABDA )                                       */
*/*%INCLUDE SYSLIB  (CVT     )                                       */
*/*%INCLUDE SYSLIB  (IKJTCB  )                                       */
*/*%INCLUDE SYSLIB  (IHARB   )                                       */
*/*%INCLUDE SYSLIB  (IKJRB   )                                       */
*/*%INCLUDE SYSLIB  (IHACDE  )                                       */
*/*%INCLUDE SYSLIB  (IHALLE  )                                       */
*/*%INCLUDE SYSLIB  (IHAXTLST)                                       */
*/*%INCLUDE SYSLIB  (IHAPSA  )                                       */
*/*%INCLUDE SYSLIB  (IHARTCT )                                       */
*                                                                  0526
*       ;                                                          0526
         B     @EL00001                                            0526
@DATA    DS    0H
@CH00114 DC    H'2'
@CH02496 DC    XL2'4700'
MOVEID   MVC   DESC(1,ABDARPTR),5(1)   MOVE ID           @UZ60132@
@CF00065 DC    F'1'
@CH00070 DC    F'4'                                      @UZ60132@
@CH00123 DC    F'5'                                      @UZ60132@
@CH00072 DC    F'8'                                      @UZ60132@
@CF01199 DC    F'64'                                     @UZ60132@
@DATD    DSECT
         DS    0D
OPSWSAVE DS    A
LINKPRBS DS    A
I        DS    A
J        DS    A
LASTSA   DS    A
SAVERB   DS    A
SA15     DS    A
SA1RET   DS    A
VALIDR14 DS    A
TESTR14  DS    A
EXENDP   DS    A
EXSTARTP DS    A
EXEND    DS    A
SA14     DS    A
SANUM    DS    H
COUNT    DS    H
LNG      DS    AL1
LINCNT   DS    AL1
FRSTENT  DS    BL1
EPINMSG  DS    BL1
IEAVAD07 CSECT
         DS    0F
@SIZDATD DC    AL1(0)
         DC    AL3(@ENDDATD-@DATD)
@CV02411 DC    V(IEAVAD11)
@CV02466 DC    V(IEAVAD31)
@CV02432 DC    V(IEAVAD41)
         DS    0D
@CC02456 DC    C'UNKNOWN '
@CC02443 DC    C'AT EP'
@CC02494 DC    C'LINK'
@CB02440 DC    X'47F0'
@CB02493 DC    X'0A03'
@CB02496 DC    X'4700'
@CB02502 DC    X'0006'
@CB02503 DC    X'000C'
K253     DC    X'FD00'                 SUBPOOL NUMBER    @UZ60132@
SPKEY8   DC    X'80'                                     @UZ60132@
SAVEMSG1 DC    CL16'-SAVE AREA TRACE'
IBCMSG   DC    CL19'-INVALID BACK CHAIN'
PBMSG    DC    CL27'-PROCEEDING BACK VIA REG 13'
LLINE1   DS    CL15
         ORG   LLINE1
@NM00006 DC    X'B201'
@NM00007 DC    CL12'INTERRUPT AT'
@NM00008 DC    X'FF'
         ORG   LLINE1+15
SAHDLINE DS    CL5
         ORG   SAHDLINE
@NM00009 DC    X'1203'
@NM00010 DC    CL2'SA'
@NM00011 DC    X'FF'
         ORG   SAHDLINE+5
SALLINE1 DS    CL31
         ORG   SALLINE1
@NM00012 DC    X'0D'
@NM00013 DC    X'2301'
@NM00014 DC    CL3'WD1'
@NM00015 DC    X'2341'
@NM00016 DC    CL3'HSA'
@NM00017 DC    X'2341'
@NM00018 DC    CL3'LSA'
@NM00019 DC    X'2341'
@NM00020 DC    CL3'RET'
@NM00021 DC    X'2341'
@NM00022 DC    CL3'EPA'
@NM00023 DC    X'1342'
@NM00024 DC    CL2'R0'
@NM00025 DC    X'FF'
         ORG   SALLINE1+31
SALLINE2 DS    CL26
         ORG   SALLINE2
@NM00026 DC    X'0D'
@NM00027 DC    X'1302'
@NM00028 DC    CL2'R1'
@NM00029 DC    X'1342'
@NM00030 DC    CL2'R2'
@NM00031 DC    X'1342'
@NM00032 DC    CL2'R3'
@NM00033 DC    X'1342'
@NM00034 DC    CL2'R4'
@NM00035 DC    X'1342'
@NM00036 DC    CL2'R5'
@NM00037 DC    X'1342'
@NM00038 DC    CL2'R6'
@NM00039 DC    X'FF'
         ORG   SALLINE2+26
SALLINE3 DS    CL29
         ORG   SALLINE3
@NM00040 DC    X'0D'
@NM00041 DC    X'1302'
@NM00042 DC    CL2'R7'
@NM00043 DC    X'1342'
@NM00044 DC    CL2'R8'
@NM00045 DC    X'1342'
@NM00046 DC    CL2'R9'
@NM00047 DC    X'2341'
@NM00048 DC    CL3'R10'
@NM00049 DC    X'2341'
@NM00050 DC    CL3'R11'
@NM00051 DC    X'2341'
@NM00052 DC    CL3'R12'
@NM00053 DC    X'FF'
         ORG   SALLINE3+29
STANDMSG DC    CL20'WAS ENTERED VIA CALL'
IEAVAD07 CSECT
         DS   0H
PATCHLOC DC ((@DATA-@PSTART)/20)X'00'
@DATD    DSECT
         ORG   *+1-(*-@DATD)/(*-@DATD) INSURE DSECT DATA
@ENDDATD EQU   *
IEAVAD07 CSECT
@00      EQU   00                      EQUATES FOR REGISTERS 0-15
@01      EQU   01
@02      EQU   02
@03      EQU   03
@04      EQU   04
@05      EQU   05
@06      EQU   06
@07      EQU   07
@08      EQU   08
@09      EQU   09
@10      EQU   10
@11      EQU   11
@12      EQU   12
@13      EQU   13
@14      EQU   14
@15      EQU   15
PARMPTR  EQU   @01
INADDR   EQU   @01
ABDARPTR EQU   @02
LSTADDR  EQU   @02
CURSA    EQU   @03
TCBPTR   EQU   @04
NOENTSP  EQU   @05
NOENTS   EQU   @05
CDENTPTR EQU   @06
SAVERET  EQU   @06
LLEPTR   EQU   @07
RBSECPTR EQU   @08
XTLSTPTR EQU   @09
TCBADDR  EQU   @09
SVWORK   EQU   @10
AUTOPTR  EQU   @12
SAVEREG  EQU   @13
RETREG   EQU   @14
RETCODE  EQU   @15
REG15    EQU   @15
VL00ADD  EQU   @15
CVTPTR   EQU   16
PSA      EQU   0
FLCRNPSW EQU   PSA
FLCROPSW EQU   PSA+8
FLCICCW2 EQU   PSA+16
PSAEEPSW EQU   PSA+132
PSAESPSW EQU   PSA+136
FLCSVILC EQU   PSAESPSW+1
PSAEPPSW EQU   PSA+140
FLCPIILC EQU   PSAEPPSW+1
FLCPICOD EQU   PSAEPPSW+2
PSAPICOD EQU   FLCPICOD+1
FLCTEA   EQU   PSAEPPSW+4
FLCPER   EQU   PSA+152
FLCMCLA  EQU   PSA+168
FLCIOEL  EQU   FLCMCLA+4
FLCIOA   EQU   FLCMCLA+16
FLCFSA   EQU   FLCMCLA+80
PSASUPER EQU   PSA+552
PSASUP1  EQU   PSASUPER
PSASUP2  EQU   PSASUPER+1
PSASUP3  EQU   PSASUPER+2
PSASUP4  EQU   PSASUPER+3
PSACLHT  EQU   PSA+640
PSALKSA  EQU   PSA+696
PSAHLHI  EQU   PSA+760
PSALITA  EQU   PSA+764
PSADSSFL EQU   PSA+836
PSADSSF1 EQU   PSADSSFL
PSADSSF3 EQU   PSADSSFL+2
PSADSSF4 EQU   PSADSSFL+3
PSARSVT  EQU   PSA+896
EXITB    EQU   0
SAVEAREA EQU   0
PREVSAVE EQU   SAVEAREA+5
NEXTSAVE EQU   SAVEAREA+9
R14      EQU   SAVEAREA+12
R14A     EQU   R14+1
R15      EQU   SAVEAREA+16
R15A     EQU   R15+1
PROGRAM  EQU   0
EPI      EQU   PROGRAM
CNT      EQU   PROGRAM+4
EXSTARTA EQU   0
EXSTART  EQU   EXSTARTA+1
EXENDA   EQU   0
EXSIZE   EQU   EXENDA+1
PGM      EQU   0
EXITI    EQU   PGM
ABDAREA  EQU   0
ABDTCB   EQU   ABDAREA
ABDCTCB  EQU   ABDAREA+4
ABDPARMS EQU   ABDAREA+12
ABDFLAG  EQU   ABDPARMS+1
APFSDATA EQU   ABDPARMS+4
APFSDAT1 EQU   ABDPARMS+5
APFPDATA EQU   ABDPARMS+6
APFSAVE2 EQU   APFPDATA
ABDPTRS  EQU   ABDAREA+84
ABDUPRXT EQU   ABDAREA+112
ABDFLAG1 EQU   ABDAREA+136
ABDFLAG2 EQU   ABDAREA+137
ABDRES1  EQU   ABDAREA+139
ABDWORK  EQU   ABDAREA+140
ABDBPTR  EQU   ABDAREA+244
ABDLLINE EQU   ABDAREA+248
ABDLPTR  EQU   ABDAREA+252
ABDUPRF  EQU   ABDAREA+265
UPRFMAT  EQU   ABDUPRF
ABDUPRFN EQU   ABDAREA+266
ABDBLNKS EQU   ABDAREA+267
ABDBUFFS EQU   ABDAREA+308
ABDGMA   EQU   ABDAREA+316
ABDAAREA EQU   ABDGMA
ABDASIZE EQU   ABDGMA+4
ESPARM   EQU   ABDAREA+340
ESTAEFLG EQU   ESPARM+28
ABDLINEA EQU   ABDAREA+384
ABDCC    EQU   ABDLINEA
ABDLINE  EQU   ABDLINEA+1
CVTMAP   EQU   0
CVT0VL00 EQU   CVTMAP+24
CVTDAR   EQU   CVTMAP+72
CVTFLGS1 EQU   CVTDAR
CVTDCB   EQU   CVTMAP+116
CVTIERLC EQU   CVTMAP+144
CVTMZ00  EQU   CVTMAP+164
CVTOPTA  EQU   CVTMAP+182
CVTOPTB  EQU   CVTMAP+183
CVTGTF   EQU   CVTMAP+236
CVTGTFST EQU   CVTGTF
CVTGTFS  EQU   CVTGTFST
CVTSTATE EQU   CVTGTFST
CVTTMODE EQU   CVTGTFST
CVTFORM  EQU   CVTGTFST
CVTAQAVT EQU   CVTMAP+240
CVTTCMFG EQU   CVTAQAVT
CVTVOLM2 EQU   CVTMAP+244
CVTTATA  EQU   CVTVOLM2
CVTTSKS  EQU   CVTTATA
CVTVOLF2 EQU   CVTTSKS
CVTTAT   EQU   CVTTATA+1
CVTATER  EQU   CVTMAP+248
CVTEXT1  EQU   CVTMAP+252
CVTPURG  EQU   CVTMAP+260
CVTQMSG  EQU   CVTMAP+268
CVTDMSR  EQU   CVTMAP+272
CVTRSV37 EQU   CVTDMSR
CVTDMSRF EQU   CVTRSV37
CVTERPV  EQU   CVTMAP+316
CVTINTLA EQU   CVTMAP+320
CVTAPF   EQU   CVTMAP+324
CVTEXT2  EQU   CVTMAP+328
CVTHJES  EQU   CVTMAP+332
CVTPGSIA EQU   CVTMAP+348
CVTA1F1  EQU   CVTMAP+356
CVTSYSK  EQU   CVTMAP+357
CVTVOLM1 EQU   CVTMAP+380
CVTVOLF1 EQU   CVTVOLM1
CVTXTNT1 EQU   0
CVTXTNT2 EQU   0
CVTDSSV  EQU   CVTXTNT2
CVTFLGBT EQU   CVTXTNT2+5
CVTQID   EQU   CVTXTNT2+24
CVTRV400 EQU   CVTXTNT2+52
CVTRV409 EQU   CVTXTNT2+53
CVTATCVT EQU   CVTXTNT2+64
CVTRV429 EQU   CVTXTNT2+84
CVTRV438 EQU   CVTXTNT2+85
CVTRV457 EQU   CVTXTNT2+112
CVTRV466 EQU   CVTXTNT2+113
CVTFIX   EQU   0
CVTRELNO EQU   CVTFIX+252
TCBFIX   EQU   0
TCBFRS   EQU   TCBFIX
TCB      EQU   0
TCBRBP   EQU   TCB
TCBPIE   EQU   TCB+4
TCBPMASK EQU   TCBPIE
TCBCMP   EQU   TCB+16
TCBCMPF  EQU   TCBCMP
TCBCPP   EQU   TCBCMPF
TCBSTCC  EQU   TCBCMPF
TCBCDBL  EQU   TCBCMPF
TCBCWTO  EQU   TCBCMPF
TCBCIND  EQU   TCBCMPF
TCBCMSG  EQU   TCBCMPF
TCBTRN   EQU   TCB+20
TCBABF   EQU   TCBTRN
TCBMSS   EQU   TCB+24
TCBPKF   EQU   TCB+28
TCBFLGS  EQU   TCB+29
TCBFLGS1 EQU   TCBFLGS
TCBFLGS2 EQU   TCBFLGS+1
TCBFLGS3 EQU   TCBFLGS+2
TCBFLGS4 EQU   TCBFLGS+3
TCBFLGS5 EQU   TCBFLGS+4
TCBFC    EQU   TCBFLGS5
TCBABWF  EQU   TCBFLGS5
TCBLLS   EQU   TCB+36
TCBJPQ   EQU   TCB+44
TCBPURGE EQU   TCBJPQ
TCBGRS   EQU   TCB+48
TCBFSA   EQU   TCB+112
TCBFSAB  EQU   TCBFSA+1
TCBTME   EQU   TCB+120
TCBJSTCB EQU   TCB+124
TCBTSFLG EQU   TCB+148
TCBTSTSK EQU   TCBTSFLG
TCBSTAB  EQU   TCB+160
TCBNSTAE EQU   TCBSTAB
TCBTCT   EQU   TCB+164
TCBTCTGF EQU   TCBTCT
TCBSCNDY EQU   TCB+172
TCBNDSP  EQU   TCBSCNDY
TCBNDSP1 EQU   TCBNDSP+1
TCBNDSP2 EQU   TCBNDSP+2
TCBNDSP3 EQU   TCBNDSP+3
TCBJSCB  EQU   TCB+180
TCBRECDE EQU   TCBJSCB
TCBEXT1  EQU   TCB+196
TCBBITS  EQU   TCB+200
TCBNDSP4 EQU   TCBBITS
TCBNDSP5 EQU   TCBBITS+1
TCBFLGS6 EQU   TCBBITS+2
TCBFLGS7 EQU   TCBBITS+3
TCBDAR   EQU   TCB+204
TCBDARC  EQU   TCBDAR
TCBEXT2  EQU   TCB+208
TCBXSCT  EQU   TCB+240
TCBXSCT1 EQU   TCBXSCT
TCBFOE   EQU   TCB+244
TCBESTAE EQU   TCB+264
TCBESTRM EQU   TCBESTAE+1
TCBFBYT1 EQU   TCB+276
TCBFBYT2 EQU   TCB+277
TCBRV124 EQU   TCB+278
TCBRV133 EQU   TCB+279
TCBXTNT2 EQU   0
TCBGTF   EQU   TCBXTNT2
TCBTFLG  EQU   TCBGTF
TCBRV146 EQU   TCBXTNT2+28
TCBRV155 EQU   TCBXTNT2+29
TCBRV164 EQU   TCBXTNT2+30
TCBRV173 EQU   TCBXTNT2+31
RBPRFX   EQU   0
RBBASIC  EQU   0
RBEXRTNM EQU   RBBASIC
RBTMFLD  EQU   RBEXRTNM
RBSTAB   EQU   RBBASIC+10
XSTAB    EQU   RBSTAB
RBSTAB1  EQU   XSTAB
XSTAB1   EQU   RBSTAB1
RBFTCKPT EQU   XSTAB1
RBSTAB2  EQU   XSTAB+1
XSTAB2   EQU   RBSTAB2
RBTCBNXT EQU   XSTAB2
RBFACTV  EQU   XSTAB2
RBFDYN   EQU   XSTAB2
RBECBWT  EQU   XSTAB2
RBOPSW   EQU   RBBASIC+16
XRBPSW   EQU   RBOPSW
RBOPSWB2 EQU   XRBPSW+1
RBLINK   EQU   RBBASIC+28
XRBLNK   EQU   RBLINK
RBWCF    EQU   XRBLNK
RBLINKB  EQU   XRBLNK+1
RBGRSAVE EQU   RBBASIC+32
XRBREG   EQU   RBGRSAVE
RBGRS0   EQU   XRBREG
RBGRS1   EQU   XRBREG+4
RBGRS2   EQU   XRBREG+8
RBGRS3   EQU   XRBREG+12
RBGRS4   EQU   XRBREG+16
RBGRS5   EQU   XRBREG+20
RBGRS6   EQU   XRBREG+24
RBGRS7   EQU   XRBREG+28
RBGRS8   EQU   XRBREG+32
RBGRS9   EQU   XRBREG+36
RBGRS10  EQU   XRBREG+40
RBGRS11  EQU   XRBREG+44
RBGRS12  EQU   XRBREG+48
RBGRS13  EQU   XRBREG+52
RBGRS14  EQU   XRBREG+56
RBGRS15  EQU   XRBREG+60
XRBESA   EQU   RBBASIC+96
RBPREFIX EQU   0
RBRSV019 EQU   RBPREFIX+14
RBRSV028 EQU   RBPREFIX+15
RBRSV041 EQU   RBPREFIX+27
RBRSV054 EQU   RBPREFIX+39
RBRTOPSW EQU   RBPREFIX+40
RBRTPSW2 EQU   RBRTOPSW+8
RBRTICIL EQU   RBRTPSW2
RBFLAGS1 EQU   RBPREFIX+56
RBINTCDA EQU   RBPREFIX+61
RBINTCOD EQU   RBINTCDA+1
RBSECT   EQU   0
RBPPSAV  EQU   RBSECT
RBPPSAV1 EQU   RBPPSAV+1
@NM00107 EQU   RBSECT+10
@NM00108 EQU   @NM00107
RBFTP    EQU   @NM00108
RBTRSVRB EQU   @NM00108
@NM00110 EQU   @NM00107+1
RBETXR   EQU   @NM00110
RBEP     EQU   RBSECT+12
RBPGMQ   EQU   RBSECT+24
@NM00117 EQU   RBSECT+28
@NM00118 EQU   @NM00117
IRBEND   EQU   RBSECT+96
@NM00121 EQU   RBSECT+96
RBRSV138 EQU   @NM00121+7
RBSCBB   EQU   RBSECT+144
RBSPARM  EQU   RBSCBB+8
RBSFLGS1 EQU   RBSPARM
RBSIOPRC EQU   RBSFLGS1
RBSOWNR  EQU   RBSCBB+12
RBSFLGS2 EQU   RBSOWNR
RBSDATA  EQU   RBSCBB+16
RBSFLG3  EQU   RBSDATA
RBRSV150 EQU   RBSECT+167
CDENTRY  EQU   0
CDNAME   EQU   CDENTRY+8
CDXLMJP  EQU   CDENTRY+20
CDATTR   EQU   CDENTRY+28
CDNIC    EQU   CDATTR
CDMIN    EQU   CDATTR
CDATTR2  EQU   CDENTRY+29
CDXLE    EQU   CDATTR2
LLE      EQU   0
LLECHN   EQU   LLE
LLECDPT  EQU   LLE+4
XTLST    EQU   0
XTLNRFAC EQU   XTLST+4
XTLMSBLA EQU   XTLST+8
XTLMSBAA EQU   XTLST+12
RTCT     EQU   0
RTCTPLIB EQU   RTCT+4
RTCTSAP  EQU   RTCTPLIB
RTCTSAP1 EQU   RTCTSAP
RTCTSAP2 EQU   RTCTSAP+1
RTCTSAP3 EQU   RTCTSAP+2
RTCTSUP  EQU   RTCTPLIB+4
RTCTSUP1 EQU   RTCTSUP
RTCTSUP2 EQU   RTCTSUP+1
RTCTSUP3 EQU   RTCTSUP+2
RTCTSYD  EQU   RTCTPLIB+8
RTCTSY01 EQU   RTCTSYD
RTCTSDDS EQU   RTCT+36
RTCTFLG  EQU   RTCTSDDS+3
RTCTSDPL EQU   RTCT+156
RTCTRFLG EQU   RTCT+176
RTCTERID EQU   RTCT+224
RTCTOPT  EQU   RTCT+236
RTCTASO  EQU   RTCT+252
RTCTASO1 EQU   RTCTASO
RTCTASO2 EQU   RTCTASO+1
RTCTSDI  EQU   RTCT+254
RTCTSDF  EQU   RTCT+264
RTCTSDF1 EQU   RTCTSDF
RTCTSDF2 EQU   RTCTSDF+1
RTCTSDF3 EQU   RTCT+268
RTCTSDF4 EQU   RTCTSDF3+2
RTCTMOPT EQU   0
RTCTMSAO EQU   RTCTMOPT
RTCTSASD EQU   RTCTMSAO
RTCTSAO1 EQU   RTCTSASD
RTCTSAO2 EQU   RTCTSASD+1
RTCTSAPD EQU   RTCTMSAO+2
RTCTSAO3 EQU   RTCTSAPD
RTCTSAO4 EQU   RTCTSAPD+1
RTCTSAMG EQU   RTCTSAO4
RTCTMSUO EQU   RTCTMOPT+4
RTCTSUSD EQU   RTCTMSUO
RTCTSUO1 EQU   RTCTSUSD
RTCTSUO2 EQU   RTCTSUSD+1
RTCTSUPD EQU   RTCTMSUO+2
RTCTSUO3 EQU   RTCTSUPD
RTCTSUO4 EQU   RTCTSUPD+1
RTCTSUMG EQU   RTCTSUO4
RTCTMSYO EQU   RTCTMOPT+8
RTCTSD01 EQU   RTCTMSYO
RTCTSD04 EQU   RTCTMSYO+3
RTCTSMMG EQU   RTCTSD04
RTCTMSDO EQU   RTCTMOPT+12
RTCTSDOD EQU   RTCTMSDO
RTCTSDO1 EQU   RTCTSDOD
RTCTSDO2 EQU   RTCTSDOD+1
RTCTSDO3 EQU   RTCTMSDO+2
RTCTSDO4 EQU   RTCTMSDO+3
RTCTSDMG EQU   RTCTSDO4
I001C    EQU   0
I008P    EQU   0
I015F    EQU   0
I015P    EQU   0
I031F    EQU   0
I031P    EQU   0
I256C    EQU   0
HEADING  EQU   ABDLINE
MODNAME  EQU   HEADING
HEAD     EQU   HEADING+9
VIACALL  EQU   HEAD+12
CALLFLD  EQU   VIACALL+4
ATEPPART EQU   HEADING+38
DESC     EQU   HEADING+44
WORK     EQU   ABDGMA
IBC      EQU   ABDLINEA
PB       EQU   ABDLINEA
SAVE     EQU   ABDLINEA
CVTS01   EQU   CVTPGSIA
CVTLPDIA EQU   CVTS01+12
CVTDIRST EQU   CVTLPDIA
CVTSLIDA EQU   CVTS01+24
CVTCTLFG EQU   CVTS01+50
CVTRTMCT EQU   CVTS01+224
CVTRV210 EQU   CVTS01+424
CVTRV219 EQU   CVTS01+425
CVTRV228 EQU   CVTS01+426
CVTRV237 EQU   CVTS01+427
CVTMFRTR EQU   CVTS01+452
CVTRV262 EQU   CVTS01+468
CVTRV271 EQU   CVTS01+469
CVTRV280 EQU   CVTS01+470
CVTRV289 EQU   CVTS01+471
CVTGSDA  EQU   CVTS01+600
RBSECS03 EQU   RBEP
RBCDE    EQU   RBSECS03
RBCDFLGS EQU   RBCDE
RBCDE1   EQU   RBCDE+1
RBSECS05 EQU   RBPGMQ
RBSQE    EQU   RBSECS05
RBSECS06 EQU   RBPGMQ
RBIQE    EQU   RBSECS06
RBSECS07 EQU   RBPGMQ
RBIQE2   EQU   RBSECS07
RBSECS08 EQU   IRBEND
RBSECS09 EQU   IRBEND
PSARSVTE EQU   PSARSVT
FLC      EQU   PSA
         AGO   .@UNREFD                START UNREFERENCED COMPONENTS
PSARSAV  EQU   PSARSVTE+60
PSARSTK  EQU   PSARSVTE+56
PSAESAV3 EQU   PSARSVTE+52
PSAESTK3 EQU   PSARSVTE+48
PSAESAV2 EQU   PSARSVTE+44
PSAESTK2 EQU   PSARSVTE+40
PSAESAV1 EQU   PSARSVTE+36
PSAESTK1 EQU   PSARSVTE+32
PSAPSAV  EQU   PSARSVTE+28
PSAPSTK  EQU   PSARSVTE+24
PSAMSAV  EQU   PSARSVTE+20
PSAMSTK  EQU   PSARSVTE+16
PSASSAV  EQU   PSARSVTE+12
PSASSTK  EQU   PSARSVTE+8
PSANSTK  EQU   PSARSVTE+4
PSACSTK  EQU   PSARSVTE
RBRSV161 EQU   RBSECS09+64
RBSIRBWA EQU   RBSECS09
RBIQEWRK EQU   RBSECS08+4
RBNEXAV  EQU   RBSECS08
RBIQEA   EQU   RBIQE2
RBIQE1   EQU   RBIQE+1
RBUSE    EQU   RBIQE
RBSQEA   EQU   RBSQE+1
@NM00122 EQU   RBSQE
RBCDLOAD EQU   RBCDFLGS
RBCDXCTL EQU   RBCDFLGS
RBCDSYNC EQU   RBCDFLGS
RBCDNODE EQU   RBCDFLGS
RBRSV010 EQU   RBCDFLGS
RBCDATCH EQU   RBCDFLGS
RBRSV009 EQU   RBCDFLGS
RBNOCELL EQU   RBCDFLGS
CVTRV628 EQU   CVTS01+728
CVTRV627 EQU   CVTS01+724
CVTRV626 EQU   CVTS01+720
CVTRV625 EQU   CVTS01+716
CVTRV624 EQU   CVTS01+712
CVTRV623 EQU   CVTS01+708
CVTRV622 EQU   CVTS01+704
CVTRV621 EQU   CVTS01+700
CVTIHASU EQU   CVTS01+696
CVTSUSP  EQU   CVTS01+692
CVTT6SVC EQU   CVTS01+688
CVTCDAL  EQU   CVTS01+684
CVTTCTL  EQU   CVTS01+680
CVTRSUME EQU   CVTS01+676
CVTJTERM EQU   CVTS01+672
CVTASMRM EQU   CVTS01+668
CVTTCASP EQU   CVTS01+664
CVT0PT03 EQU   CVTS01+660
CVT0PT0E EQU   CVTS01+656
CVTRV609 EQU   CVTS01+652
CVTCGK   EQU   CVTS01+648
CVTRAC   EQU   CVTS01+644
CVTRV606 EQU   CVTS01+640
CVTRV605 EQU   CVTS01+636
CVTRV604 EQU   CVTS01+632
CVTEFF02 EQU   CVTS01+628
CVTCBBR  EQU   CVTS01+624
CVTSSCR  EQU   CVTS01+620
CVTEVENT EQU   CVTS01+616
CVTCRCA  EQU   CVTS01+612
CVTTPIO  EQU   CVTS01+608
CVTADV   EQU   CVTS01+604
CVTGSDAB EQU   CVTGSDA
CVTQV3   EQU   CVTS01+596
CVTQV2   EQU   CVTS01+592
CVTQV1   EQU   CVTS01+588
CVTRPT   EQU   CVTS01+584
CVTSSRB  EQU   CVTS01+580
CVTCSDRL EQU   CVTS01+576
CVTEXP1  EQU   CVTS01+572
CVTRMPMT EQU   CVTS01+568
CVTRMPTT EQU   CVTS01+564
CVTVPSA  EQU   CVTS01+560
CVTVSTOP EQU   CVTS01+556
CVTGTFR8 EQU   CVTS01+552
CVTQUIT  EQU   CVTS01+548
CVTVACR  EQU   CVTS01+544
CVTWTCB  EQU   CVTS01+540
CVTSTPRS EQU   CVTS01+536
CVT0PT02 EQU   CVTS01+532
CVTDARCM EQU   CVTS01+528
CVTIRECM EQU   CVTS01+524
CVTJRECM EQU   CVTS01+520
CVTVEMS0 EQU   CVTS01+516
CVTSPFRR EQU   CVTS01+512
CVTRLSTG EQU   CVTS01+508
CVT0TC0A EQU   CVTS01+504
CVTGMBR  EQU   CVTS01+500
CVTLFRM  EQU   CVTS01+496
CVTRMBR  EQU   CVTS01+492
CVTVIOP  EQU   CVTS01+488
CVTRV307 EQU   CVTS01+486
CVTRV306 EQU   CVTS01+484
CVTRV305 EQU   CVTS01+482
CVTRV304 EQU   CVTS01+480
CVTRV303 EQU   CVTS01+478
CVTRV302 EQU   CVTS01+476
CVTTRCA  EQU   CVTS01+472
CVTRV297 EQU   CVTRV289
CVTRV296 EQU   CVTRV289
CVTRV295 EQU   CVTRV289
CVTRV294 EQU   CVTRV289
CVTRV293 EQU   CVTRV289
CVTRV292 EQU   CVTRV289
CVTRV291 EQU   CVTRV289
CVTRV290 EQU   CVTRV289
CVTRV288 EQU   CVTRV280
CVTRV287 EQU   CVTRV280
CVTRV286 EQU   CVTRV280
CVTRV285 EQU   CVTRV280
CVTRV284 EQU   CVTRV280
CVTRV283 EQU   CVTRV280
CVTRV282 EQU   CVTRV280
CVTRV281 EQU   CVTRV280
CVTRV279 EQU   CVTRV271
CVTRV278 EQU   CVTRV271
CVTRV277 EQU   CVTRV271
CVTRV276 EQU   CVTRV271
CVTRV275 EQU   CVTRV271
CVTRV274 EQU   CVTRV271
CVTRV273 EQU   CVTRV271
CVTRV272 EQU   CVTRV271
CVTRV270 EQU   CVTRV262
CVTRV269 EQU   CVTRV262
CVTRV268 EQU   CVTRV262
CVTRV267 EQU   CVTRV262
CVTRV266 EQU   CVTRV262
CVTRV265 EQU   CVTRV262
CVTRV264 EQU   CVTRV262
CVTRV263 EQU   CVTRV262
CVTVFP   EQU   CVTS01+464
CVTVSI   EQU   CVTS01+460
CVTVPSIB EQU   CVTS01+456
CVTMFACT EQU   CVTMFRTR
CVTMFCTL EQU   CVTS01+448
CVTPVBP  EQU   CVTS01+444
CVTPWI   EQU   CVTS01+440
CVTRV254 EQU   CVTS01+438
CVTRV253 EQU   CVTS01+436
CVTRV252 EQU   CVTS01+434
CVTRV251 EQU   CVTS01+433
CVTRV250 EQU   CVTS01+432
CVTRV249 EQU   CVTS01+431
CVTRV248 EQU   CVTS01+430
CVTRV247 EQU   CVTS01+429
CVTRV246 EQU   CVTS01+428
CVTRV245 EQU   CVTRV237
CVTRV244 EQU   CVTRV237
CVTRV243 EQU   CVTRV237
CVTRV242 EQU   CVTRV237
CVTRV241 EQU   CVTRV237
CVTRV240 EQU   CVTRV237
CVTRV239 EQU   CVTRV237
CVTRV238 EQU   CVTRV237
CVTRV236 EQU   CVTRV228
CVTRV235 EQU   CVTRV228
CVTRV234 EQU   CVTRV228
CVTRV233 EQU   CVTRV228
CVTRV232 EQU   CVTRV228
CVTRV231 EQU   CVTRV228
CVTRV230 EQU   CVTRV228
CVTRV229 EQU   CVTRV228
CVTRV227 EQU   CVTRV219
CVTRV226 EQU   CVTRV219
CVTRV225 EQU   CVTRV219
CVTRV224 EQU   CVTRV219
CVTRV223 EQU   CVTRV219
CVTRV222 EQU   CVTRV219
CVTRV221 EQU   CVTRV219
CVTRV220 EQU   CVTRV219
CVTRV218 EQU   CVTRV210
CVTRV217 EQU   CVTRV210
CVTRV216 EQU   CVTRV210
CVTRV215 EQU   CVTRV210
CVTRV214 EQU   CVTRV210
CVTRV213 EQU   CVTRV210
CVTRV212 EQU   CVTRV210
CVTRV211 EQU   CVTRV210
CVTLCCAT EQU   CVTS01+420
CVTPCCAT EQU   CVTS01+416
CVTIPCRP EQU   CVTS01+412
CVTIPCRI EQU   CVTS01+408
CVTIPCDS EQU   CVTS01+404
CVTAIDVT EQU   CVTS01+400
CVTSSAP  EQU   CVTS01+396
CVTEHCIR EQU   CVTS01+392
CVTEHDEF EQU   CVTS01+388
CVTDAIR  EQU   CVTS01+384
CVTPERFM EQU   CVTS01+380
CVT044R2 EQU   CVTS01+376
CVTFETCH EQU   CVTS01+372
CVTRSTWD EQU   CVTS01+368
CVTSPOST EQU   CVTS01+364
CVTIOBP  EQU   CVTS01+360
CVTASMVT EQU   CVTS01+356
CVTRECRQ EQU   CVTS01+352
CVTWSAC  EQU   CVTS01+348
CVTRV149 EQU   CVTS01+344
CVTWSAL  EQU   CVTS01+340
CVTSPSA  EQU   CVTS01+336
CVTGLMN  EQU   CVTS01+332
CVTVEAC0 EQU   CVTS01+328
CVT062R1 EQU   CVTS01+324
CVTRPOST EQU   CVTS01+320
CVTDQIQE EQU   CVTS01+316
CVTCSD   EQU   CVTS01+312
CVTLKRMA EQU   CVTS01+308
CVTRSPIE EQU   CVTS01+304
CVTRENQ  EQU   CVTS01+300
CVTLQCB  EQU   CVTS01+296
CVTFQCB  EQU   CVTS01+292
CVTQCS01 EQU   CVTS01+288
CVTAPFT  EQU   CVTS01+284
CVTPARRL EQU   CVTS01+280
CVTVWAIT EQU   CVTS01+276
CVTGSPL  EQU   CVTS01+272
CVTLSMQ  EQU   CVTS01+268
CVTGSMQ  EQU   CVTS01+264
CVTEXPRO EQU   CVTS01+260
CVTOPCTP EQU   CVTS01+256
CVTSIC   EQU   CVTS01+252
CVTTPIOS EQU   CVTS01+248
CVTRTMS  EQU   CVTS01+244
CVTSDBF  EQU   CVTS01+240
CVTSCBP  EQU   CVTS01+236
CVTSDMP  EQU   CVTS01+232
CVTSV60  EQU   CVTS01+228
CVTASCBL EQU   CVTS01+220
CVTASCBH EQU   CVTS01+216
CVTGDA   EQU   CVTS01+212
CVTASVT  EQU   CVTS01+208
CVTVVMDI EQU   CVTS01+204
CVTAQTOP EQU   CVTS01+200
CVTIOSCS EQU   CVTS01+196
CVTSDRM  EQU   CVTS01+192
CVTOPTE  EQU   CVTS01+188
CVTSTXU  EQU   CVTS01+184
CVTQUIS  EQU   CVTS01+180
CVTPARS  EQU   CVTS01+176
CVTS1EE  EQU   CVTS01+172
CVTFRAS  EQU   CVTS01+168
CVTQSAS  EQU   CVTS01+164
CVTCRAS  EQU   CVTS01+160
CVTCRMN  EQU   CVTS01+156
CVTDELCP EQU   CVTS01+152
CVTFRECL EQU   CVTS01+148
CVTGETCL EQU   CVTS01+144
CVTBLDCP EQU   CVTS01+140
CVTAUTHL EQU   CVTS01+136
CVTSCAN  EQU   CVTS01+132
CVTRV144 EQU   CVTS01+130
CVTMAXMP EQU   CVTS01+128
CVTSTCK  EQU   CVTS01+124
CVTRV139 EQU   CVTS01+123
CVTDSSAC EQU   CVTS01+122
CVTRV513 EQU   CVTS01+121
CVTIOSPL EQU   CVTS01+120
CVTPTGT  EQU   CVTS01+116
CVTCSPIE EQU   CVTS01+112
CVTSMFEX EQU   CVTS01+108
CVTOLT0A EQU   CVTS01+104
CVTSRBRT EQU   CVTS01+100
CVTPUTL  EQU   CVTS01+96
CVTASCRL EQU   CVTS01+92
CVTASCRF EQU   CVTS01+88
CVTRV326 EQU   CVTS01+84
CVTRV325 EQU   CVTS01+80
CVTRV324 EQU   CVTS01+76
CVT0VL01 EQU   CVTS01+72
CVTSHRVM EQU   CVTS01+68
CVTRV332 EQU   CVTS01+64
CVTTAS   EQU   CVTS01+60
CVTRSCN  EQU   CVTS01+56
CVTTRAC2 EQU   CVTS01+54
CVTTRACE EQU   CVTS01+52
CVTAPG   EQU   CVTS01+51
CVTSDTRC EQU   CVTCTLFG
CVTGTRCE EQU   CVTCTLFG
CVTNOMP  EQU   CVTCTLFG
CVTRSV79 EQU   CVTCTLFG
CVTDSTAT EQU   CVTCTLFG
CVTRSV78 EQU   CVTCTLFG
CVTRV333 EQU   CVTCTLFG
CVTRV323 EQU   CVTCTLFG
CVTSPVLK EQU   CVTS01+49
CVTRSV77 EQU   CVTS01+48
CVTRV331 EQU   CVTS01+44
CVTRV330 EQU   CVTS01+40
CVTRV329 EQU   CVTS01+36
CVTRV328 EQU   CVTS01+32
CVTRV322 EQU   CVTS01+28
CVTSLID  EQU   CVTSLIDA+1
CVTSYLK  EQU   CVTSLIDA
CVTRV321 EQU   CVTS01+20
CVTRV320 EQU   CVTS01+16
CVTLPDIR EQU   CVTLPDIA+1
CVTRSV69 EQU   CVTDIRST
CVTRSV68 EQU   CVTDIRST
CVTRSV67 EQU   CVTDIRST
CVTRSV66 EQU   CVTDIRST
CVTRSV65 EQU   CVTDIRST
CVTRSV64 EQU   CVTDIRST
CVTRSV63 EQU   CVTDIRST
CVTDICOM EQU   CVTDIRST
CVTPVTP  EQU   CVTS01+8
CVTLPDSR EQU   CVTS01+4
CVTGETL  EQU   CVTS01
@NM00089 EQU   HEADING+114
@NM00088 EQU   HEADING+43
@NM00087 EQU   HEADING+29
@NM00086 EQU   VIACALL
@NM00085 EQU   HEAD
@NM00084 EQU   HEADING+8
RTCTISVC EQU   RTCTSDO4
RTCTSDOV EQU   RTCTSDMG
@NM00164 EQU   RTCTSDO4
@NM00163 EQU   RTCTSDO3
RTCTSDPH EQU   RTCTSDO3
RTCTSDPG EQU   RTCTSDO3
@NM00162 EQU   RTCTSDO2
RTCTSDPD EQU   RTCTSDO2
RTCTSDPC EQU   RTCTSDO2
RTCTSDPB EQU   RTCTSDO2
RTCTSDPA EQU   RTCTSDO2
RTCTSDP9 EQU   RTCTSDO2
RTCTSDP8 EQU   RTCTSDO2
RTCTSDP7 EQU   RTCTSDO1
RTCTSDP6 EQU   RTCTSDO1
RTCTSDP5 EQU   RTCTSDO1
RTCTSDP4 EQU   RTCTSDO1
RTCTSDP3 EQU   RTCTSDO1
RTCTSDP2 EQU   RTCTSDO1
RTCTSDP1 EQU   RTCTSDO1
RTCTSDP0 EQU   RTCTSDO1
RTCTISYM EQU   RTCTSD04
RTCTSMOV EQU   RTCTSMMG
@NM00161 EQU   RTCTSD04
RTCTSD03 EQU   RTCTMSYO+2
RTCTSD02 EQU   RTCTMSYO+1
RTCTSDS7 EQU   RTCTSD01
RTCTSDS6 EQU   RTCTSD01
RTCTSDS5 EQU   RTCTSD01
RTCTSDS4 EQU   RTCTSD01
RTCTSDS3 EQU   RTCTSD01
RTCTSDS2 EQU   RTCTSD01
RTCTSDS1 EQU   RTCTSD01
RTCTSDS0 EQU   RTCTSD01
RTCTISYU EQU   RTCTSUO4
RTCTSUOV EQU   RTCTSUMG
@NM00160 EQU   RTCTSUO4
RTCTSYDF EQU   RTCTSUO3
RTCTSYDE EQU   RTCTSUO3
RTCTSYDD EQU   RTCTSUO3
RTCTSYDC EQU   RTCTSUO3
RTCTSYDB EQU   RTCTSUO3
RTCTSYDA EQU   RTCTSUO3
RTCTSYD9 EQU   RTCTSUO3
RTCTSYD8 EQU   RTCTSUO3
@NM00159 EQU   RTCTSUO2
RTCTSYDH EQU   RTCTSUO2
RTCTSYDG EQU   RTCTSUO2
RTCTSYD7 EQU   RTCTSUO1
RTCTSYD6 EQU   RTCTSUO1
RTCTSYD5 EQU   RTCTSUO1
RTCTSYD4 EQU   RTCTSUO1
RTCTSYD3 EQU   RTCTSUO1
RTCTSYD2 EQU   RTCTSUO1
RTCTSYD1 EQU   RTCTSUO1
RTCTSYD0 EQU   RTCTSUO1
RTCTISAB EQU   RTCTSAO4
RTCTSAOV EQU   RTCTSAMG
@NM00158 EQU   RTCTSAO4
RTCTSADF EQU   RTCTSAO3
RTCTSADE EQU   RTCTSAO3
RTCTSADD EQU   RTCTSAO3
RTCTSADC EQU   RTCTSAO3
RTCTSADB EQU   RTCTSAO3
RTCTSADA EQU   RTCTSAO3
RTCTSAD9 EQU   RTCTSAO3
RTCTSAD8 EQU   RTCTSAO3
@NM00157 EQU   RTCTSAO2
RTCTSADH EQU   RTCTSAO2
RTCTSADG EQU   RTCTSAO2
RTCTSAD7 EQU   RTCTSAO1
RTCTSAD6 EQU   RTCTSAO1
RTCTSAD5 EQU   RTCTSAO1
RTCTSAD4 EQU   RTCTSAO1
RTCTSAD3 EQU   RTCTSAO1
RTCTSAD2 EQU   RTCTSAO1
RTCTSAD1 EQU   RTCTSAO1
RTCTSAD0 EQU   RTCTSAO1
RTCTZZZ5 EQU   RTCT+340
RTCTZZZ4 EQU   RTCT+336
RTCTMRMQ EQU   RTCT+332
RTCTSDF5 EQU   RTCTSDF3+3
RTCTSDDO EQU   RTCTSDF4
RTCTSDEN EQU   RTCTSDF4
RTCTSDEQ EQU   RTCTSDF4
RTCTSDTR EQU   RTCTSDF4
RTCTSDRM EQU   RTCTSDF4
RTCTSDAN EQU   RTCTSDF4
RTCTSDNC EQU   RTCTSDF4
RTCTSDSS EQU   RTCTSDF4
RTCTSDAS EQU   RTCTSDF3
RTCTZZZ3 EQU   RTCT+266
@NM00156 EQU   RTCTSDF2
RTCTSDRW EQU   RTCTSDF2
RTCTSDSL EQU   RTCTSDF2
RTCTSDWF EQU   RTCTSDF2
RTCTSDLA EQU   RTCTSDF2
RTCTSDDI EQU   RTCTSDF2
RTCTSDTQ EQU   RTCTSDF2
RTCTSDMR EQU   RTCTSDF2
RTCTSDSC EQU   RTCTSDF1
RTCTSDRS EQU   RTCTSDF1
RTCTSDSD EQU   RTCTSDF1
RTCTSDEP EQU   RTCTSDF1
RTCTSDMA EQU   RTCTSDF1
RTCTSDSH EQU   RTCTSDF1
RTCTSDND EQU   RTCTSDF1
RTCTSDNO EQU   RTCTSDF1
RTCTZZZ2 EQU   RTCT+257
RTCTSDPR EQU   RTCTSDI+2
RTCTINDX EQU   RTCTSDI+1
RTCTSDNA EQU   RTCTSDI
@NM00155 EQU   RTCTASO2
RTCTASNQ EQU   RTCTASO2
RTCTASNA EQU   RTCTASO2
RTCTASNS EQU   RTCTASO2
RTCTASSU EQU   RTCTASO2
RTCTASSW EQU   RTCTASO2
RTCTASCS EQU   RTCTASO2
RTCTASTR EQU   RTCTASO1
RTCTASLP EQU   RTCTASO1
RTCTASRG EQU   RTCTASO1
RTCTASLS EQU   RTCTASO1
RTCTASSQ EQU   RTCTASO1
RTCTASNU EQU   RTCTASO1
RTCTASPS EQU   RTCTASO1
RTCTASAL EQU   RTCTASO1
RTCTSDO  EQU   RTCTOPT+12
RTCTSYO  EQU   RTCTOPT+8
RTCTSUO  EQU   RTCTOPT+4
RTCTSAO  EQU   RTCTOPT
RTCTXXX2 EQU   RTCT+234
RTCTETIM EQU   RTCTERID+6
RTCTEASD EQU   RTCTERID+4
RTCTECPU EQU   RTCTERID+2
RTCTESEQ EQU   RTCTERID
RTCTSDWK EQU   RTCT+220
RTCTTDCB EQU   RTCT+184
RTCTSDSW EQU   RTCT+180
RTCTSEQ# EQU   RTCT+178
RTCTXXX1 EQU   RTCT+177
@NM00154 EQU   RTCTRFLG
RTCTRSTF EQU   RTCTRFLG
RTCTRPER EQU   RTCTRFLG
RTCTRTER EQU   RTCTRFLG
RTCTTEST EQU   RTCT+172
RTCTMSRB EQU   RTCT+168
RTCTMLCK EQU   RTCT+164
RTCTFMT  EQU   RTCT+160
RTCTSDIP EQU   RTCTSDPL
RTCTDEV  EQU   RTCTSDDS+8
RTCTDCB  EQU   RTCTSDDS+4
@NM00153 EQU   RTCTFLG
RTCTDETP EQU   RTCTFLG
RTCTDSUS EQU   RTCTFLG
RTCTDSST EQU   RTCTFLG
RTCTDSNM EQU   RTCTSDDS
RTCTRCB  EQU   RTCT+32
RTCTRECB EQU   RTCT+28
RTCTFASB EQU   RTCT+24
RTCTMECB EQU   RTCT+20
RTCTSDID EQU   RTCT+18
RTCTYYY1 EQU   RTCT+16
RTCTSY04 EQU   RTCTSYD+3
RTCTSY03 EQU   RTCTSYD+2
RTCTSY02 EQU   RTCTSYD+1
RTCTSYM7 EQU   RTCTSY01
RTCTSYM6 EQU   RTCTSY01
RTCTSYM5 EQU   RTCTSY01
RTCTSYM4 EQU   RTCTSY01
RTCTSYM3 EQU   RTCTSY01
RTCTSYM2 EQU   RTCTSY01
RTCTSYM1 EQU   RTCTSY01
RTCTSYM0 EQU   RTCTSY01
RTCTSUP4 EQU   RTCTSUP+3
RTCTSUDF EQU   RTCTSUP3
RTCTSUDE EQU   RTCTSUP3
RTCTSUDD EQU   RTCTSUP3
RTCTSUDC EQU   RTCTSUP3
RTCTSUDB EQU   RTCTSUP3
RTCTSUDA EQU   RTCTSUP3
RTCTSUD9 EQU   RTCTSUP3
RTCTSUD8 EQU   RTCTSUP3
@NM00152 EQU   RTCTSUP2
RTCTSUDH EQU   RTCTSUP2
RTCTSUDG EQU   RTCTSUP2
RTCTSUD7 EQU   RTCTSUP1
RTCTSUD6 EQU   RTCTSUP1
RTCTSUD5 EQU   RTCTSUP1
RTCTSUD4 EQU   RTCTSUP1
RTCTSUD3 EQU   RTCTSUP1
RTCTSUD2 EQU   RTCTSUP1
RTCTSUD1 EQU   RTCTSUP1
RTCTSUD0 EQU   RTCTSUP1
RTCTSAP4 EQU   RTCTSAP+3
RTCTSABF EQU   RTCTSAP3
RTCTSABE EQU   RTCTSAP3
RTCTSABD EQU   RTCTSAP3
RTCTSABC EQU   RTCTSAP3
RTCTSABB EQU   RTCTSAP3
RTCTSABA EQU   RTCTSAP3
RTCTSAB9 EQU   RTCTSAP3
RTCTSAB8 EQU   RTCTSAP3
@NM00151 EQU   RTCTSAP2
RTCTSABH EQU   RTCTSAP2
RTCTSABG EQU   RTCTSAP2
RTCTSAB7 EQU   RTCTSAP1
RTCTSAB6 EQU   RTCTSAP1
RTCTSAB5 EQU   RTCTSAP1
RTCTSAB4 EQU   RTCTSAP1
RTCTSAB3 EQU   RTCTSAP1
RTCTSAB2 EQU   RTCTSAP1
RTCTSAB1 EQU   RTCTSAP1
RTCTSAB0 EQU   RTCTSAP1
RTCTNAME EQU   RTCT
XTLMSBAD EQU   XTLMSBAA+1
@NM00126 EQU   XTLMSBAA
XTLMSBLN EQU   XTLMSBLA+1
@NM00125 EQU   XTLMSBLA
XTLLNTH  EQU   XTLST
LLESYSCT EQU   LLE+10
LLECOUNT EQU   LLE+8
CDATTR3  EQU   CDENTRY+30
CDAUTH   EQU   CDATTR2
CDSYSLIB EQU   CDATTR2
CDOLY    EQU   CDATTR2
@NM00124 EQU   CDATTR2
CDRLC    EQU   CDATTR2
CDREL    EQU   CDATTR2
CDSPZ    EQU   CDATTR2
CDNLR    EQU   CDATTR
CDJPA    EQU   CDATTR
CDNFN    EQU   CDATTR
CDSER    EQU   CDATTR
CDREN    EQU   CDATTR
CDNIP    EQU   CDATTR
@NM00123 EQU   CDENTRY+26
CDUSE    EQU   CDENTRY+24
CDENTPT  EQU   CDENTRY+16
CDRRBP   EQU   CDENTRY+4
CDCHAIN  EQU   CDENTRY
SVRBEND  EQU   RBSECT+192
RBFEPARM EQU   RBSECT+168
SIRBEND  EQU   RBSECT+168
RBRSV158 EQU   RBRSV150
RBRSV157 EQU   RBRSV150
RBRSV156 EQU   RBRSV150
RBRSV155 EQU   RBRSV150
RBRSV154 EQU   RBRSV150
RBRSV153 EQU   RBRSV150
RBRSV152 EQU   RBRSV150
RBRSV151 EQU   RBRSV150
RBRSV149 EQU   RBSECT+166
RBRSV148 EQU   RBSECT+164
RBRSV169 EQU   RBSDATA+3
RBSID    EQU   RBSDATA+2
RBSPKEY  EQU   RBSDATA+1
RBRSV168 EQU   RBSFLG3
RBSTERMO EQU   RBSFLG3
RBSBRNTR EQU   RBSFLG3
RBSPRNTR EQU   RBSFLG3
RBSCNCEL EQU   RBSFLG3
RBSRECRD EQU   RBSFLG3
RBSTERMI EQU   RBSFLG3
RBRSV167 EQU   RBSFLG3
RBSOWNRA EQU   RBSOWNR+1
RBSSUPER EQU   RBSFLGS2
RBSKEY0  EQU   RBSFLGS2
RBRSV166 EQU   RBSFLGS2
RBRSV165 EQU   RBSFLGS2
RBSINUSE EQU   RBSFLGS2
RBRSV164 EQU   RBSFLGS2
RBSXCTL2 EQU   RBSFLGS2
RBRSV163 EQU   RBSFLGS2
RBSPARMA EQU   RBSPARM+1
RBSHALT  EQU   RBSIOPRC
RBSNOIOP EQU   RBSIOPRC
RBSASYNC EQU   RBSFLGS1
RBRSV162 EQU   RBSFLGS1
RBSESTAE EQU   RBSFLGS1
RBSDUMMY EQU   RBSFLGS1
RBSSTAR  EQU   RBSFLGS1
RBSSTAI  EQU   RBSFLGS1
RBSEXIT  EQU   RBSCBB+4
RBSCHAIN EQU   RBSCBB
TIRBEND  EQU   @NM00121+8
PRBEND   EQU   @NM00121+8
RBRSV146 EQU   RBRSV138
RBRSV145 EQU   RBRSV138
RBRSV144 EQU   RBRSV138
RBRSV143 EQU   RBRSV138
RBRSV142 EQU   RBRSV138
RBRSV141 EQU   RBRSV138
RBRSV140 EQU   RBRSV138
RBRSV139 EQU   RBRSV138
RBRSV137 EQU   @NM00121+6
RBRSV136 EQU   @NM00121+4
RBRSV135 EQU   @NM00121
@NM00120 EQU   RBSECT+32
@NM00119 EQU   @NM00117+1
RBSCF    EQU   @NM00118
RBPGMQ1  EQU   RBPGMQ+1
@NM00116 EQU   RBPGMQ
@NM00115 EQU   RBSECT+16
@NM00114 EQU   @NM00110
@NM00113 EQU   @NM00110
RBIQETP  EQU   @NM00110
RBUSIQE  EQU   RBETXR
RBATTN   EQU   @NM00110
@NM00112 EQU   @NM00110
@NM00111 EQU   @NM00110
RBPMSVRB EQU   @NM00108
RBATNXIT EQU   @NM00108
@NM00109 EQU   @NM00108
RBWAITP  EQU   @NM00108
RBFNSVRB EQU   RBTRSVRB
RBSIZE   EQU   RBSECT+8
RBABOPSW EQU   RBSECT+4
@NM00106 EQU   RBPPSAV
RBPRFXND EQU   RBPREFIX+64
RBINLNTH EQU   RBINTCDA
RBWCSA   EQU   RBPREFIX+60
RBRSV004 EQU   RBPREFIX+57
RBSSSYN  EQU   RBFLAGS1
RBSCB    EQU   RBFLAGS1
RBLONGWT EQU   RBFLAGS1
RBASIR   EQU   RBFLAGS1
RBRSV159 EQU   RBFLAGS1
RBABEND  EQU   RBFLAGS1
RBXWAIT  EQU   RBFLAGS1
RBSLOCK  EQU   RBFLAGS1
RBRTRAN  EQU   RBRTPSW2+4
RBRTINCD EQU   RBRTICIL+2
RBRTILC  EQU   RBRTICIL+1
RBRSV160 EQU   RBRTICIL
RBRTPSW1 EQU   RBRTOPSW
RBRSV062 EQU   RBRSV054
RBRSV061 EQU   RBRSV054
RBRSV060 EQU   RBRSV054
RBRSV059 EQU   RBRSV054
RBRSV058 EQU   RBRSV054
RBRSV057 EQU   RBRSV054
RBRSV056 EQU   RBRSV054
RBRSV055 EQU   RBRSV054
RBRSV053 EQU   RBPREFIX+38
RBRSV052 EQU   RBPREFIX+36
RBRSV051 EQU   RBPREFIX+32
RBPRFXST EQU   RBPREFIX+32
RBRSV050 EQU   RBPREFIX+28
RBRSV049 EQU   RBRSV041
RBRSV048 EQU   RBRSV041
RBRSV047 EQU   RBRSV041
RBRSV046 EQU   RBRSV041
RBRSV045 EQU   RBRSV041
RBRSV044 EQU   RBRSV041
RBRSV043 EQU   RBRSV041
RBRSV042 EQU   RBRSV041
RBRSV040 EQU   RBPREFIX+26
RBRSV039 EQU   RBPREFIX+24
RBRSV038 EQU   RBPREFIX+20
RBRSV037 EQU   RBPREFIX+16
RBRSV036 EQU   RBRSV028
RBRSV035 EQU   RBRSV028
RBRSV034 EQU   RBRSV028
RBRSV033 EQU   RBRSV028
RBRSV032 EQU   RBRSV028
RBRSV031 EQU   RBRSV028
RBRSV030 EQU   RBRSV028
RBRSV029 EQU   RBRSV028
RBRSV027 EQU   RBRSV019
RBRSV026 EQU   RBRSV019
RBRSV025 EQU   RBRSV019
RBRSV024 EQU   RBRSV019
RBRSV023 EQU   RBRSV019
RBRSV022 EQU   RBRSV019
RBRSV021 EQU   RBRSV019
RBRSV020 EQU   RBRSV019
RBRSV018 EQU   RBPREFIX+13
RBRSV017 EQU   RBPREFIX+12
RBSRV016 EQU   RBPREFIX+11
RBRSV015 EQU   RBPREFIX+10
RBRSV014 EQU   RBPREFIX+8
RBRSV013 EQU   RBPREFIX+4
RBRSV012 EQU   RBPREFIX
RBEXSAVE EQU   XRBESA
XRBREG15 EQU   RBGRS15
XRBREG14 EQU   RBGRS14
XRBREG13 EQU   RBGRS13
XRBREG12 EQU   RBGRS12
XRBREG11 EQU   RBGRS11
XRBREG10 EQU   RBGRS10
XRBREG9  EQU   RBGRS9
XRBREG8  EQU   RBGRS8
XRBREG7  EQU   RBGRS7
XRBREG6  EQU   RBGRS6
XRBREG5  EQU   RBGRS5
XRBREG4  EQU   RBGRS4
XRBREG3  EQU   RBGRS3
XRBREG2  EQU   RBGRS2
XRBREG1  EQU   RBGRS1
XRBREG0  EQU   RBGRS0
XRBLNKA  EQU   RBLINKB
XRBWT    EQU   RBWCF
@NM00105 EQU   RBBASIC+24
RBOPSWPS EQU   RBOPSWB2
@NM00104 EQU   RBOPSWB2
@NM00103 EQU   XRBPSW
@NM00102 EQU   RBBASIC+12
XRBWAIT  EQU   RBECBWT
XRBFRRB  EQU   RBFDYN
@NM00101 EQU   XSTAB2
XRBACTV  EQU   RBFACTV
XRBTCBP  EQU   RBTCBNXT
@NM00100 EQU   XSTAB1
XRBCKPT  EQU   RBFTCKPT
@NM00099 EQU   XSTAB1
@NM00098 EQU   RBBASIC+8
@NM00097 EQU   RBEXRTNM+1
RBTMIND3 EQU   RBTMFLD
RBTMIND2 EQU   RBTMFLD
RBTMCMP  EQU   RBTMFLD
RBWLIM   EQU   RBTMFLD
RBRSV005 EQU   RBTMFLD
RBTMTOD  EQU   RBTMFLD
RBTMQUE  EQU   RBTMFLD
@NM00096 EQU   RBPRFX
@NM00095 EQU   TCBXTNT2+32
TCBRV181 EQU   TCBRV173
TCBRV180 EQU   TCBRV173
TCBRV179 EQU   TCBRV173
TCBRV178 EQU   TCBRV173
TCBRV177 EQU   TCBRV173
TCBRV176 EQU   TCBRV173
TCBRV175 EQU   TCBRV173
TCBRV174 EQU   TCBRV173
TCBRV172 EQU   TCBRV164
TCBRV171 EQU   TCBRV164
TCBRV170 EQU   TCBRV164
TCBRV169 EQU   TCBRV164
TCBRV168 EQU   TCBRV164
TCBRV167 EQU   TCBRV164
TCBRV166 EQU   TCBRV164
TCBRV165 EQU   TCBRV164
TCBRV163 EQU   TCBRV155
TCBRV162 EQU   TCBRV155
TCBRV161 EQU   TCBRV155
TCBRV160 EQU   TCBRV155
TCBRV159 EQU   TCBRV155
TCBRV158 EQU   TCBRV155
TCBRV157 EQU   TCBRV155
TCBRV156 EQU   TCBRV155
TCBRV154 EQU   TCBRV146
TCBRV153 EQU   TCBRV146
TCBRV152 EQU   TCBRV146
TCBRV151 EQU   TCBRV146
TCBRV150 EQU   TCBRV146
TCBRV149 EQU   TCBRV146
TCBRV148 EQU   TCBRV146
TCBRV147 EQU   TCBRV146
TCBRV145 EQU   TCBXTNT2+26
TCBRV144 EQU   TCBXTNT2+24
TCBRV143 EQU   TCBXTNT2+20
TCBRV142 EQU   TCBXTNT2+16
TCBRSV49 EQU   TCBXTNT2+12
TCBEVENT EQU   TCBXTNT2+8
TCBRCMP  EQU   TCBXTNT2+5
TCBRSVAB EQU   TCBXTNT2+4
TCBGTFA  EQU   TCBGTF+1
TCBRSV47 EQU   TCBTFLG
TCBRSV46 EQU   TCBTFLG
TCBRSV45 EQU   TCBTFLG
TCBRSV44 EQU   TCBTFLG
TCBRSV43 EQU   TCBTFLG
TCBDSPIT EQU   TCBTFLG
TCBERRTN EQU   TCBTFLG
TCBASYNC EQU   TCBTFLG
@NM00094 EQU   TCB+296
TCBSVCA2 EQU   TCB+292
TCBSWASA EQU   TCB+288
TCBDBTB  EQU   TCB+284
TCBRPT   EQU   TCB+280
TCBRV141 EQU   TCBRV133
TCBRV140 EQU   TCBRV133
TCBRV139 EQU   TCBRV133
TCBRV138 EQU   TCBRV133
TCBRV137 EQU   TCBRV133
TCBRV136 EQU   TCBRV133
TCBRV135 EQU   TCBRV133
TCBRV134 EQU   TCBRV133
TCBRV132 EQU   TCBRV124
TCBRV131 EQU   TCBRV124
TCBRV130 EQU   TCBRV124
TCBRV129 EQU   TCBRV124
TCBRV128 EQU   TCBRV124
TCBRV127 EQU   TCBRV124
TCBRV126 EQU   TCBRV124
TCBRV125 EQU   TCBRV124
TCBRV123 EQU   TCBFBYT2
TCBRV122 EQU   TCBFBYT2
TCBECBNV EQU   TCBFBYT2
TCBSSSYN EQU   TCBFBYT2
TCBFPRAP EQU   TCBFBYT2
TCBFDW   EQU   TCBFBYT2
TCBFMW   EQU   TCBFBYT2
TCBCNCB  EQU   TCBFBYT2
TCBLLH   EQU   TCBFBYT1
TCBRV113 EQU   TCBFBYT1
TCBEOT   EQU   TCBFBYT1
TCBRTM2  EQU   TCBFBYT1
TCBPGNLY EQU   TCBFBYT1
TCBNDIOS EQU   TCBFBYT1
TCBRV327 EQU   TCBFBYT1
TCBEOTFM EQU   TCBFBYT1
TCBAFFN  EQU   TCB+274
TCBRV326 EQU   TCB+272
TCBUKYSP EQU   TCB+268
TCBMODE  EQU   TCBESTAE+3
TCBERTYP EQU   TCBESTAE+2
TCBRV314 EQU   TCBESTRM
TCBRV313 EQU   TCBESTRM
TCBRV312 EQU   TCBESTRM
TCBRV311 EQU   TCBESTRM
TCBRV310 EQU   TCBESTRM
TCBRV309 EQU   TCBESTRM
TCBRV308 EQU   TCBESTRM
TCBETERM EQU   TCBESTRM
TCBSCBKY EQU   TCBESTAE
TCBRTM12 EQU   TCB+260
TCBTCBID EQU   TCB+256
TCBSTAWA EQU   TCB+252
TCBSWA   EQU   TCB+248
TCBFOEA  EQU   TCBFOE+1
TCBRSV42 EQU   TCBFOE
TCBCCPVI EQU   TCBXSCT+2
TCBRV325 EQU   TCBXSCT+1
TCBRV324 EQU   TCBXSCT1
TCBRV323 EQU   TCBXSCT1
TCBRV322 EQU   TCBXSCT1
TCBRV321 EQU   TCBXSCT1
TCBRV320 EQU   TCBXSCT1
TCBRV319 EQU   TCBXSCT1
TCBS3A   EQU   TCBXSCT1
TCBACTIV EQU   TCBXSCT1
TCBRSV41 EQU   TCB+239
TCBTID   EQU   TCB+238
TCBRSVAA EQU   TCB+237
TCBABCUR EQU   TCB+236
TCBTMSAV EQU   TCB+232
TCBIOTIM EQU   TCB+228
TCBRTWA  EQU   TCB+224
TCBBACK  EQU   TCB+220
TCBTIRB  EQU   TCB+216
TCBAECB  EQU   TCB+212
TCBEXT2A EQU   TCBEXT2+1
TCBRSV39 EQU   TCBEXT2
TCBSTMCT EQU   TCB+207
TCBSYSCT EQU   TCB+206
TCBRSV37 EQU   TCB+205
TCBEXSVC EQU   TCBDAR
TCBDARMS EQU   TCBDAR
TCBDARWT EQU   TCBDAR
TCBDARO  EQU   TCBDAR
TCBDARMC EQU   TCBDARC
TCBDARD  EQU   TCBDAR
TCBDARS  EQU   TCBDAR
TCBDARP  EQU   TCBDAR
TCBGTOFM EQU   TCBFLGS7
TCBADMP  EQU   TCBFLGS7
TCBRSTSK EQU   TCBFLGS7
TCBSVCS  EQU   TCBFLGS7
TCBSTACK EQU   TCBFLGS7
TCBRSV34 EQU   TCBFLGS7
TCBRSV33 EQU   TCBFLGS7
TCBGPECB EQU   TCBFLGS7
TCBNTJS  EQU   TCBFLGS6
TCBAPG   EQU   TCBFLGS6
TCBMIGR  EQU   TCBFLGS6
TCBRV303 EQU   TCBFLGS6
TCBSPVLK EQU   TCBFLGS6
TCBCPU   EQU   TCBFLGS6
TCBPIE17 EQU   TCBFLGS6
TCBRV    EQU   TCBFLGS6
TCBRSV79 EQU   TCBNDSP5
TCBRSV78 EQU   TCBNDSP5
TCBRSV77 EQU   TCBNDSP5
TCBRSV76 EQU   TCBNDSP5
TCBRSV75 EQU   TCBNDSP5
TCBRSV74 EQU   TCBNDSP5
TCBRSV95 EQU   TCBNDSP5
TCBRSV94 EQU   TCBNDSP5
TCBRSV93 EQU   TCBNDSP4
TCBRSV92 EQU   TCBNDSP4
TCBRSV91 EQU   TCBNDSP4
TCBRSV90 EQU   TCBNDSP4
TCBRSV89 EQU   TCBNDSP4
TCBRSV88 EQU   TCBNDSP4
TCBRSV87 EQU   TCBNDSP4
TCBRSV86 EQU   TCBNDSP4
TCBEXT1A EQU   TCBEXT1+1
TCBRSV32 EQU   TCBEXT1
TCBEXCPD EQU   TCB+192
TCBIOBRC EQU   TCB+188
TCBDDWTC EQU   TCB+186
TCBDDEXC EQU   TCB+184
TCBJSCBB EQU   TCBJSCB+1
@NM00093 EQU   TCBRECDE
TCBREC   EQU   TCBRECDE
TCBMDIDS EQU   TCB+176
TCBNDINT EQU   TCBNDSP3
TCBRSV27 EQU   TCBNDSP3
TCBRSV26 EQU   TCBNDSP3
TCBRSV25 EQU   TCBNDSP3
TCBRSV24 EQU   TCBNDSP3
TCBSRBND EQU   TCBNDSP3
TCBRV302 EQU   TCBNDSP3
TCBLJSND EQU   TCBNDSP3
TCBABE   EQU   TCBNDSP2
TCBDSS   EQU   TCBNDSP2
TCBOWAIT EQU   TCBNDSP2
TCBIWAIT EQU   TCBNDSP2
TCBNDTS  EQU   TCBNDSP2
TCBNDSVC EQU   TCBNDSP2
TCBSTPP  EQU   TCBNDSP2
TCBABD   EQU   TCBNDSP2
TCBRSV22 EQU   TCBNDSP1
TCBPIEND EQU   TCBNDSP1
TCBTPSP  EQU   TCBNDSP1
TCBDDRND EQU   TCBNDSP1
TCBRSPND EQU   TCBNDSP1
TCBRSTND EQU   TCBNDSP1
TCBDARPN EQU   TCBNDSP1
TCBDARTN EQU   TCBNDSP1
TCBNDSP0 EQU   TCBNDSP
TCBUSER  EQU   TCB+168
TCBTCTB  EQU   TCBTCT+1
TCBRSV9C EQU   TCBTCTGF
TCBRSV9B EQU   TCBTCTGF
TCBRSV9A EQU   TCBTCTGF
TCBRSV99 EQU   TCBTCTGF
TCBRSV98 EQU   TCBTCTGF
TCBRSV97 EQU   TCBTCTGF
TCBRSV20 EQU   TCBTCTGF
TCBSMFGF EQU   TCBTCTGF
TCBSTABB EQU   TCBSTAB+1
TCBSTCUR EQU   TCBNSTAE
TCBRV301 EQU   TCBNSTAE
TCBSYNCH EQU   TCBNSTAE
TCBHALT  EQU   TCBNSTAE
TCBPPSUP EQU   TCBNSTAE
TCB33E   EQU   TCBNSTAE
TCBQUIES EQU   TCBNSTAE
TCBSTABE EQU   TCBNSTAE
TCBAQE   EQU   TCB+156
TCBPQE   EQU   TCB+152
TCBTSDP  EQU   TCB+151
TCBTSLP  EQU   TCB+150
TCBSTPCT EQU   TCB+149
TCBCPUBN EQU   TCBTSFLG
TCBDYDSP EQU   TCBTSFLG
TCBRSV18 EQU   TCBTSFLG
TCBRSV17 EQU   TCBTSFLG
TCBTIOTG EQU   TCBTSFLG
TCBATT   EQU   TCBTSFLG
TCBSTPPR EQU   TCBTSFLG
TCBRV300 EQU   TCBTSTSK
TCBECB   EQU   TCB+144
TCBIQE   EQU   TCB+140
TCBLTC   EQU   TCB+136
TCBOTC   EQU   TCB+132
TCBNTC   EQU   TCB+128
TCBJSTCA EQU   TCBJSTCB+1
TCBRSV16 EQU   TCBJSTCB
TCBTQET  EQU   TCBTME
TCBTCB   EQU   TCB+116
TCBQEL   EQU   TCBFSA
TCBGRS15 EQU   TCBGRS+60
TCBGRS14 EQU   TCBGRS+56
TCBGRS13 EQU   TCBGRS+52
TCBGRS12 EQU   TCBGRS+48
TCBGRS11 EQU   TCBGRS+44
TCBGRS10 EQU   TCBGRS+40
TCBGRS9  EQU   TCBGRS+36
TCBGRS8  EQU   TCBGRS+32
TCBGRS7  EQU   TCBGRS+28
TCBGRS6  EQU   TCBGRS+24
TCBGRS5  EQU   TCBGRS+20
TCBGRS4  EQU   TCBGRS+16
TCBGRS3  EQU   TCBGRS+12
TCBGRS2  EQU   TCBGRS+8
TCBGRS1  EQU   TCBGRS+4
TCBGRS0  EQU   TCBGRS
TCBJPQB  EQU   TCBJPQ+1
TCBRSV15 EQU   TCBPURGE
TCBRSV14 EQU   TCBPURGE
TCBRSV13 EQU   TCBPURGE
TCBRSV12 EQU   TCBPURGE
TCBRSV11 EQU   TCBPURGE
TCBRSV10 EQU   TCBPURGE
TCBRSV09 EQU   TCBPURGE
TCBJPQF  EQU   TCBPURGE
TCBJLB   EQU   TCB+40
TCBDSP   EQU   TCB+35
TCBLMP   EQU   TCB+34
TCBPNDSP EQU   TCBFLGS5
TCBFCD1  EQU   TCBFLGS5
TCBSTP   EQU   TCBFLGS5
TCBSYS   EQU   TCBFLGS5
TCBANDSP EQU   TCBFLGS5
TCBPAGE  EQU   TCBFLGS5
TCBUXNDF EQU   TCBABWF
TCBONDSP EQU   TCBFLGS4
TCBMPCND EQU   TCBFLGS4
TCBMPCVQ EQU   TCBFLGS4
TCBUXNDV EQU   TCBFLGS4
TCBHNDSP EQU   TCBFLGS4
TCBRQENA EQU   TCBFLGS4
TCBSER   EQU   TCBFLGS4
TCBNDUMP EQU   TCBFLGS4
TCBDWSTA EQU   TCBFLGS3
TCBRSV08 EQU   TCBFLGS3
TCBRSV07 EQU   TCBFLGS3
TCBRSV06 EQU   TCBFLGS3
TCBABGM  EQU   TCBFLGS3
TCBABTRM EQU   TCBFLGS3
TCBADINP EQU   TCBFLGS3
TCBFSM   EQU   TCBFLGS3
TCBFTS   EQU   TCBFLGS2
TCBFETXR EQU   TCBFLGS2
TCBFDSOP EQU   TCBFLGS2
TCBFJMC  EQU   TCBFLGS2
TCBFSMC  EQU   TCBFLGS2
TCBFABOP EQU   TCBFLGS2
TCBFSTI  EQU   TCBFLGS2
TCBFOINP EQU   TCBFLGS2
TCBFX    EQU   TCBFLGS1
TCBFS    EQU   TCBFLGS1
TCBFT    EQU   TCBFLGS1
TCBPDUMP EQU   TCBFLGS1
TCBNONPR EQU   TCBFLGS1
TCBFERA  EQU   TCBFLGS1
TCBFE    EQU   TCBFLGS1
TCBFA    EQU   TCBFLGS1
TCBZERO  EQU   TCBPKF
TCBFLAG  EQU   TCBPKF
TCBMSSB  EQU   TCBMSS+1
TCBRSV03 EQU   TCBMSS
TCBTRNB  EQU   TCBTRN+1
TCBRSV02 EQU   TCBABF
TCBOLTEP EQU   TCBABF
TCBTCP   EQU   TCBABF
TCBTCPP  EQU   TCBABF
TCBRSV01 EQU   TCBABF
TCBGRPH  EQU   TCBABF
TCBNOCHK EQU   TCBABF
TCBMOD91 EQU   TCBABF
TCBCMPC  EQU   TCBCMP+1
TCBRV318 EQU   TCBCMSG
TCBRV317 EQU   TCBCIND
TCBRV316 EQU   TCBCWTO
TCBCASID EQU   TCBCDBL
TCBNOCC  EQU   TCBSTCC
TCBDMPO  EQU   TCBCPP
TCBCSTEP EQU   TCBCMPF
TCBCREQ  EQU   TCBCMPF
TCBTIO   EQU   TCB+12
TCBDEB   EQU   TCB+8
TCBPIEA  EQU   TCBPIE+1
TCBPM    EQU   TCBPMASK
@NM00092 EQU   TCBPMASK
TCBFRS6  EQU   TCBFRS+24
TCBFRS4  EQU   TCBFRS+16
TCBFRS2  EQU   TCBFRS+8
TCBFRS0  EQU   TCBFRS
CVTLEVL  EQU   CVTRELNO+2
CVTNUMB  EQU   CVTRELNO
CVTMDL   EQU   CVTFIX+250
@NM00091 EQU   CVTFIX+248
@NM00090 EQU   CVTFIX
CVTRV482 EQU   CVTXTNT2+128
CVTRV481 EQU   CVTXTNT2+124
CVTRV480 EQU   CVTXTNT2+120
CVTRV479 EQU   CVTXTNT2+118
CVTRV478 EQU   CVTXTNT2+117
CVTRV477 EQU   CVTXTNT2+116
CVTRV476 EQU   CVTXTNT2+115
CVTRV475 EQU   CVTXTNT2+114
CVTRV474 EQU   CVTRV466
CVTRV473 EQU   CVTRV466
CVTRV472 EQU   CVTRV466
CVTRV471 EQU   CVTRV466
CVTRV470 EQU   CVTRV466
CVTRV469 EQU   CVTRV466
CVTRV468 EQU   CVTRV466
CVTRV467 EQU   CVTRV466
CVTRV465 EQU   CVTRV457
CVTRV464 EQU   CVTRV457
CVTRV463 EQU   CVTRV457
CVTRV462 EQU   CVTRV457
CVTRV461 EQU   CVTRV457
CVTRV460 EQU   CVTRV457
CVTRV459 EQU   CVTRV457
CVTRV458 EQU   CVTRV457
CVTRV456 EQU   CVTXTNT2+108
CVTRV455 EQU   CVTXTNT2+104
CVTRV454 EQU   CVTXTNT2+100
CVTRV453 EQU   CVTXTNT2+96
CVTRV452 EQU   CVTXTNT2+94
CVTRV451 EQU   CVTXTNT2+92
CVTRV450 EQU   CVTXTNT2+90
CVTRV449 EQU   CVTXTNT2+88
CVTRV448 EQU   CVTXTNT2+87
CVTRV447 EQU   CVTXTNT2+86
CVTRV446 EQU   CVTRV438
CVTRV445 EQU   CVTRV438
CVTRV444 EQU   CVTRV438
CVTRV443 EQU   CVTRV438
CVTRV442 EQU   CVTRV438
CVTRV441 EQU   CVTRV438
CVTRV440 EQU   CVTRV438
CVTRV439 EQU   CVTRV438
CVTRV437 EQU   CVTRV429
CVTRV436 EQU   CVTRV429
CVTRV435 EQU   CVTRV429
CVTRV434 EQU   CVTRV429
CVTRV433 EQU   CVTRV429
CVTRV432 EQU   CVTRV429
CVTRV431 EQU   CVTRV429
CVTRV430 EQU   CVTRV429
CVTRV428 EQU   CVTXTNT2+80
CVTRV427 EQU   CVTXTNT2+76
CVTRV426 EQU   CVTXTNT2+72
CVTRV425 EQU   CVTXTNT2+68
CVTATACT EQU   CVTATCVT
CVTRV423 EQU   CVTXTNT2+62
CVTRV422 EQU   CVTXTNT2+60
CVTRV421 EQU   CVTXTNT2+58
CVTRV420 EQU   CVTXTNT2+56
CVTRV419 EQU   CVTXTNT2+55
CVTRV418 EQU   CVTXTNT2+54
CVTRV417 EQU   CVTRV409
CVTRV416 EQU   CVTRV409
CVTRV415 EQU   CVTRV409
CVTRV414 EQU   CVTRV409
CVTRV413 EQU   CVTRV409
CVTRV412 EQU   CVTRV409
CVTRV411 EQU   CVTRV409
CVTRV410 EQU   CVTRV409
CVTRV408 EQU   CVTRV400
CVTRV407 EQU   CVTRV400
CVTRV406 EQU   CVTRV400
CVTRV405 EQU   CVTRV400
CVTRV404 EQU   CVTRV400
CVTRV403 EQU   CVTRV400
CVTRV402 EQU   CVTRV400
CVTRV401 EQU   CVTRV400
CVTICB   EQU   CVTXTNT2+48
CVTSKTA  EQU   CVTXTNT2+44
CVTRSV99 EQU   CVTXTNT2+40
CVTRSV98 EQU   CVTXTNT2+36
CVTRSV97 EQU   CVTXTNT2+34
CVTRSV96 EQU   CVTXTNT2+32
CVTOLTEP EQU   CVTXTNT2+28
CVTQIDA  EQU   CVTQID+1
CVTRSV95 EQU   CVTQID
CVTRSV94 EQU   CVTXTNT2+20
CVTRSV93 EQU   CVTXTNT2+16
CVTRSV92 EQU   CVTXTNT2+12
CVTDEBVR EQU   CVTXTNT2+8
CVTRSV91 EQU   CVTXTNT2+6
CVTRSV9H EQU   CVTFLGBT
CVTRSV9G EQU   CVTFLGBT
CVTRSV9F EQU   CVTFLGBT
CVTRSV9E EQU   CVTFLGBT
CVTRSV9D EQU   CVTFLGBT
CVTBAH   EQU   CVTFLGBT
CVTVME   EQU   CVTFLGBT
CVTNPE   EQU   CVTFLGBT
CVTNUCLS EQU   CVTXTNT2+4
CVTDSSVA EQU   CVTDSSV+1
CVTRSV89 EQU   CVTDSSV
CVTRSV88 EQU   CVTXTNT1+8
CVTRSV87 EQU   CVTXTNT1+4
CVTFACHN EQU   CVTXTNT1
CVTRV488 EQU   CVTMAP+412
CVTRV487 EQU   CVTMAP+408
CVTRV486 EQU   CVTMAP+404
CVTRV485 EQU   CVTMAP+400
CVTACTAP EQU   CVTMAP+396
CVTAUTH  EQU   CVTMAP+392
CVTRV490 EQU   CVTMAP+388
CVTRSV61 EQU   CVTMAP+384
CVTVOLT1 EQU   CVTVOLM1+1
CVTVOLI1 EQU   CVTVOLF1
CVTSTOA  EQU   CVTMAP+376
CVTRSV58 EQU   CVTMAP+374
CVTRSV57 EQU   CVTMAP+372
CVTDDCE  EQU   CVTMAP+368
CVTPNWFR EQU   CVTMAP+364
CVTSMF   EQU   CVTMAP+360
CVTSULK  EQU   CVTMAP+358
CVTSLKO  EQU   CVTSYSK
CVTSLKP  EQU   CVTSYSK
CVTSLKQ  EQU   CVTSYSK
CVTSLKR  EQU   CVTSYSK
CVTRSV56 EQU   CVTSYSK
CVTRSV55 EQU   CVTSYSK
CVTRSV54 EQU   CVTSYSK
CVTRSV53 EQU   CVTSYSK
CVTRSV52 EQU   CVTA1F1
CVTRSV51 EQU   CVTA1F1
CVTRSV50 EQU   CVTA1F1
CVTRSV49 EQU   CVTA1F1
CVTRSV48 EQU   CVTA1F1
CVTRSV47 EQU   CVTA1F1
CVTSRSW  EQU   CVTA1F1
CVTPFSW  EQU   CVTA1F1
CVTPCVT  EQU   CVTMAP+352
CVTRSV46 EQU   CVTMAP+344
CVTRSV45 EQU   CVTMAP+340
CVTRSV44 EQU   CVTMAP+338
CVTRSV43 EQU   CVTMAP+336
CVTHJESA EQU   CVTHJES+1
CVTRSV42 EQU   CVTHJES
CVTEXT2A EQU   CVTEXT2+1
CVTRSV41 EQU   CVTEXT2
CVTAPFA  EQU   CVTAPF+1
CVTRSV40 EQU   CVTAPF
CVTRV518 EQU   CVTINTLA
CVTRV517 EQU   CVTERPV
CVTEORM  EQU   CVTMAP+312
CVTMCHPR EQU   CVTMAP+308
CVTTZ    EQU   CVTMAP+304
CVTJEPS  EQU   CVTMAP+300
CVTJESCT EQU   CVTMAP+296
CVTMODE  EQU   CVTMAP+292
CVTPTRV  EQU   CVTMAP+288
CVTREAL  EQU   CVTMAP+284
CVTRSV39 EQU   CVTMAP+280
CVTRSV38 EQU   CVTMAP+276
CVTDMSRA EQU   CVTDMSR+1
CVTRV634 EQU   CVTDMSRF
CVTRV633 EQU   CVTDMSRF
CVTRV632 EQU   CVTDMSRF
CVTRV631 EQU   CVTDMSRF
CVTRV630 EQU   CVTDMSRF
CVTRV629 EQU   CVTDMSRF
CVTUDUMP EQU   CVTDMSRF
CVTSDUMP EQU   CVTDMSRF
CVTQMSGA EQU   CVTQMSG+1
CVTRSV36 EQU   CVTQMSG
CVTAMFF  EQU   CVTMAP+264
CVTPURGA EQU   CVTPURG+1
CVTRSV35 EQU   CVTPURG
CVTCBSP  EQU   CVTMAP+256
CVTATERA EQU   CVTATER+1
CVTSYST  EQU   CVTATER
CVTVOLT2 EQU   CVTTAT
CVTVOLI2 EQU   CVTVOLF2
CVTAQAVB EQU   CVTAQAVT+1
CVTRSV34 EQU   CVTTCMFG
CVTRSV33 EQU   CVTTCMFG
CVTRSV32 EQU   CVTTCMFG
CVTRSV31 EQU   CVTTCMFG
CVTRSV30 EQU   CVTTCMFG
CVTRSV29 EQU   CVTTCMFG
CVTLDEV  EQU   CVTTCMFG
CVTTCRDY EQU   CVTTCMFG
CVTGTFA  EQU   CVTGTF+1
CVTRSV27 EQU   CVTGTFST
CVTRNIO  EQU   CVTGTFST
CVTUSR   EQU   CVTGTFST
CVTRV318 EQU   CVTFORM
CVTRV317 EQU   CVTTMODE
CVTRV316 EQU   CVTSTATE
CVTRV315 EQU   CVTGTFS
CVTGTFAV EQU   CVTGTFS
CVT0SCR1 EQU   CVTMAP+232
CVTRV515 EQU   CVTMAP+228
CVTRMS   EQU   CVTMAP+224
CVTPATCH EQU   CVTMAP+220
CVTTSCE  EQU   CVTMAP+216
CVTLNKSC EQU   CVTMAP+214
CVTQABST EQU   CVTMAP+212
CVTMDLDS EQU   CVTMAP+208
CVTUSER  EQU   CVTMAP+204
CVTABEND EQU   CVTMAP+200
CVTSMCA  EQU   CVTMAP+196
CVTRSV18 EQU   CVTMAP+192
CVTQLPAQ EQU   CVTMAP+188
CVTQCDSR EQU   CVTMAP+184
CVTRSV17 EQU   CVTOPTB
CVTVS1A  EQU   CVTOPTB
CVTFP    EQU   CVTOPTB
CVTAPTHR EQU   CVTOPTB
CVTNLOG  EQU   CVTOPTB
CVTTOD   EQU   CVTOPTB
CVTCTIMS EQU   CVTOPTB
CVTPROT  EQU   CVTOPTB
CVTXPFP  EQU   CVTOPTA
CVTASCII EQU   CVTOPTA
CVTRSV13 EQU   CVTOPTA
CVTRSV12 EQU   CVTOPTA
CVTNIP   EQU   CVTOPTA
CVTDDR   EQU   CVTOPTA
CVTAPR   EQU   CVTOPTA
CVTCCH   EQU   CVTOPTA
CVTSNCTR EQU   CVTMAP+180
CVTQMWR  EQU   CVTMAP+176
CVTQOCR  EQU   CVTMAP+172
CVT1EF00 EQU   CVTMAP+168
CVTHEAD  EQU   CVTMAP+160
CVTRSV11 EQU   CVTMAP+156
CVT0PT01 EQU   CVTMAP+152
CVTMSER  EQU   CVTMAP+148
CVTRV516 EQU   CVTIERLC
CVTILCH  EQU   CVTMAP+140
CVT0DS   EQU   CVTMAP+136
CVTFBOSV EQU   CVTMAP+132
CVTNUCB  EQU   CVTMAP+128
CVTIXAVL EQU   CVTMAP+124
CVTIOQET EQU   CVTMAP+120
CVTDCBA  EQU   CVTMAP+117
CVTMVS2  EQU   CVTDCB
CVT6DAT  EQU   CVTDCB
CVT4MPS  EQU   CVTDCB
CVTRSV09 EQU   CVTDCB
CVT4MS1  EQU   CVTDCB
CVT2SPS  EQU   CVTDCB
CVT1SSS  EQU   CVTDCB
CVTRSV08 EQU   CVTDCB
CVTSTB   EQU   CVTMAP+112
CVTQTD00 EQU   CVTMAP+108
CVTQTE00 EQU   CVTMAP+104
CVTCUCB  EQU   CVTMAP+100
CVTSJQ   EQU   CVTMAP+96
CVTPBLDL EQU   CVTMAP+92
CVTTPC   EQU   CVTMAP+88
CVTSVDCB EQU   CVTMAP+84
CVTBRET  EQU   CVTMAP+82
CVTEXIT  EQU   CVTMAP+80
CVT0FN00 EQU   CVTMAP+76
CVTDARA  EQU   CVTDAR+1
CVTRSV07 EQU   CVTFLGS1
CVTRSV06 EQU   CVTFLGS1
CVTRSV05 EQU   CVTFLGS1
CVTRSV04 EQU   CVTFLGS1
CVTRSV03 EQU   CVTFLGS1
CVTRSV02 EQU   CVTFLGS1
CVTRSV01 EQU   CVTFLGS1
CVTDMPLK EQU   CVTFLGS1
CVTXITP  EQU   CVTMAP+68
CVTZDTAB EQU   CVTMAP+64
CVTMSLT  EQU   CVTMAP+60
CVTDATE  EQU   CVTMAP+56
CVTBTERM EQU   CVTMAP+52
CVTSYSAD EQU   CVTMAP+48
CVTXTLER EQU   CVTMAP+44
CVTILK2  EQU   CVTMAP+40
CVTILK1  EQU   CVTMAP+36
CVTPRLTV EQU   CVTMAP+32
CVTPCNVT EQU   CVTMAP+28
CVTXAPG  EQU   CVTMAP+20
CVTBUF   EQU   CVTMAP+16
CVTJOB   EQU   CVTMAP+12
CVTLINK  EQU   CVTMAP+8
CVT0EF00 EQU   CVTMAP+4
CVTTCBP  EQU   CVTMAP
CVT      EQU   CVTMAP
ABDSAVE1 EQU   ABDAREA+672
ABDSAVHD EQU   ABDAREA+592
ABDPADC  EQU   ABDAREA+589
ABDSRC2  EQU   ABDAREA+382
ABDLOG   EQU   ABDAREA+380
ABDSRC1  EQU   ABDAREA+378
ABDPHY   EQU   ABDAREA+376
ABDLCNT  EQU   ABDAREA+372
@NM00083 EQU   ESPARM+29
@NM00082 EQU   ESTAEFLG
EDCB     EQU   ESTAEFLG
ETCB     EQU   ESTAEFLG
ESUBSYT  EQU   ESTAEFLG
ERESTART EQU   ESPARM+24
EMODNAME EQU   ESPARM+16
EABDAREA EQU   ESPARM+12
ESAUTOP  EQU   ESPARM+8
ESBSREG2 EQU   ESPARM+4
ESBSREG  EQU   ESPARM
ABDRTRN  EQU   ABDAREA+336
ABDSVCB  EQU   ABDAREA+332
@NM00081 EQU   ABDGMA+14
ABDCDID  EQU   ABDGMA+12
ABDPAREA EQU   ABDGMA+8
ABDBUFSZ EQU   ABDBUFFS+4
ABDBUFAD EQU   ABDBUFFS
ABSLCTSV EQU   ABDAREA+304
ABDSTADS EQU   ABDAREA+300
ABDUPRID EQU   ABDAREA+296
ABDPCAP  EQU   ABDAREA+295
ABDLLNGH EQU   ABDAREA+294
ABDINCPL EQU   ABDAREA+292
ABDFWORK EQU   ABDAREA+288
ABDIDENT EQU   ABDAREA+286
ABDIND   EQU   ABDAREA+284
ABDSIZE  EQU   ABDAREA+280
ABDLENTH EQU   ABDAREA+276
ABDBLOCK EQU   ABDAREA+272
ABDSTAD  EQU   ABDAREA+268
@NM00080 EQU   ABDBLNKS
@NM00079 EQU   ABDBLNKS
@NM00078 EQU   ABDBLNKS
@NM00077 EQU   ABDBLNKS
@NM00076 EQU   ABDBLNKS
@NM00075 EQU   ABDBLNKS
@NM00074 EQU   ABDBLNKS
ABDBLKN3 EQU   ABDBLNKS
@NM00073 EQU   ABDUPRFN
@NM00072 EQU   ABDUPRFN
@NM00071 EQU   ABDUPRFN
@NM00070 EQU   ABDUPRFN
@NM00069 EQU   ABDUPRFN
@NM00068 EQU   ABDUPRFN
ABDUPRSL EQU   ABDUPRFN
ABDUPRPM EQU   ABDUPRFN
@NM00067 EQU   ABDUPRF
@NM00066 EQU   ABDUPRF
@NM00065 EQU   ABDUPRF
@NM00064 EQU   ABDUPRF
@NM00063 EQU   ABDUPRF
UPRFMT20 EQU   ABDUPRF
UPRFMET  EQU   ABDUPRF
ABDFMTWK EQU   ABDAREA+256
ABDSSPAR EQU   ABDAREA+188
@NM00062 EQU   ABDRES1
ABDPGHD  EQU   ABDRES1
ABDFMTLD EQU   ABDRES1
ABDSQA   EQU   ABDRES1
ABDOLSQA EQU   ABDRES1
ABDREGS  EQU   ABDRES1
ABDPSW   EQU   ABDRES1
ABDKEY   EQU   ABDAREA+138
ABDLSQA  EQU   ABDFLAG2
ABDSTAT  EQU   ABDFLAG2
ABDSWAP  EQU   ABDFLAG2
ABDGTFCL EQU   ABDFLAG2
ABDTRNAV EQU   ABDFLAG2
ABDTRBIT EQU   ABDFLAG2
ABDGTF   EQU   ABDFLAG2
ABDSUPER EQU   ABDFLAG2
ABDIDX   EQU   ABDFLAG1
ABDSPHD  EQU   ABDFLAG1
ABDSVCHD EQU   ABDFLAG1
ABDSQSDM EQU   ABDFLAG1
ABDLSTHD EQU   ABDFLAG1
ABDQCBMN EQU   ABDFLAG1
ABDQCBMJ EQU   ABDFLAG1
ABDQCBHD EQU   ABDFLAG1
ABDPGWK  EQU   ABDAREA+128
ABDWORK1 EQU   ABDAREA+120
ABDRESV3 EQU   ABDAREA+116
ABDCP1   EQU   ABDAREA+108
ABDLP    EQU   ABDAREA+104
ABDFP    EQU   ABDAREA+100
ABDCP    EQU   ABDAREA+96
ABDPTRS3 EQU   ABDPTRS+8
ABDPTRS2 EQU   ABDPTRS+4
ABDPTRS1 EQU   ABDPTRS
ABDPCTR  EQU   ABDAREA+82
ABDLCTR  EQU   ABDAREA+80
ABDIOBP  EQU   ABDAREA+76
ABDPTR   EQU   ABDAREA+72
ABDDCB   EQU   ABDAREA+68
ABDLEN   EQU   ABDAREA+66
ABDTYPE  EQU   ABDAREA+64
ABDECB   EQU   ABDAREA+60
ABDSAVE  EQU   ABDAREA+56
ABDDECB  EQU   ABDAREA+52
ABDPARA  EQU   ABDAREA+48
ABDTABPT EQU   ABDAREA+44
ABDINXPT EQU   ABDAREA+40
ABDCTR   EQU   ABDAREA+38
ABDTRLN  EQU   ABDAREA+36
ABDHDRAD EQU   ABDPARMS+20
ABDSNAPP EQU   ABDPARMS+16
ABDPTCBP EQU   ABDPARMS+12
ABDPDCBP EQU   ABDPARMS+8
@NM00061 EQU   ABDPARMS+7
@NM00060 EQU   APFPDATA
APFSPALL EQU   APFPDATA
APFPSW   EQU   APFPDATA
APFJPA   EQU   APFPDATA
APFLPA   EQU   APFPDATA
APFREGS  EQU   APFPDATA
APFSAVE  EQU   APFPDATA
@NM00059 EQU   APFSDAT1
APFERR   EQU   APFSDAT1
APFIO    EQU   APFSDAT1
APFDM    EQU   APFSDATA
APFQCB   EQU   APFSDATA
APFSUPDA EQU   APFSDATA
APFTRACE EQU   APFSDATA
APFSWA   EQU   APFSDATA
APFLSQA  EQU   APFSDATA
APFSQA   EQU   APFSDATA
APFNUC   EQU   APFSDATA
@NM00058 EQU   ABDPARMS+2
APFHDR   EQU   ABDFLAG
APFSNAPP EQU   ABDFLAG
APFTCB   EQU   ABDFLAG
APFID    EQU   ABDFLAG
APFABEND EQU   ABDFLAG
APFENVS2 EQU   ABDFLAG
APFVS2   EQU   ABDFLAG
APFSNAP  EQU   ABDFLAG
ABDPID   EQU   ABDPARMS
ABDCRB   EQU   ABDAREA+8
@NM00057 EQU   PGM+2
@NM00056 EQU   EXENDA
@NM00055 EQU   EXSTARTA
@NM00054 EQU   PROGRAM+2
R12      EQU   SAVEAREA+68
R11      EQU   SAVEAREA+64
R10      EQU   SAVEAREA+60
R9       EQU   SAVEAREA+56
R8       EQU   SAVEAREA+52
R7       EQU   SAVEAREA+48
R6       EQU   SAVEAREA+44
R5       EQU   SAVEAREA+40
R4       EQU   SAVEAREA+36
R3       EQU   SAVEAREA+32
R2       EQU   SAVEAREA+28
R1       EQU   SAVEAREA+24
R0       EQU   SAVEAREA+20
@NM00005 EQU   R15
@NM00004 EQU   R14
@NM00003 EQU   SAVEAREA+8
@NM00002 EQU   SAVEAREA+4
@NM00001 EQU   SAVEAREA
@NM00150 EQU   PSA+3668
PSASTAK  EQU   PSA+3072
@NM00149 EQU   PSA+1048
PSAUSEND EQU   PSA+1048
PSARV062 EQU   PSA+1044
PSACDAL  EQU   PSA+1040
PSAWTCOD EQU   PSA+1036
PSAATCVT EQU   PSA+1032
PSAPCPSW EQU   PSA+1024
PSAPIR2  EQU   PSA+1020
PSARV059 EQU   PSA+1018
PSASVC13 EQU   PSA+1016
PSALSFCC EQU   PSA+1012
PSASFACC EQU   PSA+1008
PSASTOP  EQU   PSA+992
PSASTART EQU   PSA+976
PSARSPSW EQU   PSA+968
PSASRPSW EQU   PSA+960
PSARV045 EQU   PSA+892
PSARV044 EQU   PSA+888
PSARV043 EQU   PSA+884
PSARV042 EQU   PSA+880
PSARV041 EQU   PSA+876
PSARV040 EQU   PSA+872
PSARV025 EQU   PSA+868
PSADSSED EQU   PSA+868
PSADSSPR EQU   PSA+864
PSADSSFW EQU   PSA+860
PSADSS14 EQU   PSA+856
PSADSSPP EQU   PSA+848
PSADSSRP EQU   PSA+840
PSADSS05 EQU   PSADSSF4
PSADSS10 EQU   PSADSSF4
PSADSSVE EQU   PSADSSF4
PSADSSDE EQU   PSADSSF4
PSADSSC0 EQU   PSADSSF4
PSADSSIE EQU   PSADSSF4
PSADSS12 EQU   PSADSSF4
PSADSSRC EQU   PSADSSF4
PSARV057 EQU   PSADSSF3
PSARV056 EQU   PSADSSF3
PSARV055 EQU   PSADSSF3
PSADSSMC EQU   PSADSSF3
PSADSSRW EQU   PSADSSF3
PSADSSNM EQU   PSADSSF3
PSADSSES EQU   PSADSSF3
PSADSSGP EQU   PSADSSF3
PSADSSF2 EQU   PSADSSFL+1
PSADSSPI EQU   PSADSSF1
PSADSSOI EQU   PSADSSF1
PSADSSSP EQU   PSADSSF1
PSADSSTP EQU   PSADSSF1
PSADSSDW EQU   PSADSSF1
PSADSSDD EQU   PSADSSF1
PSADSSDM EQU   PSADSSF1
PSADSSMV EQU   PSADSSF1
PSADSSTS EQU   PSA+816
PSADSSWK EQU   PSA+812
PSADSSR3 EQU   PSA+808
PSADSSR2 EQU   PSA+804
PSADSSRS EQU   PSA+800
PSASTOR  EQU   PSA+796
PSACPUSA EQU   PSA+794
PSAVSTAP EQU   PSA+792
PSAWKVAP EQU   PSA+788
PSAWKRAP EQU   PSA+784
PSAMCHIC EQU   PSA+783
PSARV061 EQU   PSA+782
PSASYMSK EQU   PSA+781
PSAMCHFL EQU   PSA+780
PSACR0   EQU   PSA+776
PSAPSWSV EQU   PSA+768
PSACLHS  EQU   PSAHLHI
PSALKR15 EQU   PSALKSA+60
PSALKR14 EQU   PSALKSA+56
PSALKR13 EQU   PSALKSA+52
PSALKR12 EQU   PSALKSA+48
PSALKR11 EQU   PSALKSA+44
PSALKR10 EQU   PSALKSA+40
PSALKR9  EQU   PSALKSA+36
PSALKR8  EQU   PSALKSA+32
PSALKR7  EQU   PSALKSA+28
PSALKR6  EQU   PSALKSA+24
PSALKR5  EQU   PSALKSA+20
PSALKR4  EQU   PSALKSA+16
PSALKR3  EQU   PSALKSA+12
PSALKR2  EQU   PSALKSA+8
PSALKR1  EQU   PSALKSA+4
PSALKR0  EQU   PSALKSA
PSARV023 EQU   PSACLHT+52
PSALOCAL EQU   PSACLHT+48
PSACMSL  EQU   PSACLHT+44
PSAOPTL  EQU   PSACLHT+40
PSATPACL EQU   PSACLHT+36
PSATPDNL EQU   PSACLHT+32
PSATPNCL EQU   PSACLHT+28
PSAIOSLL EQU   PSACLHT+24
PSAIOSUL EQU   PSACLHT+20
PSAIOSCL EQU   PSACLHT+16
PSAIOSSL EQU   PSACLHT+12
PSASALCL EQU   PSACLHT+8
PSAASML  EQU   PSACLHT+4
PSADISPL EQU   PSACLHT
PSASRSA  EQU   PSA+636
PSARV050 EQU   PSA+634
PSADSSGO EQU   PSA+633
PSARECUR EQU   PSA+632
PSAHLHIS EQU   PSA+628
PSAIPCSA EQU   PSA+624
@NM00148 EQU   PSA+621
PSAIPCDM EQU   PSA+620
PSAIPCD  EQU   PSA+616
@NM00147 EQU   PSA+613
PSAIPCRM EQU   PSA+612
PSAIPCR  EQU   PSA+608
PSAMCHEX EQU   PSA+600
PSAMPSW  EQU   PSA+592
PSAEXPS2 EQU   PSA+584
PSAEXPS1 EQU   PSA+576
PSAPIREG EQU   PSA+572
PSARSREG EQU   PSA+568
PSAGPREG EQU   PSA+556
PSARV022 EQU   PSASUP4
PSARV021 EQU   PSASUP4
PSARV020 EQU   PSASUP4
PSARV019 EQU   PSASUP4
PSARV018 EQU   PSASUP4
PSARV017 EQU   PSASUP4
PSARV016 EQU   PSASUP4
PSARV015 EQU   PSASUP4
PSARV014 EQU   PSASUP3
PSARV013 EQU   PSASUP3
PSARV012 EQU   PSASUP3
PSAESTA  EQU   PSASUP3
PSASPR   EQU   PSASUP3
PSAPSREG EQU   PSASUP3
PSAPI2   EQU   PSASUP3
PSAIOSUP EQU   PSASUP3
PSALCR   EQU   PSASUP2
PSARTM   EQU   PSASUP2
PSAACR   EQU   PSASUP2
PSAIPCE2 EQU   PSASUP2
PSAIPCES EQU   PSASUP2
PSAIPCEC EQU   PSASUP2
PSAGTF   EQU   PSASUP2
PSAIPCRI EQU   PSASUP2
PSATYPE6 EQU   PSASUP1
PSATCTL  EQU   PSASUP1
PSADISP  EQU   PSASUP1
PSALOCK  EQU   PSASUP1
PSAPI    EQU   PSASUP1
PSAEXT   EQU   PSASUP1
PSASVC   EQU   PSASUP1
PSAIO    EQU   PSASUP1
PSAAOLD  EQU   PSA+548
PSAANEW  EQU   PSA+544
PSATOLD  EQU   PSA+540
PSATNEW  EQU   PSA+536
PSALCCAR EQU   PSA+532
PSALCCAV EQU   PSA+528
PSAPCCAR EQU   PSA+524
PSAPCCAV EQU   PSA+520
PSACPULA EQU   PSA+518
PSACPUPA EQU   PSA+516
PSAPSA   EQU   PSA+512
FLCHDEND EQU   PSA+512
FLCCRSAV EQU   FLCMCLA+280
FLCGRSAV EQU   FLCMCLA+216
FLCFPSAV EQU   FLCMCLA+184
FLCFLA   EQU   FLCMCLA+88
FLCRGNCD EQU   FLCMCLA+84
FLCFSAA  EQU   FLCFSA+1
@NM00146 EQU   FLCFSA
@NM00145 EQU   FLCMCLA+72
FLCMCIC  EQU   FLCMCLA+64
@NM00144 EQU   FLCMCLA+20
FLCIOAA  EQU   FLCIOA+1
@NM00143 EQU   FLCIOA
@NM00142 EQU   FLCMCLA+15
@NM00141 EQU   FLCMCLA+14
@NM00140 EQU   FLCMCLA+12
FLCLCL   EQU   FLCMCLA+8
FLCIOELA EQU   FLCIOEL+1
@NM00139 EQU   FLCIOEL
FLCCHNID EQU   FLCMCLA
@NM00138 EQU   PSA+160
FLCMTRCD EQU   PSA+157
@NM00137 EQU   PSA+156
FLCPERA  EQU   FLCPER+1
@NM00136 EQU   FLCPER
@NM00135 EQU   PSA+151
FLCPERCD EQU   PSA+150
FLCMCNUM EQU   PSA+149
@NM00134 EQU   PSA+148
FLCTEAA  EQU   FLCTEA+1
@NM00133 EQU   FLCTEA
PSAPIPC  EQU   PSAPICOD
PSAPIMC  EQU   PSAPICOD
PSAPIPER EQU   PSAPICOD
PSARV049 EQU   FLCPICOD
FLCPILCB EQU   FLCPIILC
@NM00132 EQU   FLCPIILC
@NM00131 EQU   PSAEPPSW
FLCSVCN  EQU   PSAESPSW+2
FLCSILCB EQU   FLCSVILC
@NM00130 EQU   FLCSVILC
@NM00129 EQU   PSAESPSW
FLCEICOD EQU   PSAEEPSW+2
PSASPAD  EQU   PSAEEPSW
@NM00128 EQU   PSA+128
FLCINPSW EQU   PSA+120
FLCMNPSW EQU   PSA+112
FLCPNPSW EQU   PSA+104
FLCSNPSW EQU   PSA+96
FLCENPSW EQU   PSA+88
FLCTRACE EQU   PSA+84
FLCTIMER EQU   PSA+80
FLCCVT2  EQU   PSA+76
FLCCAW   EQU   PSA+72
FLCCSW   EQU   PSA+64
FLCIOPSW EQU   PSA+56
FLCMOPSW EQU   PSA+48
FLCPOPSW EQU   PSA+40
FLCSOPSW EQU   PSA+32
FLCEOPSW EQU   PSA+24
@NM00127 EQU   FLCICCW2+4
FLCCVT   EQU   FLCICCW2
FLCICCW1 EQU   FLCROPSW
FLCIPPSW EQU   FLCRNPSW
.@UNREFD ANOP                          END UNREFERENCED COMPONENTS
@RT00129 EQU   DIE
@RT00132 EQU   AWAY
@RT00136 EQU   NEXTRB
@RT00140 EQU   NEXTRB
@RT00143 EQU   DIE
@RT00146 EQU   FORTRC
@RT00163 EQU   PRBCODE
@RT00167 EQU   BADBACK
@RT00170 EQU   PRBCODE
@RT00174 EQU   DIE
@RT00177 EQU   PRBCODE
@RT00188 EQU   DIE
@RT00192 EQU   DIE
@RT00199 EQU   AWAY
@RT00212 EQU   DIE
@RT00218 EQU   DIE
@RT00221 EQU   AWAY
@RT00228 EQU   AWAY
@RT00237 EQU   DIE
@RT00243 EQU   DIE
@RT00246 EQU   AWAY
@RT00252 EQU   AWAY
@RF00252 EQU   PRBLOOP
@RT00261 EQU   SA1EXITA
@RT00265 EQU   MSGDONE
@RT00270 EQU   MSGDONE1
@RT00284 EQU   TERMINAL
@RT00287 EQU   CKHEAD
@RT00290 EQU   TRYLLE
@RF00302 EQU   SA1EXITA
@RT00312 EQU   TERMINAL
@RT00315 EQU   CKHEAD
@RT00334 EQU   TERMINAL
@RT00343 EQU   TERMINAL
@RT00348 EQU   TERMINAL
@RT00352 EQU   SAL2
@RT00355 EQU   SAL3
@RT00375 EQU   RC12
@RT00379 EQU   TEST2A
@RT00388 EQU   RC12
@RT00398 EQU   RC12
@RT00402 EQU   FOUND
@RF00431 EQU   @RC00420
@RT00438 EQU   WRITE
LOOPOUT  EQU   @EL00004
@RT00495 EQU   BACKCK
@RT00508 EQU   NORM
@RT00512 EQU   NORM
@RT00464 EQU   LOOPOUT
@RT00471 EQU   LOOPOUT
@ENDDATA EQU   *
         END   IEAVAD07,(C'PLS1709',0802,82300)
