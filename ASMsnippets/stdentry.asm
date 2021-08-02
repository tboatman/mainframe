*
* STANDARD ENTRY
*
MAIN     CSECT
         STM   14,12,12(13)
         BASR  12,0
         USING *,12
         ST    13,SAVE+4
         LA    13,SAVE
*
* STANDARD EXIT
*
         L     13,SAVE+4
         LM    14,12,12(13)
         LA    15,0
         BR    14
SAVE     DC    18F'0'
         END   MAIN