//SMPJOB08 JOB 'GENERATE ICKDSF',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)      00000100
//*                                                                     00000200
//********************************************************************* 00000300
//*                                                                     00000400
//*         MVS 3.8 SYSGEN                                              00000500
//*                                                                     00000600
//* ICKDSF IS NOT PRESENT IN THE STARTER SYSTEM, BUT IS REQUIRED TO     00000700
//* PLACE THE IPL RECORDS ONTO THE VOLUME WHICH WILL HOLD THE TARGET    00000800
//* MVS 3.8 SYSTEM. THIS JOBSTREAM UTILIZES THE SYSGEN MACROS INSTALLED 00000900
//* DURING THE BUILDING OF THE DISTRIBUTION LIBRARIES TO LINK EDIT      00001000
//* ICKDSF FROM THE DISTRIBUTION LIBRARIES INTO SYS1.LINKLIB ON         00001100
//* THE STARTER SYSTEM VOLUME - START1.                                 00001200
//*                                                                     00001300
//*   !!!  AN IPL IS REQUIRED AFTER THIS JOB COMPLETES  !!!             00001400
//*                                                                     00001500
//********************************************************************* 00001600
//*                                                                     00001700
/*MESSAGE  ************************************************************ 00001800
/*MESSAGE  * AN IPL IS REQUIRED AFTER THIS JOB HAS COMPLETED!!!       * 00001900
/*MESSAGE  ************************************************************ 00002000
//*                                                                     00002100
//********************************************************************* 00002200
//* USE SGIEH404 TO GENERATE LINK EDIT STATEMENTS FOR ICKDSF            00002300
//********************************************************************* 00002400
//*                                                                     00002500
//ASM      EXEC PGM=IEUASM,REGION=1024K,PARM='LIST,NOOBJECT,DECK'       00002600
//SYSLIB   DD  DSN=SYS1.AGENLIB,DISP=SHR,UNIT=SYSDA,VOL=SER=SMP000      00002700
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        00002800
//SYSUT2   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        00002900
//SYSUT3   DD  UNIT=SYSDA,SPACE=(1700,(1400,50))                        00003000
//SYSPRINT DD  SYSOUT=A                                                 00003100
//SYSPUNCH DD  UNIT=SYSDA,DISP=(,PASS),SPACE=(TRK,15),DCB=BLKSIZE=80    00003200
//SYSIN    DD  *                                                        00003300
         PRINT OFF                                                      00003400
         SGIEH404                                                       00003500
         END                                                            00003600
//*                                                                     00003700
//********************************************************************* 00003800
//* BUILD JOBSTREAM TO LINK EDIT IFOX00 and ICKDSF                      00003900
//********************************************************************* 00004000
//*                                                                     00004100
//IDCAMS   EXEC PGM=IDCAMS,REGION=4096K,COND=(0,NE,ASM)                 00004200
//SYSPRINT DD  DUMMY                                                    00004300
//DDIN1    DD  DATA,DLM='><'                                            00004400
//SMPJOB09 JOB 'LINK IFOX00/ICKDSF',CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1)   00004500
//* ***************************************************************** * 00004600
//* Re-link IFOX00 from the Distribution Libraries                    * 00004700
//* ***************************************************************** * 00004800
//LINK    EXEC LINKS,PARM=(NCAL,LIST,XREF),                             00004900
//             CLASS=A,OBJ=,UNIT=,SER=,N=' ',NAME=,P1=,MOD=,P2=         00005000
//SYSPUNCH DD  DUMMY                                                    00005100
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                00005200
//AOS03    DD  DISP=SHR,VOL=(,RETAIN),DSN=SYS1.AOS03                    00005300
//SYSLIN   DD  *                                                        00005400
  INCLUDE  AOS03(IFOX0A,IFOX0B)                                         00005500
  ENTRY  IFOX0A01                                                       00005600
  ALIAS  ASMBLR,IEUASM                                                  00005700
  NAME  IFOX00(R)                                                       00005800
  INCLUDE  AOS03(IFOX0C)                                                00005900
  ENTRY IFOX0C01                                                        00006000
  NAME  IFOX01(R)                                                       00006100
  INCLUDE  AOS03(IFOX0D,IFOX0J)                                         00006200
  ENTRY IFOX0D01                                                        00006300
  NAME  IFOX02(R)                                                       00006400
  INCLUDE  AOS03(IFOX0E)                                                00006500
  ENTRY  IFOX0E01                                                       00006600
  NAME  IFOX03(R)                                                       00006700
  INCLUDE  AOS03(IFOX0F)                                                00006800
  ENTRY IFOX0F01                                                        00006900
  NAME  IFOX04(R)                                                       00007000
  INCLUDE  AOS03(IFOX0G)                                                00007100
  ENTRY IFOX0G01                                                        00007200
  NAME  IFOX05(R)                                                       00007300
  INCLUDE  AOS03(IFOX0H)                                                00007400
  ENTRY IFOX0H01                                                        00007500
  NAME  IFOX06(R)                                                       00007600
  INCLUDE  AOS03(IFOX0I)                                                00007700
  ENTRY IFOX0I01                                                        00007800
  NAME  IFOX07(R)                                                       00007900
  INCLUDE  AOS03(IFNX1A,IFNX1J,IFNX1K,IFNX1S)                           00008000
  ENTRY  IFNX1A01                                                       00008100
  NAME  IFOX11(R)                                                       00008200
  INCLUDE  AOS03(IFNX2A)                                                00008300
  ENTRY IFNX2A01                                                        00008400
  NAME  IFOX21(R)                                                       00008500
  INCLUDE  AOS03(IFNX3A,IFNX3B,IFNX3K,IFNX3N)                           00008600
  ENTRY  IFNX3A01                                                       00008700
  NAME  IFOX31(R)                                                       00008800
  INCLUDE  AOS03(IFNX4D,IFNX4E,IFNX4M,IFNX4S,IFNX4V)                    00008900
  ENTRY  IFNX4M01                                                       00009000
  NAME  IFOX41(R)                                                       00009100
  INCLUDE  AOS03(IFNX4E,IFNX4N,IFNX4S,IFNX4T,IFNX4V)                    00009200
  ENTRY  IFNX4T01                                                       00009300
  NAME  IFOX42(R)                                                       00009400
  INCLUDE  AOS03(IFNX5A,IFNX5C,IFNX5D,IFNX5F)                           00009500
  INCLUDE  AOS03(IFNX5L,IFNX5M,IFNX5P,IFNX5V)                           00009600
  ENTRY  IFNX5C01                                                       00009700
  NAME  IFOX51(R)                                                       00009800
  INCLUDE  AOS03(IFNX6A)                                                00009900
  ENTRY IFNX6A01                                                        00010000
  NAME  IFOX61(R)                                                       00010100
  INCLUDE  AOS03(IFNX6B,IFNX6C)                                         00010200
  ENTRY  IFNX6B01                                                       00010300
  NAME  IFOX62(R)                                                       00010400
//*                                                                     00010500
//LINK    EXEC LINKS,PARM='NCAL,LIST,XREF,LET,RENT,REFR',               00010600
//             CLASS=A,OBJ=,UNIT=,SER=,N=' ',NAME=,P1=,MOD=,P2=         00010700
//SYSPUNCH DD  DUMMY                                                    00010800
//SYSLMOD  DD  DSN=SYS1.LINKLIB,DISP=SHR                                00010900
><                                                                      00011000
//DDIN2    DD  DSN=*.ASM.SYSPUNCH,DISP=(OLD,DELETE)                     00011100
//DDOUT    DD  UNIT=SYSDA,DISP=(MOD,PASS),                              00011200
//             SPACE=(TRK,15),DCB=BLKSIZE=80                            00011300
//SYSIN    DD  *                                                        00011400
  REPRO INFILE(DDIN1) OUTFILE(DDOUT)                                    00011500
  REPRO INFILE(DDIN2) OUTFILE(DDOUT) SKIP(4)                            00011600
//*                                                                     00011700
//********************************************************************* 00011800
//* WRITE JOBSTEAM TO INTERNAL READER TO LINK EDIT IFOX00/ICKDSF        00011900
//********************************************************************* 00012000
//*                                                                     00012100
//IEBGENER EXEC PGM=IEBGENER,COND=(0,NE,ASM)                            00012200
//SYSIN    DD  DUMMY                                                    00012300
//SYSPRINT DD  DUMMY                                                    00012400
//SYSUT1   DD  DSN=*.IDCAMS.DDOUT,DISP=(OLD,DELETE)                     00012500
//SYSUT2   DD  SYSOUT=(A,INTRDR)                                        00012600
//*                                                                     00012700
//********************************************************************* 00012800
//* COPY SVC REQUIRED BY ICKDSF TO SVCLIB                               00012900
//********************************************************************* 00013000
//*                                                                     00013100
//IEBCOPY EXEC PGM=IEBCOPY,REGION=1024K,COND=(0,NE,ASM)                 00013200
//SYSPRINT DD  SYSOUT=A                                                 00013300
//IN       DD  DSN=SYS1.AOSU0,DISP=SHR,UNIT=SYSDA,VOL=SER=SMP000        00013400
//OUT      DD  DSN=SYS1.SVCLIB,DISP=OLD                                 00013500
//SYSUT3   DD  UNIT=SYSDA,SPACE=(80,(60,45)),DISP=(NEW,DELETE)          00013600
//SYSIN    DD  *                                                        00013700
  COPY INDD=IN,OUTDD=OUT                                                00013800
  SELECT MEMBER=(IGG019P2)                                              00013900
//                                                                      00014000
