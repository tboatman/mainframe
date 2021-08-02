//SMPMOUNT JOB (SYSGEN),'ISSUE MOUNT COMMANDS',CLASS=A,MSGCLASS=A       00010000
//********************************************************************* 00020000
//*                                                                   * 00030000
//* This job presents a MOUNT command to the operator for confirma-   * 00040000
//* tion that sets the Storage Class to PRIVATE for the volume that   * 00050000
//* is to receive Distribution Library Datasets.                      * 00060000
//*                                                                   * 00070000
//********************************************************************* 00080000
// M 148,VOL=(SL,SMP000),USE=PRIVATE                                    00090000
//S1       EXEC PGM=IEFBR14                                             00100000
//                                                                      00110000
