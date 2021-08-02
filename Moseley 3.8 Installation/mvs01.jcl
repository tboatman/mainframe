//MVS01    JOB (1),'SETUP USER CATS',CLASS=S,MSGLEVEL=(1,1),
//             MSGCLASS=A
//*
//IDCAMS01 EXEC PGM=IDCAMS,REGION=4096K
//* ----------------------------------------------------------------- *
//* This job creates User Catalogs and defines ALIASes to them.       *
//*                                                                   *
//* PUB000 will contain TSO User's NON-VSAM datasets; they will be    *
//* catalogued in a User Catalog defined on PUB000 volume.            *
//* PUB001 will contain a VSAM dataspace to hold suballocated VSAM    *
//* objects; they and NON-VSAM datasets defined on PUB001 will be     *
//* catalogued in a User Catalog defined on PUB001 volume.            *
//* ----------------------------------------------------------------- *
//SYSPRINT DD  SYSOUT=*
//PUB000   DD  UNIT=SYSDA,DISP=OLD,VOL=SER=PUB000   (3380)
//PUB001   DD  UNIT=SYSDA,DISP=OLD,VOL=SER=PUB001   (3390)
//SYSIN    DD  *

  PARM GRAPHICS(CHAIN(SN))         
  /* This User Catalog will contain NON-VSAM datasets that     */
  /* reside on volume PUB000.                                  */

  DEFINE USERCATALOG ( -
         NAME (UCPUB000) -
         VOLUME (PUB000) -
         CYLINDERS (20) -
         FOR (9999) -
         BUFFERSPACE (8192) )

  /* An Alias is defined so that all datasets with a high-     */
  /* level qualifier of PUB000 will be catalogued in the       */
  /* User Catalog UCPUB000.                                    */

  DEFINE ALIAS ( -
         NAME (PUB000) -
         RELATE (UCPUB000) )

  /* This User Catalog will contain NON-VSAM and VSAM objects  */
  /* that reside on volume PUB001.  Half of the volume will be */
  /* allocated at the same time for use as a VSAM Dataspace.   */
  /* The User Catalog is sub-allocated from that Dataspace.    */

  DEFINE USERCATALOG ( -
         NAME (UCPUB001) -
         VOLUME (PUB001) -
         CYLINDERS (556) -
         FOR (9999) -
         BUFFERSPACE (8192) ) -
           DATA (CYLINDERS (30) ) -
           INDEX (CYLINDERS (15) )

  /* An Alias is defined so that all datasets and VSAM objects */
  /* with the high-level qualifier of PUB000 will be           */
  /* catalogued in the User Catalog UCPUB001.                  */

  DEFINE ALIAS ( -
         NAME (PUB001) -
         RELATE (UCPUB001) )

  /* The last major change has been made to the VSAM Master    */
  /* Catalog at this point, so we will add an Update password  */
  /* to make sure only valid items go in from now forward.     */
         
  ALTER SYS1.VSAM.MASTER.CATALOG -  
        UPDATEPW(SYSPROG)           
//
