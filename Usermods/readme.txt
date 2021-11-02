The source code in these files was contributed by Tom Armstrong.

ieavad07.asm - Source code to match the UZ60132 level of IEAVAD07
               with the possible exception that references to bytes
               at offsets +1E and +1F of the CDE have been corrected
               - probably due to an incorrect IHACDE structure at
               compile time at IBM.  Those bytes are not used under
               MVS/370.

ZP60028.JCL  - JCL to create, RECEIVE and APPLY the ZP60028 USERMOD.
               This includes source code based on the above file, deftly
               modified by Tom, and further mutilated by me to remove
               hundreds of lines that could be replaced by a few DSECT
               macro calls.

               GP@P6
               March 2010.