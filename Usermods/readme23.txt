
Contents of zp60023.zip:


ZP60023.JCL - Job (shipped as ASCII text) to install
              the ZP60023 usermod - note that it
              requires the SYS1.APVTMACS macro library.
              Also note the MACUPD dependence on the
              sequence numbers in columns 72 to 80.


ZP60023.BIN - The binary (ie. EBCDIC) SMPPTFIN file
              of the ZP60023 usermod ready to RECEIVE.




Greg Price    November 2008

_______________________________________________________

Reworked to fix the save location of the contents of
floating point registers for suspended tasks.  Most
applications using DAS may not use floating point, so
the possible data loss may not be important, but the
unintended storage overlay can have potentially serious
consequences.


Greg Price    January  2012

_______________________________________________________

Reworked to fix condition code setting, fix the BRCL 
branch address calculation, and add six more instructions.


Greg Price    June     2015

_______________________________________________________

Reworked to remove simulation of post-370 instructions.

This is no longer needed due to activation of these
(and more) instructions in versions of Hercules that
are now available. Also, some of the instructions were
not correctly simulated, and some were prone to causing
recursion loops in the PCFLIH.


Greg Price    August   2016

