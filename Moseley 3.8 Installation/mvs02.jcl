//MVS02    JOB (1),'ADD TSO USERS',CLASS=S,MSGLEVEL=(1,1),
//             MSGCLASS=A
//*
//* ----------------------------------------------------------------- *
//* This job adds a new TSO USER ID and allocates default datasets    *
//* for that user. You may change parameters for any USER ID to fit   *
//* your needs; however you should not change a parameter if you are  *
//* unsure of the results.
//* ----------------------------------------------------------------- *
// EXEC TSONUSER,ID=HMVS01,    This will be the logon ID 
//      PW='*',                No password will be required to logon
//      AN='*',                No accounting number stored for user
//      OP='OPER',             Allow operator authority
//      AC='NOACCT',           Do not allow ACCOUNT TSO COMMAND
//      JC='JCL',              alloc submit/cancel/status/output 
//      MT='MOUNT'             allow tape mounts
// EXEC TSONUSER,ID=HMVS02,    This will be the logon ID 
//      PW='secret',           Password 'secret' required to logon 
//      AN='IT1506',           'IT1506' will be accounting info
//      OP='NOOPER',           Do not allow operator authority
//      AC='NOACCT',           Do not allow ACCOUNT TSO COMMAND
//      JC='NOJCL',            Do not alloc submit/cancel/status/output
//      MT='NOMOUNT'           Do not allow tape mounts
//* ----------------------------------------------------------------- *
//* ID specified must be no longer than 7 characters
//*    first character must be alphabetic or national ($,@,#)
//*    remaining characters may be alphabetic, numeric, or national
//* PW asterisk (*) if no password is required
//*    if 'word' specified, may be no longer than 8 characters
//*    all characters must be alphabetic, numeric, or national ($,@,#)
//* AN asterisk (*) or accounting number to be used for user
//*    will be stored in SMF records for user
//*    maximum length 40 characters
//*    any character except blank, comma, tab, semicolon, apostrophe
//* OP may be OPER or NOOPER
//*    if OPER specified, operator commands will be allowed from ID
//* AC may be ACCT or NOACCT
//*    if ACCT specified, ACCOUNT TSO COMMAND will be allowed from ID
//* JC may be JCL or NOJCL
//*    if JCL specified, SUBMIT, CANCEL, STATUS, and OUTPUT commands
//*       will be allowed from ID
//* MT may be MOUNT or NOMOUNT
//*    if MOUNT specified, ID may request tape mounts
//