!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ERRSpecRead01 - WAIT() Statement
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : March 15, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending (Unformatted) Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier)
!*  SECONDARY FUNCTIONS TESTED : Error Condition causes jump to ERR= Label
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), ERR= Specifier, ID= Specifier,
!*                               IOSTAT= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  9.6.1 WAIT statement
!*
!*  A WAIT statement performs a wait operation for specified pending
!*  asynchronous data transfer operations.
!*
!*  R921 wait-stmt  is  WAIT (wait-spec-list)
!*  R922 wait-spec  is  [ UNIT = ] file-unit-number
!*                  or  END = label
!*                  or  EOR = label
!*                  or  ERR = label
!*                  or  ID = scalar-int-expr
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  If any error, end-of-file, or end-of-record conditions occur, the
!*  applicable actions specified by the IOSTAT=, IOMSG=, ERR=, END=,
!*  and EOR= specifiers of the statement that performs the wait oper-
!*  ation are taken.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM ERRSpecRead01

    CHARACTER(LEN = 256) :: iMsg = ''


    OPEN(447, ACTION='read', ACCESS='direct',&
        &ASYNCHRONOUS='yes', RECL=4, FORM='unformatted',&
        &FILE='ERRSpecRead01.dat', IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    READ(447, ID=iID, ASYNCHRONOUS='yes',&
        &REC=100, IOSTAT=iStat, IOMSG=iMsg) dataValue
    IF (iStat <> 0) THEN
        WRITE(0, *) "READ() <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF


    WAIT(447, ID=iID, ERR=100, IOSTAT=iStat, IOMSG=iMsg)

    WRITE(0, *) "WAIT(ID=", iID, ") <", iStat, "> ", iMsg
    CALL zzrc( 3 )

100 WRITE(0, *) "WAIT(ERR=100,ID=", iID, ") <", iStat, "> ", iMsg

    IF (iStat <> 1) THEN
        WRITE(0, *) "WAIT(ERR=100,ID=", iID, ") <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF



    READ(447, ASYNCHRONOUS='yes', REC=100, IOSTAT=iStat, IOMSG=iMsg) dataValue
    IF (iStat <> 0) THEN
        WRITE(0, *) "READ() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    WAIT(447, ERR=200, IOSTAT=iStat, IOMSG=iMsg)

    WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
    CALL zzrc( 6 )

200 WRITE(0, *) "WAIT(ERR=200) <", iStat, "> ", iMsg

    IF (iStat <> 1) THEN
        WRITE(0, *) "WAIT(ERR=200) <", iStat, "> ", iMsg
        CALL zzrc( 7 )
    END IF


    CLOSE(447, IOMSG=iMsg, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 8 )
    END IF

END PROGRAM ERRSpecRead01
