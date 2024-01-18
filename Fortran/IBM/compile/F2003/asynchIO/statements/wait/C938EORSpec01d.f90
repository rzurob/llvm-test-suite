!*  ===================================================================
!*
!*  DATE                       : March 15, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() Statement
!*  SECONDARY FUNCTIONS TESTED : Multiple EOR= Specifiers
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), EOR= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
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
!*
!*  C938 (R922) No specifier shall appear more than once in a given
!*              wait-spec-list.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM C938EORSpec01d

    OPEN(237, ASYNCHRONOUS='yes', IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, ">"
        CALL zzrc( 1 )
    END IF


    WRITE(237, ASYNCHRONOUS='yes', IOSTAT=iStat) 237
    IF (iStat <> 0) THEN
        WRITE(0, *) "WRITE() <", iStat, ">"
        CALL zzrc( 2 )
    END IF


    WAIT(UNIT=237, EOR=100, IOSTAT=iStat, EOR=200)
100 IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, ">"
        CALL zzrc( 3 )
    END IF


200 CLOSE(237, IOSTAT=iStat)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, ">"
        CALL zzrc( 4 )
    END IF

END PROGRAM C938EORSpec01d
