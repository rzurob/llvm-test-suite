!*  ===================================================================
!*
!*  DATE                       : March  10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Non-Advancing Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier) for a Specific Unit for a
!*                               Stream File
!*  SECONDARY FUNCTIONS TESTED : EOR= Specifier (end-of-record Condition
!*                               is *NOT* encountered)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), EOR= Specifier, ID= Specifier
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
!*                  or  IOMSG = iomsg-variable
!*                  or  IOSTAT = scalar-int-variable
!*
!*  ...
!*
!*  NOTE 9.52
!*  An EOR= specifier has no effect if the pending data transfer operation
!*  is not a nonadvancing read.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM EORSpecRead05

    INTEGER :: dataItem1
    INTEGER :: dataItem2

    CHARACTER(LEN = 256) :: iMsg


    ioUnit=2908
    OPEN(ioUnit, ACCESS='stream', ASYNCHRONOUS='yes',&
        &ACTION='read', FORM='formatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    READ(ioUnit, FMT=999, ASYNCHRONOUS='yes',&
            &IOSTAT=iStat, IOMSG=iMsg) dataItem1
    IF (iStat <> 0) THEN
        WRITE(0, *) "1) READ() <", iStat, "> ", iMsg
        CALL zzrc( 2 )
    END IF


    i = 1
    ioID = -9

    WAIT(EOR=100, UNIT=ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF


    READ(ioUnit, FMT=999, ASYNCHRONOUS='yes',&
        &ID=ioID, IOSTAT=iStat, IOMSG=iMsg) dataItem2
    IF (iStat <> 0) THEN
        WRITE(0, *) "2) READ() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF

999 FORMAT(I4)


    i = 2

    WAIT(ID=ioID, UNIT=ioUnit, IOSTAT=iStat, EOR=100, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    WRITE(6, '("dataItem1 = <",I4,">, dataItem2 = <",I4,">")')&
                                                &dataItem1, dataItem2

    GOTO 200


100 WRITE(0, *) i, ") EndOfRecord WAIT() <", iStat, "> ", iMsg

200 CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF

END PROGRAM EORSpecRead05
