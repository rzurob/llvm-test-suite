!*  ===================================================================
!*
!*  DATE                       : March  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Read Data Transfer
!*                               Operations (with/without ID= Specifier)
!*                               for a Specific Unit
!*  SECONDARY FUNCTIONS TESTED : END= Specifier (end-of-file Condition *NOT*
!*                               encountered)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : WAIT(), END= Specifier, ID= Specifier
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
!*  ...
!*  And END= specifier has no effect if the pending data transfer operation
!*  is not a READ.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM ENDSpecRead01

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 5,10 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    ioUnit = 91
    OPEN(ioUnit, ACTION='read', IOSTAT=iStat,&
        &IOMSG=iMsg, FILE='ENDSpecRead01.dat',&
        &ACCESS='stream', FORM='formatted', ASYNCHRONOUS='yes')
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        READ(UNIT=ioUnit, FMT='(5I4)', ASYNCHRONOUS='yes', ID=ioID( i ),&
            &IOSTAT=iStat, IOMSG=iMsg) (dataValues( j,i ), j = 1, 5)
        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    DO j = 10, 5, -1
        WAIT(END=100, UNIT=ioUnit, ID=ioID( j ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) j, ") WAIT(", ioID( j ), ") <", iStat, "> ", iMsg
            CALL zzrc( (20 + j) )
        END IF
    END DO


    WAIT(ioUnit, END=200, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 31 )
    END IF

    GOTO 300


100 WRITE(6, *) j, ") WAIT(", ioID( j ), ")  End of File"
    GOTO 400


200 WRITE(6, *) "WAIT()  End of File"
    GOTO 400


300 DO i = 1, 10
        WRITE(6, '(5I4)') (dataValues( j,i ), j = 1, 5)
    END DO


400 CLOSE(ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 40 )
    END IF

END PROGRAM ENDSpecRead01
