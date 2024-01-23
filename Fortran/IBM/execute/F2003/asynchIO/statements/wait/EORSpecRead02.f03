!*  ===================================================================
!*
!*  DATE                       : March  8, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Non-Advancing Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier) for a Specific Unit for a
!*                               Sequential File
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

PROGRAM EORSpecRead02

    INTEGER, DIMENSION( 10 ) :: ioIDs
    INTEGER, DIMENSION( 10 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    OPEN(UNIT=1901, ASYNCHRONOUS='yes', ACCESS='sequential',&
        &FORM='unformatted', FILE='EORSpecRead02.dat',&
        &ACTION='read', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 10
        READ(1901, ID=ioIDs( i ), ASYNCHRONOUS='yes',&
            &ADVANCE='no', IOSTAT=iStat, IOMSG=iMsg) dataValues( i )
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", iMsg
            CALL zzrc( (10 + i) )
        END IF
    END DO


    DO i = 10, 1, -3
        WAIT(1901, ID=ioIDs( i ), E0R=100, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WAIT(", ioIDs( i ), ") <", iStat, "> ", iMsg
            CALL zzrc( (20 + i) )
        END IF

        GOTO 200

100     WRITE(6, *) i, ") EOR WAIT(", ioIDs( i ), ") <", iStat, "> ", iMsg

200     CONTINUE
    END DO


    WAIT(E0R=300, IOSTAT=iStat, UNIT=1901, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) i, ") WAIT() <", iStat, "> ", iMsg
        ERROR STOP 31
    END IF

    GOTO 400

300 WRITE(6, *) "EOR WAIT() <", iStat, "> ", iMsg

400 CONTINUE


    DO i = 1, 10
        WRITE(6, '(I2,")",I4)') i, dataValues( i )
    END DO

    CLOSE(UNIT=1901, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF

END PROGRAM EORSpecRead02
