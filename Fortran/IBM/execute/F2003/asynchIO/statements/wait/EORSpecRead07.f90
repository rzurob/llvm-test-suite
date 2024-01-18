!*  ===================================================================
!*
!*  DATE                       : March  10, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Non-Advancing Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier) for a Specific Unit for a
!*                               Direct File
!*  SECONDARY FUNCTIONS TESTED : EOR= Specifier (end-of-record Condition
!*                               is encountered)
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

PROGRAM EORSpecRead07

    INTEGER, DIMENSION( 100 ) :: ioID
    INTEGER, DIMENSION( 100 ) :: dataList

    CHARACTER(LEN = 256) :: iMsg


    OPEN(78, IOSTAT=iStat, IOMSG=iMsg, ASYNCHRONOUS='yes',&
              &FILE='EORSpecRead07.dat', FORM='formatted',&
                        &ACCESS='direct', ACTION='read', RECL=5)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 100
        READ(78, '(10I5)', ASYNCHRONOUS='yes', ID=ioID( i ),&
                &IOSTAT=iStat, REC=i, IOMSG=iMsg) dataList( i )
        IF (iStat /= 0) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO


    WAIT(ID=ioID( 78 ), UNIT=78, EOR=100, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "WAIT(", ioID( 78 ), ") <", iStat, "> ", iMsg
        CALL zzrc( 3 )
    END IF

    GOTO 200

100 WRITE(6, *) "EndOfRecord -- WAIT(", ioID( 78 ), ") <", iStat, "> ", iMsg


200 WAIT(78, EOR=300, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 4 )
    END IF

    GOTO 400

300 WRITE(6, *) "EndOfRecord -- WAIT() <", iStat, "> ", iMsg


400 CLOSE(78, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    DO i = 1, 100, 10
        WRITE(6, '(10I5)') (dataList( (i + j) ), j = 0, 9)
    END DO

END PROGRAM EORSpecRead07
