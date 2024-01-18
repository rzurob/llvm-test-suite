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

PROGRAM EORSpecRead06
    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: iStat

    INTEGER, DIMENSION( 100 ) :: ioID
    INTEGER, DIMENSION( 100 ) :: dataList

    CHARACTER(LEN = 256) :: iMsg


    OPEN(3834, ASYNCHRONOUS='yes', ACCESS='direct', RECL=4,&
        &ACTION='readwrite', FORM='unformatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 100
        WRITE(3834, ASYNCHRONOUS='yes', REC=i,&
                &IOSTAT=iStat, IOMSG=iMsg) (i * 100 - i)
        IF (iStat /= 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF
    END DO


    DO i = 1, 100
        READ(3834, ASYNCHRONOUS='yes', ID=ioID( i ), ADVANCE='no',&
            &REC=(100 - i + 1), IOSTAT=iStat, IOMSG=iMsg) dataList( i )
        IF (iStat /= 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 3 )
        END IF
    END DO


    DO i = 75, 25, -5
        WAIT(3834, ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg, EOR=300)
        IF (iStat /= 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 4 )
        END IF
    END DO


    i = 1
    ioID( i ) = -99

    WAIT(3834, IOSTAT=iStat, IOMSG=iMsg, EOR=300)
    IF (iStat /= 0) THEN
        WRITE(0, *) "READ() <", iStat, "> ", iMsg
        CALL zzrc( 5 )
    END IF


    DO i = 1, 100, 10
        WRITE(6, 100) (dataList( (i + j) ), j = 0, 9)
    END DO

100 FORMAT(10I6)


200 CLOSE(3834, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 6 )
    END IF

    STOP 0


300 WRITE(6, *) i, ") EndOfRecord WAIT(",&
                &ioID( i ), ") <", iStat, "> ", iMsg
    GOTO 200

END PROGRAM EORSpecRead06
