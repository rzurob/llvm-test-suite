!*  ===================================================================
!*
!*  DATE                       : March  10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : WAIT() on Pending Non-Advancing Read Data
!*                               Transfer Operations (with/without ID=
!*                               Specifier) for a Specific Unit for a
!*                               Stream File
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

PROGRAM EORSpecRead04
    USE ISO_FORTRAN_ENV

    INTEGER, DIMENSION( 10 ) :: ioID
    INTEGER, DIMENSION( 10 ) :: dataValues

    CHARACTER(LEN = 256) :: iMsg


    OPEN(555, ASYNCHRONOUS='yes', ACCESS='stream',&
         &ACTION='read', FILE='EORSpecRead04.dat',&
         &FORM='formatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        READ(555, '(I4)', ASYNCHRONOUS='yes', ADVANCE='no',&
            &ID=ioID( i ), IOSTAT=iStat, IOMSG=iMsg) dataValues( i )

        IF ((iStat <> 0)  .AND.  (iStat <> IOSTAT_EOR)) THEN
            WRITE(0, *) i, ") READ() <", iStat, "> ", iMsg
            CALL zzrc( (i + 10) )
        END IF
    END DO


    WAIT(ID=ioID( 7 ), END=100, UNIT=555, EOR=200, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(", ioID( 7 ), ") <", iStat, "> ", iMsg
        CALL zzrc( 27 )
    END IF

    GOTO 300

100 WRITE(6, *) "7) EndOfFile WAIT(", ioID( 7 ), ") <", iStat, "> < ", iMsg
    GOTO 300

200 WRITE(6, *) "7) EOR WAIT(", ioID( 7 ), ") <", iStat, "> < ", iMsg


300 WAIT(555, EOR=300, END=500, ID=ioID( 3 ), IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT(", ioID( 3 ), ") <", iStat, "> ", iMsg
        CALL zzrc( 23 )
    END IF

    GOTO 600

400 WRITE(6, *) "3) EOR WAIT(", ioID( 3 ), ") <", iStat, "> < ", iMsg
    GOTO 600

500 WRITE(6, *) "3) EndOfFile WAIT(", ioID( 3 ), ") <", iStat, "> < ", iMsg


600 WAIT(555, EOR=700, IOSTAT=iStat, END=800, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
        CALL zzrc( 31 )
    END IF

    GOTO 900

700 WRITE(6, *) "EOR WAIT() <", iStat, "> < ", iMsg
    GOTO 900

800 WRITE(6, *) "EndOfFile WAIT() <", iStat, "> < ", iMsg


900 DO i = 1, 10
        WRITE(6, '(I2,")",I4)') i, dataValues( i )
    END DO


    CLOSE(555, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 41 )
    END IF

END PROGRAM EORSpecRead04
