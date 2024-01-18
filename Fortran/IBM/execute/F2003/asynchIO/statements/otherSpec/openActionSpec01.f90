!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : openActionSpec01 - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 20, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : ACTION=Read Specifier in OPEN()
!*                               Statement
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier,
!*                               ACTION= Specifier
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*
!*  9.4.5 The OPEN statement
!*  R904 open-stmt     is  OPEN ( connect-spec-list )
!*  R905 connect-spec  is  [ UNIT = ] file-unit-number
!*                     or  ACCESS = scalar-default-char-expr
!*                     or  ACTION = scalar-default-char-expr
!*                     or  ASYNCHRONOUS = scalar-default-char-expr
!*  ...
!*
!*  9.4.5.2 ACTION= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to READ, WRITE, or READWRITE.
!*
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM openActionSpec01

    INTEGER, PARAMETER :: ioUnit = 55

    INTEGER :: i

    INTEGER :: iStat
    INTEGER :: wStat

    CHARACTER(LEN = 256) :: iMsg

    INTEGER :: aID
    INTEGER, DIMENSION( 50,50 ) :: dataArray
    COMMON /theCommon/ aID, dataArray


    OPEN(UNIT=ioUnit, FILE='openActionSpec01.dat', FORM='formatted',&
            &ACTION='read', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    wStat = GetData( ioUnit )

    IF (wStat == 0) THEN
        WAIT(UNIT=ioUnit, ID=aID, IOSTAT=wStat, IOMSG=iMsg)
        IF (wStat <> 0) THEN
            WRITE(0, *) "OPEN() <", wStat, "> ", iMsg
            wStat = 3
        END IF
    END IF


    DO i = 1, 50
        DO j = 1, 5
            PRINT 100, (dataArray( i,(j + k) ), k = 1, 10)
100         FORMAT(10I4)
        END DO
    END DO


    CLOSE(UNIT=ioUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 4 )

    ELSE IF (wStat <> 0) THEN
        CALL zzrc( wStat )
    END IF

END PROGRAM openActionSpec01


INTEGER FUNCTION GetData( theUnit )
    INTEGER :: theUnit

    CHARACTER(LEN = 256) :: msg

    INTEGER :: idA
    INTEGER :: intArray( 2500 )
    COMMON /theCommon/ idA, intArray

    READ(theUnit, FMT='(10I4)', ID=idA,&
        &ASYNCHRONOUS='yes', IOSTAT=iStatus,&
        &IOMSG=msg) (intArray( i ), i = 1, 2500)
    IF (iStatus <> 0) THEN
        WRITE(0, *) "READ() <", iStatus, "> ", msg
    END IF

    GetData = iStatus
END FUNCTION GetData
