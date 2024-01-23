! GB DTP extension using:
! ftcx_dtp -qck -qnol -qreuse=base /tstdev/F2003/asynchIO/statements/otherSpec/openAccessSpec03.f
! opt variations: -qnock -ql -qreuse=none

!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February 20, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : ACCESS=Stream Specifier in OPEN() Statement
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier,
!*                               ACCESS= Specifier
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
!*  9.4.5.1 ACCESS= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to SEQUENTIAL, DIRECT, or
!*  STREAM.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase
    IMPLICIT NONE

    TYPE tBase(K1,N1)    ! (1,26)
        INTEGER, KIND             :: K1
        INTEGER, LEN              :: N1
        CHARACTER(kind=K1,len=N1) :: alpha
    END TYPE tBase
END MODULE mBase


MODULE mDerived
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived(K2)    ! (1,26,1)
        INTEGER, KIND                     :: K2
        INTEGER(kind=K2), DIMENSION( N1 ) :: ascii
    END TYPE tDerived
END MODULE mDerived


PROGRAM openAccessSpec03
    USE mDerived

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: j

    INTEGER :: iStat

    INTEGER :: iUnit = 3
    INTEGER :: oUnit = 4

    INTEGER, DIMENSION( 2 ) :: aioID
    INTEGER :: aID

    CHARACTER(LEN = 26) :: tmpChar
    CHARACTER(LEN = 256) :: iMsg

    TYPE(tDerived(1,26,1)), DIMENSION( 2 ) :: derived


    OPEN(iUnit, ACCESS='stream', ASYNCHRONOUS='yes',&
            &ACTION='read', FORM='formatted',&
            &FILE="openAccessSpec03.dat", IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN(", iUnit, ") <", iStat, "> ", iMsg
        ERROR STOP 1
    END IF


    DO i = 1, 2
        READ(iUnit, FMT='(A26)', ASYNCHRONOUS='yes',&
            &ID=aioID( i ), IOSTAT=iStat, IOMSG=iMsg) derived( i )%alpha

        IF (iStat /= 0) THEN
            WRITE(0, *) "READ(Asynchronous) <", iStat, "> ", iMsg
            ERROR STOP 2
        END IF
    END DO


    DO i = 1, 2
        WAIT(iUnit, ID=aioID( i ), IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) i, ") WAIT(", aioID( i ), ") <", iStat, "> ", iMsg
            ERROR STOP 3
        END IF

        tmpChar = derived( i )%alpha
        DO j = 1, 26
            derived( i )%ascii( j ) = ICHAR( derived( i )%alpha( j:j ) )
        END DO
    END DO


    OPEN(oUnit, ACCESS='stream', ASYNCHRONOUS='yes',&
        &ACTION='write', FORM='formatted', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN(", oUnit, ") <", iStat, "> ", iMsg
        ERROR STOP 4
    END IF


    WRITE(oUnit, "(26(' ',Z2))", ASYNCHRONOUS='yes',&
            &ID=aioID( 1 ), IOSTAT=iStat, IOMSG=iMsg)&
            &((derived( i )%ascii( j ), j = 1, 26), i = 1, 2)

    IF (iStat /= 0) THEN
        WRITE(0, *) "WRITE() <", iStat, "> ", iMsg
        ERROR STOP 5
    END IF


    CLOSE(oUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE(", oUnit, ") <", iStat, "> ", iMsg
        ERROR STOP 6
    END IF


    CLOSE(iUnit, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE(", iUnit, ") <", iStat, "> ", iMsg
        ERROR STOP 7
    END IF

END PROGRAM openAccessSpec03
