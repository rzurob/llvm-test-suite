! GB DTP extension using:
! ftcx_dtp -qck -qreuse=none /tstdev/F2003/asynchIO/statements/otherSpec/openDecimalSpec01.f
! opt variations: -qnock -qreuse=self

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : openDecimalSpec01 - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 22, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : DECIMAL=Comma Specifier in OPEN() Statement
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier,
!*                               DECIMAL= Specifier
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
!*                     or  DECIMAL = scalar-default-char-expr
!*
!*  9.4.5.5 DECIMAL= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to COMMA or POINT.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mRealPair
    IMPLICIT NONE

    TYPE tRealPair(K1,K2,N1,K3)    ! (4,1,1,4)
        INTEGER, KIND             :: K1,K2,K3
        INTEGER, LEN              :: N1
        INTEGER(K1)               :: realAsInt
        CHARACTER(kind=K2,len=N1) :: filler
        REAL(K3)                  :: realAsReal
    END TYPE tRealPair
END MODULE mRealPair


PROGRAM openDecimalSpec01
    USE mRealPair
    USE ISO_FORTRAN_ENV

    IMPLICIT NONE

    INTEGER :: iStat
    INTEGER :: oStat

    INTEGER :: rID = -1
    INTEGER :: wID = -1

    INTEGER :: oUnit = 2048

    CHARACTER(LEN = 256) :: iMsg
    CHARACTER(LEN = 256) :: oMsg

    CHARACTER(LEN = 30) :: fileName = 'openDecimalSpec01.dat'

    INTEGER :: i
    INTEGER :: j
    INTEGER :: k

    TYPE(tRealPair(4,1,1,4)), DIMENSION( 100 ) :: readPair
    TYPE(tRealPair(4,1,1,4)), DIMENSION( 100 ) :: procPair
    TYPE(tRealPair(4,1,1,4)), DIMENSION( 100 ) :: writePair


    OPEN(1024, FILE=fileName, DECIMAL='comma',&
                &FORM='formatted', ASYNCHRONOUS='yes',&
                &ACTION='read', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN(Read,", fileName, ") <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF

    OPEN(oUnit, FORM='formatted', ACTION='write',&
        &DECIMAL='comma', ASYNCHRONOUS='yes', IOSTAT=oStat, IOMSG=oMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "OPEN(Write,fort.", oUnit, ") <", oStat, "> ", oMsg
        CALL zzrc( 2 )
    END IF


    READ(UNIT=1024, FMT=200, ASYNCHRONOUS='no',&
        &IOSTAT=iStat, IOMSG=iMsg) (readPair( i ), i = 1, 100)

200 FORMAT(I6,A1,F7.3)

    DO WHILE ((iStat == 0)  .AND.  (oStat == 0))
        procPair = readPair

        READ(UNIT=1024, FMT=200, ASYNCHRONOUS='yes', ID=rID,&
            &IOSTAT=iStat, IOMSG=iMsg) (readPair( i ), i = 1, 100)
        IF ((iStat /= 0) .AND.&
            &(iStat /= IOSTAT_END)) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 3 )
        END IF


        DO j = 1, 100
            procPair( j )%realAsInt = procPair( j )%realAsInt + 1
            procPair( j )%realAsReal = procPair( j )%realAsReal + 1.0
        END DO


        IF (wID /= -1) THEN
            WAIT(2048, ID=wID, IOSTAT=oStat, IOMSG=oMsg)
            IF (oStat /= 0) THEN
                WRITE(0, *) "WAIT(Write) <", oStat, "> ", oMsg
                CALL zzrc( 4 )
            END IF
        END IF


        writePair = procPair
        WRITE(2048, FMT=300, ASYNCHRONOUS='yes', ID=wID, IOSTAT=oStat,&
                    &IOMSG=oMsg) ((writePair( k )%realAsReal - 1.0), k = 1, 100)
300     FORMAT(F7.3)

        IF (oStat /= 0) THEN
            WRITE(0, *) "WRITE() <", oStat, "> ", oMsg
            CALL zzrc( 5 )
        END IF


        IF (iStat == 0) THEN
            WAIT(1024, ID=rID, IOSTAT=iStat, IOMSG=iMsg)
            IF (iStat /= 0) THEN
                WRITE(0, *) "WAIT(Read) <", iStat, "> ", iMsg
                CALL zzrc( 6 )
            END IF
        END IF
    END DO


    CLOSE(UNIT=2048, IOSTAT=oStat, IOMSG=oMsg)
    IF (oStat /= 0) THEN
        WRITE(0, *) "CLOSE() <", oStat, "> ", oMsg
        CALL zzrc( 7 )
    END IF

    CLOSE(UNIT=1024, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat /= 0) THEN
        WRITE(0, *) "CLOSE(", fileName, ") <", iStat, "> ", iMsg
        CALL zzrc( 8 )
    END IF

END PROGRAM openDecimalSpec01
