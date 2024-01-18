! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/asynchIO/statements/otherSpec/openBlankSpec01.f
! opt variations: -qnol -qreuse=base

!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February 21, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : BLANK=Null Specifier in OPEN() Statement
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : OPEN(), ASYNCHRONOUS= Specifier,
!*                               BLANK= Specifier
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
!*                     or  BLANK = scalar-default-char-expr
!*  ...
!*
!*  9.4.5.4 BLANK= specifier in the OPEN statement
!*
!*  The scalar-default-char-expr shall evaluate to NULL or ZERO.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

MODULE mBase
    TYPE tBase(N1,K1)    ! (20,4)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
        INTEGER(K1)   :: b
    END TYPE tBase
END MODULE mBase


MODULE mDerived1
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived1(N2,K2)    ! (20,4,20,4)
        INTEGER, KIND :: K2
        INTEGER, LEN  :: N2
        INTEGER(K2)   :: d1
    END TYPE tDerived1
END MODULE mDerived1


MODULE mDerived2
    USE mDerived1

    TYPE, EXTENDS(tDerived1) :: tDerived2(N3,K3)    ! (20,4,20,4,20,4)
        INTEGER, KIND :: K3
        INTEGER, LEN  :: N3
        INTEGER(K3)   :: d2
    END TYPE tDerived2
END MODULE mDerived2


MODULE mDerived3
    USE mDerived2

    TYPE, EXTENDS(tDerived2) :: tDerived3(N4,K4)    ! (20,4,20,4,20,4,20,4)
        INTEGER, KIND :: K4
        INTEGER, LEN  :: N4
        INTEGER(K4)   :: d3
    END TYPE tDerived3
END MODULE mDerived3


MODULE mDerived4
    USE mDerived3

    TYPE, EXTENDS(tDerived3) :: tDerived4(N5,K5)    ! (20,4,20,4,20,4,20,4,20,4)
        INTEGER, KIND :: K5
        INTEGER, LEN  :: N5
        INTEGER(K5)   :: d4
    END TYPE tDerived4
END MODULE mDerived4


PROGRAM openBlankSpec01
    USE ISO_FORTRAN_ENV
    USE mDerived4

    IMPLICIT NONE

    INTEGER :: aID

    INTEGER :: iStat
    INTEGER :: rStat
    INTEGER :: wStat

    LOGICAL :: failed = .FALSE.

    CHARACTER(LEN =256) :: msg
    CHARACTER(LEN =256) :: rMsg
    CHARACTER(LEN =256) :: wMsg

    TYPE(tDerived4(20,4,20,4,20,4,20,4,20,4)) :: b1d4
    TYPE(tDerived4(20,4,20,4,20,4,20,4,20,4)) :: aB1d4


    OPEN(29, FILE='openBlankSpec01.dat', BLANK='null',&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=msg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", msg
        CALL zzrc( 1 )
    END IF


    READ(29, '(5I5)', ASYNCHRONOUS='no', IOSTAT=rStat, IOMSG=rMsg) b1d4

    DO WHILE ((.NOT. failed)  .AND.  (rStat .NE. IOSTAT_END))
        READ(29, FMT='(5I5)', ID=aID,&
            &ASYNCHRONOUS='yes', IOSTAT=rStat, IOMSG=rMsg) aB1d4


        IF (b1d4%b <> b1d4%d1) THEN
            failed = .TRUE.
            WRITE(0, *) "b1d4%b = (", b1d4%b, "), b1d4%d1 = (", b1d4%d1, ")"

        ELSE IF (b1d4%b <> b1d4%d2) THEN
            failed = .TRUE.
            WRITE(0, *) "b1d4%b = (", b1d4%b, "), b1d4%d2 = (", b1d4%d2, ")"

        ELSE IF (b1d4%b <> b1d4%d3) THEN
            failed = .TRUE.
            WRITE(0, *) "b1d4%b = (", b1d4%b, "), b1d4%d3 = (", b1d4%d3, ")"

        ELSE IF (b1d4%b <> b1d4%d4) THEN
            failed = .TRUE.
            WRITE(0, *) "b1d4%b = (", b1d4%b, "), b1d4%d4 = (", b1d4%d4, ")"
        END IF


        WAIT(29, ID=aID, IOSTAT=wStat, IOMSG=wMsg)

        b1d4 = aB1d4
    END DO


    CLOSE(29, IOSTAT=iStat, IOMSG=msg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", msg
        CALL zzrc( 5 )

    ELSE IF ((rStat <> 0)  .AND.  (rStat <> IOSTAT_END)) THEN
        WRITE(0, *) "READ() <", rStat, "> ", rMsg
        CALL zzrc( 2 )

    ELSE IF ( failed ) THEN
        CALL zzrc( 3 )

    ELSE IF (wStat <> 0) THEN
        WRITE(0, *) "WAIT() <", wStat, "> ", wMsg
        CALL zzrc( 4 )
    END IF

END PROGRAM openBlankSpec01
