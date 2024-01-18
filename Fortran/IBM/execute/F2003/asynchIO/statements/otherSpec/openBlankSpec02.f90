!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February 21, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : BLANK=Zero Specifier in OPEN() Statement
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

MODULE mPair
    IMPLICIT NONE

    TYPE tPair
        INTEGER :: a
        INTEGER :: b
    END TYPE tPair
END MODULE mPair


MODULE mEntry
    USE mPair

    IMPLICIT NONE

    TYPE tEntry
        TYPE(tPair), DIMENSION( 5 ) :: pair
    END TYPE tEntry
END MODULE mEntry


PROGRAM openBlankSpec02
    USE mEntry

    IMPLICIT NONE

    INTEGER :: i
    INTEGER :: iStat

    CHARACTER(LEN = 256) :: iMsg

    TYPE(tEntry) :: entry
    TYPE(tEntry) :: newEntry

    OPEN(99, FILE='openBlankSpec02.dat', BLANK='zero',&
        &FORM='formatted', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", iMsg
        CALL zzrc( 1 )
    END IF


    DO i = 1, 10
        READ(99, FMT=100, IOSTAT=iStat, IOMSG=iMsg) entry
100     FORMAT(I1,I2,I3,I4,I5,I6,I7,I8,I9,I10)

        IF (iStat <> 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 2 )
        END IF

        CALL Check(entry, 3)
    END DO


    READ(99, FMT='(10I5)', ASYNCHRONOUS='no',&
                &IOSTAT=iStat, IOMSG=iMsg, END=300) entry

    DO WHILE (iStat == 0)
        READ(99, FMT='(10I5)', ASYNCHRONOUS='yes',&
                &IOSTAT=iStat, IOMSG=iMsg, END=300) newEntry

        IF (iStat <> 0) THEN
            WRITE(0, *) "READ() <", iStat, "> ", iMsg
            CALL zzrc( 4 )
        END IF

        CALL Check(entry, 5)

        WAIT(99, IOSTAT=iStat, IOMSG=iMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "WAIT() <", iStat, "> ", iMsg
            CALL zzrc( 6 )
        END IF

        entry = newEntry
    END DO


300 CLOSE(99, IOSTAT=iStat, IOMSG=iMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "CLOSE() <", iStat, "> ", iMsg
        CALL zzrc( 7 )
    END IF

END PROGRAM openBlankSpec02


SUBROUTINE Check(e, rc)
    USE mEntry

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: rc
    TYPE(tEntry), INTENT(IN) :: e

    INTEGER :: i

    DO i = 1, 5
        IF (e%pair( i )%a .NE. e%pair( i )%a) THEN
            PRINT *, "Check() e%pair(", i, ")%a = '", e%pair( i )%a, "'"
            PRINT *, "        e%pair(", i, ")%b = '", e%pair( i )%b, "'"

            CALL zzrc( rc )
        END IF
    END DO

END SUBROUTINE Check
