!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : openActionSpec02 - ASYNCHRONOUS=
!*                               Specifier in I/O Statements
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : February 21, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : ACTION=Write Specifier in OPEN() Statement
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

MODULE mBase
    TYPE tBase
        INTEGER :: base1
        INTEGER :: base2
    END TYPE tBase
END MODULE mBase


MODULE mDerived1
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived1
        INTEGER :: derived1
        INTEGER :: derived2
    END TYPE tDerived1
END MODULE mDerived1


MODULE mDerived2
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived2
        INTEGER :: derived1
        INTEGER :: derived2
    END TYPE tDerived2
END MODULE mDerived2


PROGRAM openActionSpec02
    USE mDerived1
    USE mDerived2

    TYPE(tBase), TARGET :: b1 = tBase(1,10)
    TYPE(tDerived1), TARGET :: d1 = tDerived1(2,20, 200,100)
    TYPE(tDerived2), TARGET :: d2 = tDerived2(3,30, 300,150)

    INTEGER, DIMENSION( 3 ) :: idA
    LOGICAL :: aPending
    CHARACTER(LEN = 256) :: oMsg


    ioUnit = 75
    OPEN(UNIT=ioUnit, FILE='openActionSpec02.dat', FORM='formatted',&
        &ACTION='write', ASYNCHRONOUS='yes', IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "OPEN() <", iStat, "> ", oMsg
        CALL zzrc( 1 )
    END IF


    CALL DumpClass(75, idA( 1 ), 'b1', b1)
    CALL DumpClass(75, idA( 2 ), 'd1', d1)
    CALL DumpClass(75, idA( 3 ), 'd2', d2)


    DO 200 i = 1, 3
        INQUIRE(ioUnit, ID=idA( i ), PENDING=aPending, IOSTAT=iStat, IOMSG=oMsg)
        IF (iStat <> 0) THEN
            WRITE(0, *) "INQUIRE() <", iStat, "> ", oMsg
            CALL zzrc( 3 )
        END IF


        WRITE(ioUnit, FMT=100, ASYNCHRONOUS='no',&
                &IOSTAT=iStat, IOMSG=oMsg) i, idA( i ), aPending
100     FORMAT(I1,') ID="',I3,'" Pending = "',L1,'"')

        IF (iStat <> 0) THEN
            WRITE(0, *) "WRITE() <", iStat, "> ", oMsg
            CALL zzrc( 4 )
        END IF
200 CONTINUE


    CLOSE(ioUnit, IOSTAT=iStat, IOMSG=oMsg)
    IF (iStat <> 0) THEN
        WRITE(0, *) "INQUIRE() <", iStat, "> ", oMsg
        CALL zzrc( 5 )
    END IF

    CONTAINS

        SUBROUTINE DumpClass(ioUnit, aID, varName, poly)

            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, INTENT(OUT) :: aID
            CHARACTER(LEN = 2) :: varName
            CLASS(tBase), INTENT(IN) :: poly

            CHARACTER(LEN = 256) :: oMsg


            WRITE(ioUnit, FMT=200, ASYNCHRONOUS='yes', ID=aID,&
                    &IOSTAT=iStat, IOMSG=oMsg) varName, poly%base1, poly%base2
200         FORMAT(a2,'(base1=',I1,',base2=',I2,')')

            IF (iStat <> 0) THEN
                WRITE(0, *) "WRITE() <", iStat, "> ", oMsg
                CALL zzrc( 2 )
            END IF
        END SUBROUTINE

END PROGRAM openActionSpec02
