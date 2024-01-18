!*  ===================================================================
!*
!*                               Specifier in I/O Statements
!*
!*  DATE                       : February 17, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ASYNCHRONOUS= Specifier in OPEN() Statement
!*  SECONDARY FUNCTIONS TESTED : ACCESS=Sequential Specifier in OPEN()
!*                               Statement
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
    TYPE tBase
        INTEGER :: i1
    END TYPE tBase
END MODULE mBase


MODULE mExtend
    USE mBase

    TYPE, EXTENDS(tBase) :: tExtend
        INTEGER, DIMENSION( 71 ) :: i2
    END TYPE tExtend
END MODULE mExtend


PROGRAM openAccessSpec01
    USE mExtend

    CHARACTER(LEN = 9) acc
    CHARACTER(LEN = 3) asyn

    TYPE(tExtend), DIMENSION( 73 ) :: ext


    iVal = (73 * 72)
    DO i = 1, 73
        ext( i )%i1 = iVal
        iVal = iVal - 1

        DO j = 1, 71
            ext( i )%i2( j ) = iVal
            iVal = iVal - 1
        END DO
    END DO


    asyn = 'yes'
    acc = 'sequential'
    ioUnit = 512

    OPEN(ioUnit, ACCESS=acc, ASYNCHRONOUS=asyn)

    DO 20 i = 1, 73
20       WRITE(ioUnit, 30, ASYNCHRONOUS='yes') i, ext( i )
30  FORMAT(73I5)

    CLOSE(ioUnit)

END PROGRAM openAccessSpec01
