!*  ===================================================================
!*
!*                               Attribute in Scoping Unit
!*
!*  DATE                       : January 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Base Object appears in a
!*                               Specification Expression in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed while the Variable is a Pending
!*                               I/O Storage Sequence Affector
!*
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Baseline Test Case #1 -- Intrinsic Type, explicit Attribute, matches
!*  both conditions.
!*
!*  5.1.2.3  ASYNCHRONOUS Attribute
!*
!*  The base object of a variable shall have the ASYNCHRONOUS attribute in
!*  a scoping unit if:
!*
!*  (1) the variable appears in an executable statement or specification
!*      expression in that scoping unit and
!*
!*  (2) any statement of the scoping unit is executed while the variable is
!*      a pending I/O storage sequence affector (9.5.1.4)
!*
!*  7.1.6 Specification expression
!*
!*  A specification expression is an expression with limitations that make it
!*  suitable for use in specifications such as length type parameters (C501)
!*  and array bounds (R512, R513).
!*
!*  R729 specification-expr  is  scalar-int-expr
!*
!*  C710 (R729) The scalar-int-expr shall be a restricted expression.
!*
!*  A restricted expression is an expression in which each operation is
!*  intrinsic and each primary is
!*
!*      (1) A constant or subobject of a constant,
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM specExprPending01
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE AsynchronousWrite(ioUnit, cSize, cVar)
            IMPLICIT NONE

            INTEGER, INTENT(IN) :: ioUnit
            INTEGER, ASYNCHRONOUS, INTENT(IN) :: cSize
            CHARACTER(LEN = cSize) :: cVar
        END SUBROUTINE AsynchronousWrite
    END INTERFACE

    INTEGER, PARAMETER :: ioUnit = 78
    INTEGER, ASYNCHRONOUS, PARAMETER :: charSize = 30

    !
    !  1)  The variable "charSize" appears in a "Specification Expression"
    !      in the scoping unit.
    !
    CHARACTER(LEN = charSize) :: charVar


    charVar = "a character variable"


    OPEN(UNIT=ioUnit, ASYNCHRONOUS='yes', ERR=100)

    CALL AsynchronousWrite(ioUnit, charSize, charVar)


    !
    !  2)  Any statement of the scoping unit is executed while the
    !      variable "charSize" is a "Pending I/O Storage Sequence
    !      Affector".
    !
    PRINT *, "I/O Unit = (", ioUnit, ")"


    CLOSE(ioUnit, ERR=200)
    GOTO 300


    !
    !  Handle OPEN() Error
    !
100 CONTINUE
    PRINT *, "OPEN():  failed"
    CALL zzrc( 1 )
    GOTO 300


    !
    !  Handle CLOSE() Error
    !
200 CONTINUE
    PRINT *, "CLOSE():  failed"
    CALL zzrc( 3 )


300 CONTINUE

END PROGRAM specExprPending01


SUBROUTINE AsynchronousWrite(ioUnit, cSize, cVar)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ioUnit
    INTEGER, ASYNCHRONOUS, INTENT(IN) :: cSize

    CHARACTER(LEN = cSize) :: cVar


    !
    !  On successful completion of the Asynchronous WRITE(), "cSize"
    !  will be a "Pending I/O Storage Sequence Affector".
    !
    WRITE(FMT=125, ASYNCHRONOUS='yes', ERR=150, UNIT=ioUnit) cSize, cVar
    GOTO 175

125 FORMAT('Size = (',i2,'), Value = "',a30,'"')

    !
    !  Handle WRITE() Error
    !
150 CONTINUE
    PRINT *, "WRITE(ASYNCHRONOUS=yes):  failed"
    CALL zzrc( 2 )


175 CONTINUE

END SUBROUTINE AsynchronousWrite
