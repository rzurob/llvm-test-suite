!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : baseObjSpecExpr01 - ASYNCHRONOUS
!*                               Attribute in Scoping Unit
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : January 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Base Object appears in a
!*                               Specification Expression in a Scoping Unit
!*  SECONDARY FUNCTIONS TESTED : Any Statement of the Scoping Unit is
!*                               executed but the Variable is *NOT* a
!*                               Pending I/O Storage Sequence Affector
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : -qattr=full
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  Omit One Condition -- Intrinsic Type, implicit Attribute, matches
!*  Condition (1) only.
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
!*
!*  7.1.6 Specification expression
!*
!*  A specification expression is an expression with limitations that make
!*  it suitable for use in specifications such as length type parameters
!*  (C501) and array bounds (R512, R513).
!*
!*  R729 specification-expr  is  scalar-int-expr
!*
!*  C710 (R729) The scalar-int-expr shall be a restricted expression.
!*
!*  A restricted expression is an expression in which each operation is
!*  intrinsic and each primary is
!*
!*      ...
!*
!*      (1) A constant or subobject of a constant,
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM baseObjSpecExpr01
    IMPLICIT NONE

    INTERFACE
        SUBROUTINE AsynchronousWrite( intValue )
            IMPLICIT NONE
            INTEGER, ASYNCHRONOUS :: intValue
        END SUBROUTINE AsynchronousWrite
    END INTERFACE

    INTEGER, PARAMETER :: shortInt = 2

    !
    !  1)  The variable "shortInt" appears in a Specification Expression
    !      in the Scoping Unit
    !
    INTEGER(KIND = shortInt) :: aShortInteger


    OPEN(ASYNCHRONOUS='yes', UNIT=7)


    CALL AsynchronousWrite( shortInt )


    !
    !  2)  A Statement of the Scoping Unit is executed while the variable
    !      "shortInt" is a "Pending I/O Storage Sequence Affector".
    !
    !  ==> "shortInt" is no longer a "Pending I/O Storage Sequence Affector."
    !
    PRINT *, aShortInteger


    CLOSE( UNIT=7 )

END PROGRAM baseObjSpecExpr01


SUBROUTINE AsynchronousWrite( intValue )
    IMPLICIT NONE

    INTEGER, ASYNCHRONOUS :: intValue


    !
    !  On completion of the Asynchronous WRITE(), "intValue" becomes a
    !  "Pending I/O Storage Sequence Affector".
    !
    WRITE(7, *, ASYNCHRONOUS='yes') intValue


    !
    !  On completion of the WAIT(),"intValue" is no longer a "Pending
    !  I/O Storage Sequence Affector".
    !
    WAIT( 7 )

END SUBROUTINE AsynchronousWrite
