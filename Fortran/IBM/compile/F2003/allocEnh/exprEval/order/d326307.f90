!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : d326181 - Order of Expression Evaluation
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October  5, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : F2003: Syntax Error in Type Guard Statement
!*                               causes ICE in xlfcode
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SELECT TYPE, TYPE IS
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
!*
!*  DESCRIPTION                :
!*  The Reduced Code below has an incorrectly formatted Type Guard Statement.
!*  A number of Diagnostic Messages are emitted when this code is compiled,
!*  and then the Compiler ICEs.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM deferredArrExprVectIdx01

    CLASS(*), POINTER :: intPtr
    INTEGER, TARGET :: intVar

    intPtr => intVar

    SELECT TYPE ( intPtr )
!        TYPE IS (INTEGER)
        TYPE IS  INTEGER
            PRINT *, "INTEGER"
    END SELECT

END PROGRAM deferredArrExprVectIdx01
