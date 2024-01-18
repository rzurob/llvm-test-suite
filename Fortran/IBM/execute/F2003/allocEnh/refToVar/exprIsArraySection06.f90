!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : exprIsArraySection06 - variable and/or
!*                               expr Contain References to variable
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 19, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               of Deferred Length
!*  SECONDARY FUNCTIONS TESTED : expr is an Array Section of variable
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 4
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
!*
!*
!*  7.4.1.3 Interpretation of intrinsic assignments
!*
!*  If variable is an allocated allocatable variable, it is deallocated if
!*  expr is an array of different shape or any of the corresponding length
!*  type parameter values of variable and expr differ. If variable is or
!*  becomes an unallocated allocatable variable, then it is allocated with
!*  each deferred type parameter equal to the corresponding type parameters
!*  of expr, with the shape of expr, and with each lower bound equal to the
!*  corresponding element of LBOUND(expr).
!*
!*  Both variable and expr may contain references to any portion of variable.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsArraySection06

    INTEGER :: lower = 0
    INTEGER :: upper = 0

    INTEGER(4) :: failRC = 10_4

    CHARACTER(:), ALLOCATABLE :: charArrAlloc( : )


    charArrAlloc = (/ (CHAR( (IACHAR( 'A' ) + i) ), i = 0, 25) /)


    CALL Dump(lower, upper, failRC)
    DO WHILE (SIZE( charArrAlloc ) > 1)
        lower = SIZE( charArrAlloc ) / 2
        IF ((lower * 2) < SIZE( charArrAlloc )) THEN
            upper = lower + 2

        ELSE IF ((lower * 2) > SIZE( charArrAlloc )) THEN
            lower = lower - 1
            upper = lower + 2

        ELSE
            upper = lower + 1
        END IF


        charArrAlloc = charArrAlloc( :lower ) // charArrAlloc( upper: )


        failRC = failRC + 10_4
        CALL Dump(lower, upper, failRC)
    END DO


    PRINT *


    lower = 0
    upper = 0

    failRC = failRC + 10_4
    CALL Dump(lower, upper, failRC)
    DO WHILE (LEN( charArrAlloc( 1 ) ) > 1)
        lower = LEN( charArrAlloc( 1 ) ) / 2
        upper = lower + 1


        charArrAlloc = (/ charArrAlloc( : )( :lower ),&
                          charArrAlloc( : )( upper: ) /)


        failRC = failRC + 10_4
        CALL Dump(lower, upper, failRC)
    END DO


    CONTAINS


        SUBROUTINE Dump(l, u, f)
            INTEGER :: l
            INTEGER :: u
            INTEGER(4) :: f

            CHARACTER(44) :: fmt


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( f )


            WRITE(fmt, 10) SIZE( charArrAlloc ), LEN( charArrAlloc )
10          FORMAT('("l = ",I2,", u = ",I2," (",',I2,'(A',I2,',","),")")')

            PRINT fmt, l, u, charArrAlloc
! fmt  ==   FORMAT("l = ",I2,", u = ",I2," (",26(A 1,","),")")

        END SUBROUTINE Dump

END PROGRAM exprIsArraySection06
