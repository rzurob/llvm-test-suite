!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : dummyArgsVarScalarExpr01 - expr is a
!*                               Scalar and variable is an Array
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 30, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array Dummy Argument
!*                               and expr is a Scalar Dummy Argument of the
!*                               same Type as variable.
!*  SECONDARY FUNCTIONS TESTED : The Actual Argument for expr is an Element
!*                               of the Actual Argument for variable.
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
!*  If expr is a scalar and variable is an array, the expr is treated as if
!*  it were an array of the same shape as variable with every element of the
!*  array equal to the scalar value of expr.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM dummyArgsVarScalarExpr01

    INTERFACE
        SUBROUTINE AssignIt(variable, expr, test)
            REAL, ALLOCATABLE :: variable( :,: )
            REAL :: expr
            INTEGER :: test
        END SUBROUTINE AssignIt
    END INTERFACE

    REAL, ALLOCATABLE :: realArrAlloc( :,: )


    CALL InitIt( )
    CALL DumpIt( 10_4 )

    CALL AssignIt(realArrAlloc, realArrAlloc( 5,5 ), 0)
    CALL DumpIt( 20_4 )


    CALL InitIt( )
    CALL DumpIt( 30_4 )

    CALL AssignIt(realArrAlloc, (realArrAlloc( 5,5 ) + realArrAlloc( 8,8 )), 0)
    CALL DumpIt( 40_4 )


    CALL InitIt( )
    CALL DumpIt( 50_4 )

    CALL AssignIt(realArrAlloc, realArrAlloc( 5,5 ), 1)
    CALL DumpIt( 60_4 )


    CALL InitIt( )
    CALL DumpIt( 70_4 )

    CALL AssignIt(realArrAlloc, (realArrAlloc( 5,5 ) + realArrAlloc( 2,2 )), 2)
    CALL DumpIt( 80_4 )


    CONTAINS


        SUBROUTINE InitIt( )

            realArrAlloc =&
                RESHAPE([ (((REAL( j ) + (REAL( i ) / 10.)),&
                                j = 0, 9), i = 0, 9) ], [ 10,10 ])

        END SUBROUTINE InitIt


        SUBROUTINE DumpIt( failRC )
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( realArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, SIZE(realArrAlloc, 1), SIZE(realArrAlloc, 2)
            DO i = 1, SIZE(realArrAlloc, 1)
                PRINT '(10(F5.1))', realArrAlloc( i,: )
            END DO


            IF (SIZE(realArrAlloc, 1) /= 10) CALL zzrc( (failRC + 1_4) )
            IF (SIZE(realArrAlloc, 2) /= 10) CALL zzrc( (failRC + 2_4) )

        END SUBROUTINE DumpIt

END PROGRAM dummyArgsVarScalarExpr01


SUBROUTINE AssignIt(variable, expr, test)
    REAL, ALLOCATABLE :: variable( :,: )
    REAL :: expr
    INTEGER :: test


    IF (test == 0) THEN
        variable = expr

    ELSE IF (test == 1) THEN
        variable = expr + variable( 9,9 )

    ELSE IF (test == 2) THEN
        variable = expr - variable( 7,8 )
    END IF

END SUBROUTINE AssignIt
