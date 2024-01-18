!*  ===================================================================
!*
!*                               Conversion
!*
!*  DATE                       : November  3, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Unallocated/Allocated ALLOCATABLE Array,
!*  SECONDARY FUNCTIONS TESTED : and expr is the Intrinsic Function SPREAD()
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 2
!*
!*  DESCRIPTION                :
!*
!*  7.4.1.1 General form
!*
!*  R734 assignment-stmt  is  variable = expr
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
!*  For a numeric intrinsic assignment statement, variable and expr may have
!*  different numeric types or different kind type parameters, in which case
!*  the value of expr is converted to the type and kind type parameter of
!*  variable according to the rules of Table 7.9.
!*
!*  Table 7.9: Numeric conversion and the assignment statement
!*  Type of variable Value Assigned
!*  integer     INT (expr, KIND = KIND (variable))
!*  real        REAL (expr, KIND = KIND (variable))
!*  complex     CMPLX (expr, KIND = KIND (variable))
!*  Note: The functions INT, REAL, CMPLX, and KIND are the generic functions
!*  defined in 13.7.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM exprIsIntrinsicFunc03

    INTEGER(2) :: intVal = 7

    INTEGER(8), ALLOCATABLE :: intArrAlloc( :,: )


    intArrAlloc = RESHAPE(SPREAD(intVal, 1, 1), [ 1,1 ])
    CALL Check(1, 1, 10_4)

    DO i = 0, 1
        m = (i + 2) * 10
        intArrAlloc = RESHAPE(SPREAD(INT(intArrAlloc, 4), 1, 5),[ 5,(5 ** i) ])
        CALL Check(5, (5 ** i), INT(m, 4))
    END DO


    CONTAINS


        SUBROUTINE Check(s1, s2, failRC)
            INTEGER :: s1
            INTEGER :: s2
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( intArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, SIZE(intArrAlloc, 1), SIZE(intArrAlloc, 2)


            IF (SIZE(intArrAlloc, 1) /= s1) CALL zzrc( (failRC + 1_4) )
            IF (SIZE(intArrAlloc, 2) /= s2) CALL zzrc( (failRC + 2_4) )


            DO j = 1, SIZE(intArrAlloc, 2)
                PRINT *, j, ')  ', intArrAlloc( :,j)
            END DO


            IF ( ANY( (intArrAlloc /= intVal) ) ) CALL zzrc( (failRC + 3_4) )

        END SUBROUTINE Check

END PROGRAM exprIsIntrinsicFunc03
