!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 30, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array Dummy Argument
!*                               and expr is a Scalar Dummy Argument of the
!*                               same Type as variable (CHARACTER).
!*  SECONDARY FUNCTIONS TESTED : The Actual Argument for expr is an Element
!*                               of the Actual Argument for variable.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
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
!*  If expr is a scalar and variable is an array, the expr is treated as if
!*  it were an array of the same shape as variable with every element of the
!*  array equal to the scalar value of expr.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM dummyArgsVarScalarExpr02

    CHARACTER(4) :: charArr( 10 )
    CHARACTER(4), ALLOCATABLE :: charArrAlloc( : )


    DO i = 1, 10
        WRITE(charArr( i ), '(I4)') (2000 + i)
    END DO

    charArrAlloc = charArr
    CALL Dump(charArr, 10_4)


    CALL AssignIt(charArrAlloc, charArrAlloc( 7 ))
    CALL Dump([ ('2007', i = 1, 10) ], 20_4)

    CALL AssignIt(charArrAlloc, ('2' // charArrAlloc( 7 )))
    CALL Dump([ ('2200', i = 1, 10) ], 30_4)

    CALL AssignIt(charArrAlloc, charArrAlloc( 7 )( :3 ))
    CALL Dump([ ('220 ', i = 1, 10) ], 40_4)


    CONTAINS


        SUBROUTINE Dump(pattern, failRC)
            CHARACTER(*) :: pattern( : )
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )


            PRINT *
            PRINT *, failRC, SIZE( charArrAlloc ), LEN( charArrAlloc )
            PRINT '(10("[",A5,"]"))', charArrAlloc


            IF (SIZE( charArrAlloc ) /= 10)         CALL zzrc( (failRC + 1_4) )
            IF (LEN( charArrAlloc ) /= 4)           CALL zzrc( (failRC + 2_4) )
            IF ( ANY( (charArrAlloc /= pattern) ) ) CALL zzrc( (failRC + 3_4) )

        END SUBROUTINE Dump


        SUBROUTINE AssignIt(variable, expr)
            CHARACTER(*) :: variable( : )
            CHARACTER(*) :: expr


            variable = expr

        END SUBROUTINE AssignIt

END PROGRAM dummyArgsVarScalarExpr02
