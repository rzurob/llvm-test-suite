!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar Result of the same
!*                               Type from a FUNCTION.
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 6
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

PROGRAM scalarFuncResultExpr02

    CHARACTER(6) :: char6Arr( 10 ) =&
        [ 'ABCDEF', 'ghijkl', 'MNOPQR', 'stuvwx', 'YZabcd',&
          'efGHIJ', 'KLmnop', 'qrSTUV', 'WXyzAB', 'CDEFGH' ]

    CHARACTER(:), POINTER :: charArrPtr( : )

    CHARACTER(6), ALLOCATABLE, TARGET :: char6ArrAlloc( : )
    CHARACTER(:), ALLOCATABLE, TARGET :: defArrAlloc( : )


    char6ArrAlloc = char6Arr
    charArrPtr => char6ArrAlloc
    CALL Dump(char6Arr, 6, 10, 10_4)

    char6ArrAlloc = ScalarExpr( '654321' )
    charArrPtr => char6ArrAlloc
    CALL Dump([ ('654321', i = 1, 10) ], 6, 10, 20_4)

    char6ArrAlloc = ScalarExpr( 'TesT' )
    charArrPtr => char6ArrAlloc
    CALL Dump([ ('TesT  ', i = 1, 10) ], 6, 10, 30_4)

    char6ArrAlloc = ScalarExpr( '12345678' )
    charArrPtr => char6ArrAlloc
    CALL Dump([ ('123456', i = 1, 10) ], 6, 10, 40_4)


    defArrAlloc = char6Arr
    charArrPtr => defArrAlloc
    CALL Dump(char6Arr, 6, 10, 110_4)

    defArrAlloc = ScalarExpr( '654321' )
    charArrPtr => defArrAlloc
    CALL Dump([ ('654321', i = 1, 10) ], 6, 10, 120_4)

    defArrAlloc = ScalarExpr( 'TesT' )
    charArrPtr => defArrAlloc
    CALL Dump([ ('TesT', i = 1, 10) ], 4, 10, 130_4)       ! See Defect 320792

    defArrAlloc = ScalarExpr( '12345678' )
    charArrPtr => defArrAlloc
    CALL Dump([ ('12345678', i = 1, 10) ], 8, 10, 140_4)   ! See Defect 320792


    CONTAINS


        SUBROUTINE Dump(p, l, s, failRC)
            CHARACTER(*) :: p( : )
            INTEGER :: l
            INTEGER :: s
            INTEGER(4) :: failRC


            IF ( ASSOCIATED(charArrPtr, char6ArrAlloc) ) THEN
                IF (.NOT. ALLOCATED( char6ArrAlloc ))&
                                    CALL zzrc( (failRC + 1_4) )

            ELSE IF ( ASSOCIATED(charArrPtr, defArrAlloc) ) THEN
                IF (.NOT. ALLOCATED( defArrAlloc ))&
                                    CALL zzrc( (failRC + 2_4) )

            ELSE
                CALL zzrc( (failRC + 3_4) )
            END IF


            PRINT *
            PRINT *, failRC, LEN( charArrPtr ), SIZE( charArrPtr )

            DO i = 1, SIZE( charArrPtr )
                PRINT *, i, '"', charArrPtr( i ), "' (", p( i ), ")"
            END DO


            IF ( ANY( (charArrPtr /= p) ) ) CALL zzrc( (failRC + 4_4) )
            IF (LEN( charArrPtr ) /= l)     CALL zzrc( (failRC + 5_4) )
            IF (SIZE( charArrPtr ) /= s)    CALL zzrc( (failRC + 6_4) )

        END SUBROUTINE Dump


        FUNCTION ScalarExpr( c )
            CHARACTER(*) :: c

            CHARACTER(:), ALLOCATABLE :: ScalarExpr


            ScalarExpr = c

        END FUNCTION ScalarExpr

END PROGRAM scalarFuncResultExpr02
