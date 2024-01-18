!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar
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
!*  NOTE:  The Length Comparison in "CheckDefCharArrAlloc()" is affected
!*  by Interp d320792.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM allocArrVarScalarExpr02

    CHARACTER(3), PARAMETER :: char3Param = 'ABC'
    CHARACTER(5), PARAMETER :: char5Param1 = 'abcde'
    CHARACTER(5), PARAMETER :: char5Param2 = 'VWXYZ'
    CHARACTER(8), PARAMETER :: char8Param = 'LmNoPqRs'

    CHARACTER(5), ALLOCATABLE :: charArrAlloc( : )
    CHARACTER(:), ALLOCATABLE :: defCharArrAlloc( : )


    charArrAlloc = [ (char5Param1, i = 1, 10) ]
    CALL CheckCharArrAlloc(char5Param1, 10_4)


    charArrAlloc = char3Param
    CALL CheckCharArrAlloc((char3Param // '  '), 20_4)

    charArrAlloc = char5Param2
    CALL CheckCharArrAlloc(char5Param2, 30_4)

    charArrAlloc = char8Param
    CALL CheckCharArrAlloc(char8Param( :5 ), 40_4)


    PRINT *


    defCharArrAlloc = [ (char5Param1, i = 1, 10) ]
    CALL CheckDefCharArrAlloc(5, char5Param1, 110_4)


    defCharArrAlloc = char5Param2
    CALL CheckDefCharArrAlloc(5, char5Param2, 120_4)

    defCharArrAlloc = char3Param
    CALL CheckDefCharArrAlloc(3, char3Param, 130_4)

    defCharArrAlloc = char8Param
    CALL CheckDefCharArrAlloc(8, char8Param, 140_4) ! d320792


    CONTAINS


        SUBROUTINE CheckCharArrAlloc(pattern, failRC)
            CHARACTER(*) :: pattern
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( charArrAlloc )) CALL zzrc( failRC )

            PRINT *, failRC, SIZE( charArrAlloc ), LEN( charArrAlloc ),&
                                "'", pattern, "' (", charArrAlloc, ")"

            IF (SIZE( charArrAlloc ) /= 10)      CALL zzrc( (failRC + 1_4 ) )
            IF (LEN( charArrAlloc ) /= 5)        CALL zzrc( (failRC + 2_4 ) )
            IF (ANY((charArrAlloc /= pattern)))  CALL zzrc( (failRC + 3_4 ) )

        END SUBROUTINE CheckCharArrAlloc


        SUBROUTINE CheckDefCharArrAlloc(length, pattern, failRC)
            INTEGER :: length
            CHARACTER(*) :: pattern
            INTEGER(4) :: failRC


            IF (.NOT. ALLOCATED( defCharArrAlloc )) CALL zzrc( failRC )

            PRINT *, failRC, SIZE( defCharArrAlloc ), LEN( defCharArrAlloc ),&
                             length, "'", pattern, "' (", defCharArrAlloc, ")"

            IF (SIZE( defCharArrAlloc ) /= 10)     CALL zzrc( (failRC + 1_4 ) )
            IF (LEN( defCharArrAlloc ) /= length)  CALL zzrc( (failRC + 2_4 ) )
            IF (ANY((defCharArrAlloc /= pattern))) CALL zzrc( (failRC + 3_4 ) )

        END SUBROUTINE CheckDefCharArrAlloc

END PROGRAM allocArrVarScalarExpr02
