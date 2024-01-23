!*  ===================================================================
!*
!*                               Scalar and variable is an Array
!*
!*  DATE                       : October 25, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of an Intrinsic
!*                               Type,
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 5
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

PROGRAM allocArrVarScalarExpr01

    INTEGER, PARAMETER :: intScalar = 37
    INTEGER, ALLOCATABLE :: intArrAlloc( :,: )

    REAL, PARAMETER :: realScalar = 1.34
    REAL, ALLOCATABLE :: realArrAlloc( :,:,: )

    DOUBLE PRECISION, PARAMETER :: doubleScalar = 3.14Q-01
    DOUBLE PRECISION, ALLOCATABLE :: doubleArrAlloc( :,: )

    COMPLEX, PARAMETER :: complexScalar = (1.23,4.56)
    COMPLEX, ALLOCATABLE :: complexArrAlloc( : )

    LOGICAL, PARAMETER :: logicalScalar = .TRUE.
    LOGICAL, ALLOCATABLE :: logicalArrAlloc( :,: )


    intArrAlloc = RESHAPE([ (i, i = 1, 100)], [ 10,10 ])
    IF (.NOT. ALLOCATED( intArrAlloc )) ERROR STOP 10_4

    intArrAlloc = intScalar

    PRINT *, SIZE(intArrAlloc, 1), SIZE(intArrAlloc, 2)
    IF (.NOT. ALLOCATED( intArrAlloc ))         ERROR STOP 11_4
    IF (SIZE(intArrAlloc, 1) /= 10)             ERROR STOP 12_4
    IF (SIZE(intArrAlloc, 2) /= 10)             ERROR STOP 13_4
    IF ( ANY( (intArrAlloc /= intScalar) ) )    ERROR STOP 14_4


    realArrAlloc = RESHAPE([ ((((REAL( k ) + (REAL( j ) / 10.0)),&
                        k = 1, 10), j = 1, 10), i = 1, 10) ], [ 10,10,10 ])
    IF (.NOT. ALLOCATED( realArrAlloc )) ERROR STOP 20_4

    realArrAlloc = realScalar

	PRINT *
    PRINT *,SIZE(realArrAlloc, 1),SIZE(realArrAlloc, 2),SIZE(realArrAlloc, 3)
    IF (.NOT. ALLOCATED( realArrAlloc ))        ERROR STOP 21_4
    IF (SIZE(realArrAlloc, 1) /= 10)            ERROR STOP 22_4
    IF (SIZE(realArrAlloc, 2) /= 10)            ERROR STOP 23_4
    IF (SIZE(realArrAlloc, 3) /= 10)            ERROR STOP 24_4
    IF ( ANY( (realArrAlloc /= realScalar) ) )  ERROR STOP 25_4


    doubleArrAlloc = RESHAPE([ (((REAL(j, 16) / REAL(i, 16)),&
                                    j = 1, 10), i = 1, 10) ], [ 10,10 ])
    IF (.NOT. ALLOCATED( doubleArrAlloc )) ERROR STOP 30_4

    doubleArrAlloc = doubleScalar

	PRINT *
    PRINT *, SIZE(doubleArrAlloc, 1), SIZE(doubleArrAlloc, 2)
    IF (.NOT. ALLOCATED( doubleArrAlloc ))          ERROR STOP 31_4
    IF (SIZE(doubleArrAlloc, 1) /= 10)              ERROR STOP 32_4
    IF (SIZE(doubleArrAlloc, 2) /= 10)              ERROR STOP 33_4
    IF ( ANY( (doubleArrAlloc /= doubleScalar) ) )  ERROR STOP 34_4


    complexArrAlloc = [ (CMPLX(i,i), i = 1, 10) ]
    IF (.NOT. ALLOCATED( complexArrAlloc )) ERROR STOP 40_4

    complexArrAlloc = complexScalar

	PRINT *
    PRINT *, SIZE( complexArrAlloc )
    IF (.NOT. ALLOCATED( complexArrAlloc ))          ERROR STOP 41_4
    IF (SIZE( complexArrAlloc ) /= 10)               ERROR STOP 42_4
    IF ( ANY( (complexArrAlloc /= complexScalar) ) ) ERROR STOP 43_4


    logicalArrAlloc = RESHAPE([ ((.FALSE.,j = 1, 10),i = 1, 10) ],[ 10,10 ])
    IF (.NOT. ALLOCATED( logicalArrAlloc )) ERROR STOP 50_4

    logicalArrAlloc = logicalScalar

	PRINT *
    PRINT *, SIZE(logicalArrAlloc, 1), SIZE(logicalArrAlloc, 2)
    IF (.NOT. ALLOCATED( logicalArrAlloc ))              ERROR STOP 51_4
    IF (SIZE(logicalArrAlloc, 1) /= 10)                  ERROR STOP 52_4
    IF (SIZE(logicalArrAlloc, 2) /= 10)                  ERROR STOP 53_4
    IF ( ANY( (logicalArrAlloc .NEQV. logicalScalar) ) ) ERROR STOP 54_4


END PROGRAM allocArrVarScalarExpr01
