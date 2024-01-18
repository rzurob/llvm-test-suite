!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : September 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : expr references variable (Indexed using a
!*                               Vector Subscript), and will have a different
!*                               Shape Result
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ALLOCATABLE Attribute, Intrinsic Assignment
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 1
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
!234567890123456789012345678901234567890123456789012345678901234567890

PROGRAM arrayExprVectorIndex01

    INTEGER :: idx( 5 ) = (/ 1, 3, 5, 7, 9 /)
    INTEGER :: intArray( 5 ) = (/ (i, i = -2, -10, -2) /)
    INTEGER, ALLOCATABLE :: intArrayAlloc( : )


    ALLOCATE(intArrayAlloc( 10 ), SOURCE=(/ (i, i = 1, 10) /))

    PRINT 10, 'Before:', 'intArrayAlloc', intArrayAlloc
10  FORMAT(A7,'  ',A13,' = (',9(I2,','),I3,')')

    PRINT 20, ' ', 'intArray', intArray
20  FORMAT(A7,'  ',A13,' = (',4(I2,','),I3,')')


    intArrayAlloc = intArrayAlloc( idx ) + intArray

    PRINT 20, 'After:', 'intArrayAlloc', intArrayAlloc

END PROGRAM arrayExprVectorIndex01
