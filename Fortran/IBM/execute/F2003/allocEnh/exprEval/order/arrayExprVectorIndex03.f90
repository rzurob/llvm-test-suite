!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : September 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type (Indexed using a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable (Indexed using
!*                               a Vector Subscript), and will have a
!*                               different Shape Result
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

PROGRAM arrayExprVectorIndex03

    INTEGER :: idx1( 5 ) = (/ (i, i = 1,  9, 2) /)
    INTEGER :: idx2( 5 ) = (/ (i, i = 2, 10, 2) /)

    COMPLEX, ALLOCATABLE :: complexArrayAlloc( : )


    ALLOCATE(complexArrayAlloc( 10 ),&
                SOURCE=(/ (CMPLX(i , (11 - i)), i = 1, 10) /))


    PRINT *, 'Before:'
    PRINT 10, complexArrayAlloc
10  FORMAT(5("(",F4.1,",",F4.1,"), "))


    complexArrayAlloc( idx1 ) = complexArrayAlloc( idx2 ) * 2


    PRINT *
    PRINT *, 'After:'
    PRINT 10, complexArrayAlloc

END PROGRAM arrayExprVectorIndex03
