!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : September 27, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of CHARACTER
!*                               Intrinsic Type
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

PROGRAM arrayExprVectorIndex04

    CHARACTER(54), PARAMETER :: charVar =&
        '1abcdefghijklmnopqrstuvwxyz2ABCDEFGHIJKLMNOPQRSTUVWXYZ'

    CHARACTER(5), ALLOCATABLE :: charArrAlloc( :,: )


    ALLOCATE( charArrAlloc( 10,10 ) )


    charArrAlloc = RESHAPE(&
        (/ ((charVar( idx(i, j):(idx(i, j) + 4) ), j = 0, 9), i = 0, 9) /),&
                                                                 (/ 10,10 /))


    PRINT *, 'Before:'
    PRINT 10, charArrAlloc
10  FORMAT(10("'",A5,"',"))

    PRINT *
    PRINT 20, charArrAlloc( (/ (i, i = 3, 9, 3) /),(/ 1,5,9 /) )
20  FORMAT(3("'",A5,"',"))


    charArrAlloc =&
        charArrAlloc( (/ (i, i = 3, 9, 3) /),(/ 1,5,9 /) )( 5:5 ) //&
        charArrAlloc( (/ (i, i = 3, 9, 3) /),(/ 1,5,9 /) )( 4:4 ) //&
        charArrAlloc( (/ (i, i = 3, 9, 3) /),(/ 1,5,9 /) )( 3:3 ) //&
        charArrAlloc( (/ (i, i = 3, 9, 3) /),(/ 1,5,9 /) )( 2:2 ) //&
        charArrAlloc( (/ (i, i = 3, 9, 3) /),(/ 1,5,9 /) )( 1:1 )


    PRINT *
    PRINT *, 'After:'
    PRINT 20, charArrAlloc


    CONTAINS

        INTEGER FUNCTION idx(i, j)
            idx = MOD(((i * 10) + j), 50) + 1
        END FUNCTION idx

END PROGRAM arrayExprVectorIndex04
