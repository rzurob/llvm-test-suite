!*  ===================================================================
!*
!*                               Evaluation
!*
!*  DATE                       : September 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Intrinsic
!*                               Type (Indexed using a Vector Subscript)
!*  SECONDARY FUNCTIONS TESTED : expr references variable, and will have a
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

PROGRAM arrayExprVectorIndex02

    INTEGER :: idx( 5 ) = (/ 1, 3, 5, 7, 9 /)
    REAL, ALLOCATABLE :: realArrayAlloc( :,: )


    ALLOCATE(realArrayAlloc( 10,10 ), SOURCE=RESHAPE(&
            (/ ((REAL( (REAL( j ) + (REAL( i ) / 100)) ),&
                    j = 1, 10), i = 1, 10) /), (/ 10,10 /)))


    PRINT *, "Before:  realArrayAlloc( 1:5,1:5 )"
    PRINT 10, realArrayAlloc( 1:5,1:5 )
10  FORMAT(5(F5.2,", "))

    PRINT *
    PRINT *, "Before:  realArrayAlloc"
    PRINT 20, realArrayAlloc
20  FORMAT(10(F5.2,", "))

    realArrayAlloc(&
        (/ (INT( realArrayAlloc( i,i ) ), i = 1, 9, 2) /),&
        (/ (INT( realArrayAlloc( i,i ) ), i = 1, 9, 2) /) )&
                                        = realArrayAlloc * 2.0

    PRINT *
    PRINT *, "After:  realArrayAlloc"
    PRINT 20, realArrayAlloc


    PRINT *
    PRINT *, "After:  realArrayAlloc( [13579],[13579] )"
    PRINT 10, realArrayAlloc(&
        (/ (INT( realArrayAlloc( i,i ) ), i = 1, 9, 2) /),&
        (/ (INT( realArrayAlloc( i,i ) ), i = 1, 9, 2) /) )

END PROGRAM arrayExprVectorIndex02
