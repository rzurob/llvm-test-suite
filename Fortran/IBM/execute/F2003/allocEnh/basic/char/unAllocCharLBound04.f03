!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : September 13, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Type
!*                               CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 6
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

PROGRAM unAllocCharLBound04

    CHARACTER(7) :: chrArr( -5:-4,-4:-3,-3:-2,-2:-1,-1:0,0:1 ) =&
            RESHAPE((/ ('abcDEFg', i = 1, 64) /), (/ 2,2,2,2,2,2 /))

    CHARACTER(7), ALLOCATABLE :: chrArrAlloc( :,:,:,:,:,: )


    IF ( ALLOCATED( chrArrAlloc ) ) CALL zzrc( 10_4 )


    chrArrAlloc = chrArr


    IF (.NOT. ALLOCATED( chrArrAlloc )) CALL zzrc( 20_4 )

    PRINT *, SIZE( chrArrAlloc )
    IF (SIZE( chrArrAlloc ) /= 64)      CALL zzrc( 30_4 )


    PRINT *

    DO i = 1_4, 6_4
        PRINT *, i, SIZE(chrArrAlloc, i), LBOUND(chrArrAlloc, i)

        IF (SIZE(chrArrAlloc, i) /= 2)          CALL zzrc( (40_4 + i) )
        IF (LBOUND(chrArrAlloc, i) /= (i - 6))  CALL zzrc( (50_4 + i) )
    END DO


    PRINT *
    PRINT *, "chrArrAlloc:"
    PRINT 10, chrArrAlloc
10  FORMAT(4(" '",A7,"',"))

    IF (.NOT. ALL(chrArrAlloc == 'abcDEFg')) CALL zzrc( 60_4 )

END PROGRAM unAllocCharLBound04