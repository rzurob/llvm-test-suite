!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedLBound02 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : July 28, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE of Intrinsic
!*                               (Non-CHARACTER) Type and
!*  SECONDARY FUNCTIONS TESTED : LBOUND(expr) returns a value other than "1"
!*                               as the Lower Bound for an N-Dimension Array
!*                               (1<= N <= 7) -- N == 2
!*
!*  DRIVER STANZA              : xlf2003
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

PROGRAM allocatedLBound02

    INTEGER(4) :: k

    LOGICAL(2) :: logArray2( 5:6,3:4 ) =&
        RESHAPE((/ (LOGICAL((MOD(i, 2) == 0), 2), i = 0, 3) /), (/ 2, 2 /))
    LOGICAL(2), ALLOCATABLE :: logAllocArray2( :,: )


    ALLOCATE(logAllocArray2( 0:3,5:5 ),&
            SOURCE=RESHAPE((/ (.TRUE._2, i = 1, 4) /), (/ 4, 1 /)))

    IF (.NOT. ALLOCATED( logAllocArray2 )) CALL zzrc( 10_4 )

    IF (LBOUND(logAllocArray2, 1) /= 0) CALL zzrc( 20_4 )
    IF (LBOUND(logAllocArray2, 2) /= 5) CALL zzrc( 30_4 )


    logAllocArray2 = logArray2


    IF (.NOT. ALLOCATED( logAllocArray2 )) CALL zzrc( 40_4 )

    IF (SIZE( logAllocArray2 ) /= 4) CALL zzrc( 50_4 )
    IF (SIZE(logAllocArray2, 1) /= 2) CALL zzrc( 60_4 )
    IF (SIZE(logAllocArray2, 2) /= 2) CALL zzrc( 70_4 )


    IF (LBOUND(logAllocArray2, 1) /= 5) THEN
        PRINT *, "LBOUND(logAllocArray2, 1) =", LBOUND(logAllocArray2, 1)
        CALL zzrc( 80_4 )

    ELSE IF (LBOUND(logAllocArray2, 2) /= 3) THEN
        PRINT *, "LBOUND(logAllocArray2, 2) =", LBOUND(logAllocArray2, 2)
        CALL zzrc( 90_4 )
    END IF


    k = 0
    DO j = 3, 4
        DO i = 5, 6
            IF (logAllocArray2( i,j ) .NEQV.&
                LOGICAL((MOD(k, 2) == 0), 2)) CALL zzrc( (100_4 + k) )

            k = k + 1
        END DO
    END DO

END PROGRAM allocatedLBound02
