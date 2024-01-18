!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : August  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Intrinsic
!*                               (Non-CHARACTER) Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for a single Dimension
!*                               Array
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

PROGRAM unAllocatedLBound01

    INTEGER(4) :: i

    LOGICAL, PARAMETER :: trueFalse( 2 ) = (/ .TRUE., .FALSE. /)
    LOGICAL, PARAMETER :: logArray( 96:105 ) = (/ (trueFalse, i = 1, 10, 2) /)

    LOGICAL, ALLOCATABLE :: logArrayAlloc( : )


    IF ( ALLOCATED( logArrayAlloc ) ) CALL zzrc( 10_4 )


    logArrayAlloc = logArray


    IF (.NOT. ALLOCATED( logArrayAlloc )) CALL zzrc( 20_4 )

    PRINT *, SIZE( logArrayAlloc )
    IF (SIZE( logArrayAlloc ) /= 10) CALL zzrc( 30_4 )

    PRINT *, LBOUND( logArrayAlloc )
    IF (LBOUND(logArrayAlloc, 1) /= 96) CALL zzrc( 40_4 )


    DO i = 96, 105
        IF (logArrayAlloc( i ) .NEQV. trueFalse( MOD((i - 95), 2) )) THEN
            PRINT *, "logArrayAlloc(", i, ") =", logArrayAlloc( i )
            PRINT *, "Expected:", trueFalse( MOD((i - 95), 2) )

            CALL zzrc( (50_4 + i) )
        END IF
    END DO

END PROGRAM unAllocatedLBound01
