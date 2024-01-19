!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : August  2, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Scalar/Array of
!*                               Intrinsic (Non-CHARACTER) Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar/Array of Intrinsic Type
!*                               with different Length Type Parameter Values
!*                               (but the same Shape)
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

PROGRAM unAllocatedAllocatable03

    INTEGER(4) :: i
    INTEGER(4) :: j
    INTEGER(4) :: k
    INTEGER(4) :: l

    INTEGER(8), ALLOCATABLE :: intScalarAlloc
    REAL(8), ALLOCATABLE :: realArrayAlloc( :,:,: )


    IF ( ALLOCATED( intScalarAlloc ) ) CALL zzrc( 10_4 )
    IF ( ALLOCATED( realArrayAlloc ) ) CALL zzrc( 20_4 )


    realArrayAlloc = RESHAPE((/ (REAL(i, 16), i = 1, 60) /), (/ 3,4,5 /))
    intScalarAlloc = 42_2


    IF (.NOT. ALLOCATED( intScalarAlloc ))  CALL zzrc( 30_4 )
    IF (KIND( intScalarAlloc ) /= 8)        CALL zzrc( 40_4 )
    IF (intScalarAlloc /= 42_8)             CALL zzrc( 50_4 )


    IF (.NOT. ALLOCATED( realArrayAlloc ))  CALL zzrc( 50_4 )

    IF (SIZE( realArrayAlloc ) /= 60)       CALL zzrc( 70_4 )
    IF (SIZE(realArrayAlloc, 1) /= 3)       CALL zzrc( 80_4 )
    IF (SIZE(realArrayAlloc, 2) /= 4)       CALL zzrc( 90_4 )
    IF (SIZE(realArrayAlloc, 3) /= 5)       CALL zzrc( 100_4 )


    DO i = 1,5
        DO j = 1,4
            DO k = 1,3
                l = ((i - 1) * 12) + ((j - 1) * 3) + k
                IF (realArrayAlloc( k,j,i ) /= REAL(l, 8)) THEN
                    PRINT *, "REAL(", l, ",8) =", REAL(l, 8)
                    PRINT *, "realArrayAlloc(", k, ",", j, ",", i, ") =",&
                                realArrayAlloc( k,j,i )

                    CALL zzrc( (110_4 + l) )
                END IF
            END DO
        END DO
    END DO


END PROGRAM unAllocatedAllocatable03
