!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : July 27, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar/Array of
!*                               Intrinsic Type
!*  SECONDARY FUNCTIONS TESTED : with different Length Type Parameters from
!*                               expr (but does have the same Shape as expr)
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

PROGRAM allocatedAllocatable03

    INTEGER(4) :: i
    INTEGER(8) :: l

    LOGICAL(2), ALLOCATABLE :: logScalarAlloc
    INTEGER(8), ALLOCATABLE :: intArrayAlloc( :,:,: )


    ALLOCATE(logScalarAlloc, SOURCE=.TRUE._2)
    IF (.NOT. ALLOCATED( logScalarAlloc )) CALL zzrc( 10_4 )


    logScalarAlloc = .FALSE._4

    IF (.NOT. ALLOCATED( logScalarAlloc )) CALL zzrc( 20_4 )
    IF (KIND( logScalarAlloc ) /= 2) CALL zzrc( 30_4 )
    IF ( logScalarAlloc ) CALL zzrc( 40_4 )


    ALLOCATE(intArrayAlloc( 10,10,10 ), SOURCE=RESHAPE(&
        (/ (((INT(((j * k) + i), 8), k = 1, 10), j = 1, 10), i = 1, 10) /),&
                                                            (/ 10, 10, 10 /)))

    IF (.NOT. ALLOCATED( intArrayAlloc )) CALL zzrc( 50_4 )
    IF (SIZE( intArrayAlloc ) /= 1000) CALL zzrc( 60_4 )


    intArrayAlloc = RESHAPE(&
        (/ (((INT((((i - 1) * 100) + ((j - 1) * 10) + k), 2),&
                        k = 1, 10), j = 1, 10), i = 1, 10) /),&
                                                (/ 10, 10, 10 /))

    IF (.NOT. ALLOCATED( intArrayAlloc )) CALL zzrc( 70_4 )
    IF (SIZE( intArrayAlloc ) /= 1000) CALL zzrc( 80_4 )

    DO i = 1, 3
        IF (SIZE(intArrayAlloc, i) /= 10) CALL zzrc( (90_4 + i) )
    END DO


    DO i = 1, 10
        DO j = 1, 10
            DO k = 1, 10
                l = INT((((i - 1) * 100) + ((j - 1) * 10) + k), 8)
                IF (intArrayAlloc( k,j,i ) /= l) THEN
                    PRINT *, "intArrayAlloc(", k, ",", j,&
                                ",", i, ") = ", intArrayAlloc( k,j,i )
                    PRINT *, "Should be:", l

                    CALL zzrc( 100_4 )

                ELSE IF (KIND( intArrayAlloc( k,j,i ) ) /= 8) THEN
                    PRINT *, "KIND( intArrayAlloc(", k, ",", j,&
                            ",", i, ") =",KIND( intArrayAlloc( k,j,i ) )
                    CALL zzrc( 110_4 )
                END IF
            END DO
        END DO
    END DO

END PROGRAM allocatedAllocatable03
