!*  ===================================================================
!*
!*                               Intrinsic Type
!*
!*  DATE                       : September  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 2
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

PROGRAM allocCharLBound02

    INTEGER(4) :: i

    CHARACTER(5) :: charValue

    CHARACTER(:), ALLOCATABLE :: chrArrAlloc1( :,: )
    CHARACTER(3), ALLOCATABLE :: chrArrAlloc2( :,: )

    ALLOCATE(CHARACTER(9) :: chrArrAlloc1( -1:1,0:2 ))
    IF (.NOT. ALLOCATED( chrArrAlloc1 )) CALL zzrc( 10_4 )

    chrArrAlloc1 = RESHAPE((/ ('xyzXYZxyz', i = 1, 9) /), (/ 3,3 /))

    ALLOCATE(chrArrAlloc2( 0:1,-1:0 ),&
            SOURCE=RESHAPE((/ ('!@#', i = 1, 4) /), (/ 2,2 /)))

    IF (.NOT. ALLOCATED( chrArrAlloc2 )) CALL zzrc( 11_4 )

    CALL IntrinsicAssignment( )

    IF (.NOT. ALLOCATED( chrArrAlloc1 )) CALL zzrc( 20_4 )
    IF (.NOT. ALLOCATED( chrArrAlloc2 )) CALL zzrc( 21_4 )

    IF (SIZE( chrArrAlloc1 ) /= 16) CALL zzrc( 30_4 )
    IF (SIZE( chrArrAlloc2 ) /= 16) CALL zzrc( 31_4 )


    PRINT *, SIZE(chrArrAlloc1, 1), SIZE(chrArrAlloc2, 1),&
             LBOUND(chrArrAlloc1, 1), LBOUND(chrArrAlloc2, 1)

    IF (SIZE(chrArrAlloc1, 1) /= 4)     CALL zzrc( 30_4 )
    IF (SIZE(chrArrAlloc2, 1) /= 4)     CALL zzrc( 31_4 )
    IF (LBOUND(chrArrAlloc1, 1) /= -1)  CALL zzrc( 32_4 )
    IF (LBOUND(chrArrAlloc2, 1) /= -1)  CALL zzrc( 33_4 )

    PRINT *, SIZE(chrArrAlloc1, 2), SIZE(chrArrAlloc2, 2),&
             LBOUND(chrArrAlloc1, 2), LBOUND(chrArrAlloc2, 2)

    IF (SIZE(chrArrAlloc1, 2) /= 4)     CALL zzrc( 40_4 )
    IF (SIZE(chrArrAlloc2, 2) /= 4)     CALL zzrc( 41_4 )
    IF (LBOUND(chrArrAlloc1, 2) /= 0)   CALL zzrc( 42_4 )
    IF (LBOUND(chrArrAlloc2, 2) /= 0)   CALL zzrc( 43_4 )


    PRINT *

    DO i = 0, 3
        DO j = -1, 2
            WRITE(charValue, '(I5)') ((i * j) + 15000)

            PRINT *, i, j, LEN( chrArrAlloc1( j,i ) ),&
                     LEN( chrArrAlloc2( j,i ) ), charValue, ' ',&
                     chrArrAlloc1( j,i ), ' ', chrArrAlloc2( j,i )

            IF (LEN( chrArrAlloc1( j,i ) ) /= 5) CALL zzrc( 51_4 )
            IF (LEN( chrArrAlloc2( j,i ) ) /= 3) CALL zzrc( 52_4 )

            IF (chrArrAlloc1( j,i ) /= charValue)        CALL zzrc( 53_4 )
            IF (chrArrAlloc2( j,i ) /= charValue( 1:3 )) CALL zzrc( 54_4 )
        END DO
    END DO


    CONTAINS

        SUBROUTINE IntrinsicAssignment( )
            CHARACTER(5) :: chrArray( -1:2,0:3 )

            DO i = 0, 3
                DO j = -1, 2
                    WRITE(chrArray( j,i ), '(I5)') ((i * j) + 15000)
                END DO
            END DO

            chrArrAlloc1 = chrArray
            chrArrAlloc2 = chrArray

        END SUBROUTINE IntrinsicAssignment

END PROGRAM allocCharLBound02
