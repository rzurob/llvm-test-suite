!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocCharLBound04 - Basic Tests: CHARACTER
!*                               Intrinsic Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : September 12, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Type CHARACTER
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 7
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

MODULE mModule

    CHARACTER(:), ALLOCATABLE :: chrArrAlloc1( :,:,:,:,:,:,: )
    CHARACTER(3), ALLOCATABLE :: chrArrAlloc2( :,:,:,:,:,:,: )

    CHARACTER(1) :: chrArr( -1:0,-1:-1,-1:0,-1:-1,-1:0,-1:-1,-1:0 ) =&
        RESHAPE((/ (CHAR( i ), i = 65, 80) /), (/ 2,1,2,1,2,1,2 /))


    CONTAINS

        SUBROUTINE Init( )

            ALLOCATE(CHARACTER(4) :: chrArrAlloc1( 2,2,2,2,2,2,2 ))
            IF (.NOT. ALLOCATED( chrArrAlloc1 )) CALL zzrc( 10_4 )
            chrArrAlloc1 =&
                RESHAPE((/ ('cube', i = 1, 128) /), (/ 2,2,2,2,2,2,2 /))

            ALLOCATE(chrArrAlloc2( 0:1,0:1,0:1,0:1,0:1,0:1,0:1 ),&
                SOURCE=RESHAPE((/ ('CAB', i = 1, 128) /), (/ 2,2,2,2,2,2,2 /)))
            IF (.NOT. ALLOCATED( chrArrAlloc2 )) CALL zzrc( 11_4 )

        END SUBROUTINE Init


        SUBROUTINE IntrinsicAssignment( )

            chrArrAlloc1 = chrArrAlloc2
            IF (.NOT. ALLOCATED( chrArrAlloc1 )) CALL zzrc( 20_4 )
            IF (SIZE( chrArrAlloc1 ) /= 128)     CALL zzrc( 21_4 )

            chrArrAlloc2 = chrArr
            IF (.NOT. ALLOCATED( chrArrAlloc2 )) CALL zzrc( 22_4 )
            IF (SIZE( chrArrAlloc2 ) /= 16)      CALL zzrc( 23_4 )

        END SUBROUTINE IntrinsicAssignment

END MODULE mModule


PROGRAM allocCharLBound04
    USE mModule

    INTEGER(4) :: m = 64


    CALL Init( )

    CALL IntrinsicAssignment( )


    DO i = 1, 7
        PRINT *, i, LBOUND(chrArrAlloc1, i), SIZE(chrArrAlloc1, i),&
                    LBOUND(chrArrAlloc2, i), SIZE(chrArrAlloc2, i)

        IF (LBOUND(chrArrAlloc1, i) /= 0)   CALL zzrc( 30_4 )
        IF (SIZE(chrArrAlloc1, i) /= 2)     CALL zzrc( 31_4 )

        IF (LBOUND(chrArrAlloc2, i) /= -1)  CALL zzrc( 40_4 )
        IF (MOD(i, 2) == 1) THEN
            IF (SIZE(chrArrAlloc2, i) /= 2)     CALL zzrc( 41_4 )
        ELSE
            IF (SIZE(chrArrAlloc2, i) /= 1)     CALL zzrc( 42_4 )
        END IF
    END DO


    IF (.NOT. ALL(chrArrAlloc1 == 'CAB')) THEN
        PRINT *, 'chrArrAlloc1:'
        PRINT 10, chrArrAlloc1
10      FORMAT(7('"',A3,'", '),'"',A3,'"')

        CALL zzrc( 50_4 )

    ELSE
        DO i = -1, 0
            DO j = -1, 0
                DO k = -1, 0
                    DO l = -1, 0
                        m = m + 1

                        PRINT 20, l, k, j, i,&
                                  chrArrAlloc2( l,-1,k,-1,j,-1,i ), CHAR( m )

                        IF (chrArrAlloc2( l,-1,k,-1,j,-1,i ) /= CHAR( m )) THEN
                            CALL zzrc( m )
                        END IF
                    END DO
                END DO
            END DO
        END DO
    END IF

20  FORMAT('chrArrAlloc2(',I2,',-1,',I2,',-1,',I2,',-1,',I2,&
                                        ') = "',A3,'" (',A3,')')

END PROGRAM allocCharLBound04
