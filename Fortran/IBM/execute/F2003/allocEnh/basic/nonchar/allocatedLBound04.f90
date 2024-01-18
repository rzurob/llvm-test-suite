!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : August  1, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE of Intrinsic
!*                               (Non-CHARACTER) Type and
!*  SECONDARY FUNCTIONS TESTED : LBOUND(expr) returns a value other than "1"
!*                               as the Lower Bound for an N-Dimension Array
!*                               (1<= N <= 7) -- N == 7
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

PROGRAM allocatedLBound04

    INTEGER, PARAMETER :: indices( 14 ) =&
        (/ 5,9, 10,14, 15,19, 20,24, 25,29, 30,34, 35,39 /)

    REAL, ALLOCATABLE :: realAlloc( :,:,:,:,:,:,: )

    REAL :: newReal( 3:5, 6:8, 9:11, 12:14, 15:17, 18:20, 21:23 ) =&
        RESHAPE((/ (REAL( i ), i = 3**7, 1, -1) /), (/ 3,3,3,3,3,3,3 /))


    ALLOCATE(realAlloc( indices(  1 ):indices(  2 ),&
                        indices(  3 ):indices(  4 ),&
                        indices(  5 ):indices(  6 ),&
                        indices(  7 ):indices(  8 ),&
                        indices(  9 ):indices( 10 ),&
                        indices( 11 ):indices( 12 ),&
                        indices( 13 ):indices( 14 ) ),&
                SOURCE=RESHAPE((/ (REAL( i ), i = 1, 5**7) /),&
                                                (/ 5,5,5,5,5,5,5 /)))

    CALL CheckIt(5, 10_4)


    realAlloc = newReal

    CALL CheckIt(3, 50_4)

    DO i = 3, 5
        DO j = 6, 8
            DO k = 9, 11
                DO l = 12, 14
                    DO m = 15, 17
                        DO n = 18, 20
                            DO o = 21, 23
                                IF (realAlloc( i,j,k,l,m,n,o )&
                                            /= newReal( i,j,k,l,m,n,o )) THEN
                                    PRINT 100, i, j, k, l, m, n, o,&
                                                realAlloc( i,j,k,l,m,n,o ),&
                                                newReal( i,j,k,l,m,n,o )
                                    CALL zzrc( 100_4 )
                                END IF
                            END DO
                        END DO
                    END DO
                END DO
            END DO
        END DO
    END DO


100 FORMAT('realAlloc( ',I2,',',I2,',',I2,',',I2,',',I2,',',I2,',',I2,&
                                            ' ) = "',F6.2,'" (',F6.2,')')


    CONTAINS

        SUBROUTINE CheckIt(expectedSize, failRCBase)
            INTEGER :: expectedSize
            INTEGER(4) :: failRCBase


            IF (.NOT. ALLOCATED( realAlloc ))&
                        CALL zzrc( failRCBase )

            IF (SIZE( realAlloc ) /= (expectedSize ** 7))&
                        CALL zzrc( (failRCBase + 10_4) )


            DO i = 1, 7
                IF (SIZE(realAlloc, i) /= expectedSize) THEN
                    CALL zzrc( (failRCBase + 20_4 + i) )

                ELSE IF (LBOUND(realAlloc, i) /= (i * expectedSize)) THEN
                    CALL zzrc( (failRCBase + 30_4 + i) )
                END IF

            END DO

        END SUBROUTINE CheckIt

END PROGRAM allocatedLBound04
