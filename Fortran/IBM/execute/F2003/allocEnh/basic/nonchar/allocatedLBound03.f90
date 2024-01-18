!*  ===================================================================
!*
!*                               Non-CHARACTER Intrinsic Types
!*
!*  DATE                       : July 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is
!*                               an Allocated ALLOCATABLE of Intrinsic
!*                               (Non-CHARACTER) Type and
!*  SECONDARY FUNCTIONS TESTED : LBOUND(expr) returns a value other than "1"
!*                               as the Lower Bound for an N-Dimension Array
!*                               (1<= N <= 7) -- N == 5
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

MODULE module
    COMPLEX(4), ALLOCATABLE :: complexAlloc( :,:,:,:,: )

    CONTAINS

        SUBROUTINE intrinsicAssignment( bounds )
            INTEGER, INTENT(IN) :: bounds( 10 )

            COMPLEX(8) :: newComplex(   bounds( 1 ):bounds( 2 ),&
                                        bounds( 3 ):bounds( 4 ),&
                                        bounds( 5 ):bounds( 6 ),&
                                        bounds( 7 ):bounds( 8 ),&
                                        bounds( 9 ):bounds( 10 )    )

            newComplex =&
                RESHAPE((/ (CMPLX(REAL( -i ), REAL( i ), 4),&
                                i = 4, 1, -1) /), (/ 2, 1, 1, 1, 2 /))

            complexAlloc = newComplex

        END SUBROUTINE intrinsicAssignment

        SUBROUTINE checkLBounds(lBounds, rcBase)
            INTEGER, INTENT(IN) :: lBounds( 0:9 )
            INTEGER(4), INTENT(IN) :: rcBase

            INTEGER(4) :: i

            DO i = 1, 5
                PRINT '(I2,") Expected = (",I3,"), Actual = (",I3,")")',&
                        i, lbounds( ((i - 1) * 2) ), LBOUND(complexAlloc, i)

                IF (LBOUND(complexAlloc, i) /=&
                    lBounds( ((i - 1) * 2) )) CALL zzrc( (rcBase + i) )
            END DO

        END SUBROUTINE checkLBounds

END MODULE module


PROGRAM allocatedLBound03
    USE module

    INTEGER, PARAMETER :: start( 10 ) = (/ -1,0, 0,0, 3,3,   0,1, -1,-1 /)
    INTEGER, PARAMETER :: end( 10 )   = (/ 0,1,  0,0, -1,-1, 4,4, -1,0  /)


    ALLOCATE( COMPLEX(4) :: complexAlloc(   start( 1 ):start( 2 ),&
                                            start( 3 ):start( 4 ),&
                                            start( 5 ):start( 6 ),&
                                            start( 7 ):start( 8 ),&
                                            start( 9 ):start( 10 )  ) )

    IF (.NOT. ALLOCATED( complexAlloc )) CALL zzrc( 10_4 )

    CALL checkLBounds(start, 20_4)

    complexAlloc =&
        RESHAPE((/ (CMPLX(REAL( i ), REAL( -i ), 4), i = 1, 4) /),&
                                                    (/ 2, 1, 1, 2, 1 /))

    PRINT *
    PRINT 100, "Before", complexAlloc
100 FORMAT(A6,': ',('(',F5.2,',',F5.2,')'))


    CALL intrinsicAssignment( end )

    IF (.NOT. ALLOCATED( complexAlloc )) CALL zzrc( 30_4 )

    PRINT *
    PRINT 100, "After", complexAlloc
    PRINT *

    CALL checkLBounds(end, 40_4)

END PROGRAM allocatedLBound03
