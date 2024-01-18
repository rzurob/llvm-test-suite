!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedLBoundDerived02 - Basic Tests:
!*                               Non-CHARACTER Derived Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August  9, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for an N-Dimension
!*                               Array (1<= N <= 7) -- N == 2
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : TYPE, ALLOCATABLE Attribute, Intrinsic
!*                               Assignment
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

    TYPE :: tType

        COMPLEX(16) :: c
        
        CONTAINS

            FINAL :: tTypeFinal

    END TYPE tType

    CONTAINS

        SUBROUTINE tTypeFinal( o )
            TYPE(tType) :: o( :,: )

            PRINT '("tTypeFinal( (",F5.2,",",F5.2,") )")', o( 1,1 )%c

        END SUBROUTINE tTypeFinal

END MODULE mModule


PROGRAM allocatedLBoundDerived02
    USE mModule

    INTEGER(4) :: k = 0

    TYPE(tType) :: tTypeStat( -1:0,0:1 ) =&
        RESHAPE((/ (tType(CMPLX(-i,i)), i = 1, 4) /), (/ 2,2 /))

    TYPE(tType), ALLOCATABLE :: tTypeAlloc( :,: )


    ALLOCATE(tTypeAlloc( 0:2,-1:1 ),&
        SOURCE=RESHAPE((/ (tType(CMPLX(i, -i)), i = 1, 9) /), (/ 3,3 /)))

    IF (.NOT. ALLOCATED( tTypeAlloc )) CALL zzrc( 10_4 )

    IF (SIZE( tTypeAlloc ) /= 9)    CALL zzrc( 20_4 )
    IF (SIZE(tTypeAlloc, 1) /= 3)   CALL zzrc( 21_4 )
    IF (SIZE(tTypeAlloc, 2) /= 3)   CALL zzrc( 22_4 )

    IF (LBOUND(tTypeAlloc, 1) /= 0)     CALL zzrc( 30_4 )
    IF (LBOUND(tTypeAlloc, 2) /= -1)    CALL zzrc( 31_4 )


    tTypeAlloc = tTypeStat


    IF (.NOT. ALLOCATED( tTypeAlloc )) CALL zzrc( 40_4 )

    IF (SIZE( tTypeAlloc ) /= 4)    CALL zzrc( 50_4 )
    IF (SIZE(tTypeAlloc, 1) /= 2)   CALL zzrc( 51_4 )
    IF (SIZE(tTypeAlloc, 2) /= 2)   CALL zzrc( 52_4 )

    IF (LBOUND(tTypeAlloc, 1) /= -1)    CALL zzrc( 60_4 )
    IF (LBOUND(tTypeAlloc, 2) /= 0)     CALL zzrc( 61_4 )


    DO i = 0, 1
        DO j = -1, 0
            k = k + 1

            IF (tTypeAlloc( j,i )%c /= tTypeStat( j,i )%c) THEN
                PRINT 100, "tTypeStat", j, i, tTypeStat( j,i )%c
                PRINT 100, "tTypeAlloc", j, i, tTypeAlloc( j,i )%c

100             FORMAT(A10,'(',I2,',',I2,')%c = (',F5.2,',',F5.2,')')

                CALL zzrc( 70_4 + k )
            END IF
        END DO
    END DO

END PROGRAM allocatedLBoundDerived02
