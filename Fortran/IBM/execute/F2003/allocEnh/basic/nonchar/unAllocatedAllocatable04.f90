!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unAllocatedAllocatable04 - Basic Tests:
!*                               Non-CHARACTER Intrinsic Types
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August  3, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Intrinsic
!*                               (Non-CHARACTER) Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Array of Intrinsic Type with
!*                               different Length Type Parameter Values (and
!*                               a different Shape)
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

PROGRAM unAllocatedAllocatable04

    INTEGER(4) :: i

    COMPLEX(4) :: complexArray( 100,100 )

!    COMPLEX(4) :: complexArray( 100,100 ) = RESHAPE(&
!        (/ (CMPLX((i * 3.14), (-i / 3.14), 4), i = -4999, 5000) /),&
!                                                        (/ 100, 100 /))

    REAL(8) :: value
    REAL(8), ALLOCATABLE :: realArrayAlloc( :,: )


    complexArray = RESHAPE(&
        (/ (CMPLX((i * 3.14), (-i / 3.14), 4), i = -4999, 5000) /),&
                                                        (/ 100, 100 /))

    IF ( ALLOCATED( realArrayAlloc ) ) CALL zzrc( 10_4 )


    realArrayAlloc = complexArray


    IF (.NOT. ALLOCATED( realArrayAlloc )) CALL zzrc( 20_4 )


    PRINT *, SIZE( realArrayAlloc )
    IF (SIZE( realArrayAlloc ) /= 10000) CALL zzrc( 30_4 )

    PRINT *, SIZE(realArrayAlloc, 1)
    IF (SIZE(realArrayAlloc, 1) /= 100) CALL zzrc( 40_4 )

    PRINT *, SIZE(realArrayAlloc, 2)
    IF (SIZE(realArrayAlloc, 2) /= 100) CALL zzrc( 50_4 )


    PRINT *, KIND( realArrayAlloc( j,i ) )
    IF (KIND( realArrayAlloc( j,i ) ) /= 8) CALL zzrc( 60_4 )


    DO i = 1, 100
        DO j = 1, 100
            value = REAL(((((i - 1) * 100) + j - 5000) * 3.14), 8)

            IF (realArrayAlloc( j,i ) /= value) THEN
                PRINT *, "value =", value
                PRINT *, "realArrayAlloc(", j, ",", i, ") =",&
                                            realArrayAlloc( j,i )

                CALL zzrc( 70_4 )
            END IF
        END DO
    END DO

END PROGRAM unAllocatedAllocatable04
