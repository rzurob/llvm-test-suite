!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : allocatedScalarDerived01 - Basic Tests:
!*                               Non-CHARACTER Derived Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August  8, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Scalar of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and expr is a Scalar of Derived Type with the
!*                               same Shape and Length Type Parameter Values
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

    TYPE tType
        REAL(8) :: r

        CONTAINS

            FINAL :: tTypeFinal

    END TYPE tType

    CONTAINS

        SUBROUTINE tTypeFinal( o )
            TYPE(tType) :: o

            PRINT '("tTypeFinal(",F4.2,")")', o%r

        END SUBROUTINE tTypeFinal

END MODULE mModule

PROGRAM allocatedScalarDerived01
    USE mModule

    TYPE(tType) :: tTypeScalar = tType(0.99_8)
    TYPE(tType), ALLOCATABLE :: tTypeScalarAlloc


    ALLOCATE(tTypeScalarAlloc, SOURCE=tType(0.66_8))

    IF (.NOT. ALLOCATED( tTypeScalarAlloc ))    CALL zzrc( 10_4 )
    IF (tTypeScalarAlloc%r /= 0.66_8)           CALL zzrc( 20_4 )

    tTypeScalar%r = 0.98_8
    tTypeScalarAlloc%r = 0.67_8

    PRINT *, "Before:  Intrinsic Assignment"
    tTypeScalarAlloc = tTypeScalar
    PRINT *, " After:  Intrinsic Assignment"


    IF (.NOT. ALLOCATED( tTypeScalarAlloc ))    CALL zzrc( 30_4 )
    IF (tTypeScalarAlloc%r /= 0.98_8)           CALL zzrc( 40_4 )

    tTypeScalar%r = 0.97_8
    tTypeScalarAlloc%r = 0.68_8

END PROGRAM allocatedScalarDerived01
