! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/basic/nonchar/allocatedLBoundDerived01.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*                               Non-CHARACTER Derived Type
!*
!*  DATE                       : August  9, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Allocated ALLOCATABLE Array of Derived Type
!*  SECONDARY FUNCTIONS TESTED : and LBOUND(expr) returns a value other than
!*                               "1" as the Lower Bound for a single Dimension
!*                               Array
!*
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

    TYPE :: tBase(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE tBase


    TYPE, EXTENDS(tBase) :: tDerived    ! (4,20)

        INTEGER(K1), POINTER :: idxPtr

        CONTAINS
            FINAL :: TDerivedFinal

    END TYPE tDerived

    CONTAINS

        ELEMENTAL SUBROUTINE TDerivedFinal( o )
            TYPE(tDerived(4,*)), INTENT(INOUT) :: o

            o%idxPtr = -99

        END SUBROUTINE TDerivedFinal

END MODULE mModule


PROGRAM allocatedLBoundDerived01
    USE mModule

    INTEGER(4) :: i

    INTEGER, TARGET :: idx( -5:5 ) = (/ (i, i = -5, 5) /)

    TYPE(tDerived(4,20)) :: derived( -1:1 )
    TYPE(tDerived(4,:)), ALLOCATABLE :: derivedAlloc( : )


    ALLOCATE( tDerived(4,20) :: derivedAlloc( 0:1 ) )

    IF (.NOT. ALLOCATED( derivedAlloc ))    ERROR STOP 10_4

    IF (SIZE( derivedAlloc ) /= 2)          ERROR STOP 20_4
    IF (LBOUND(derivedAlloc, 1) /= 0)       ERROR STOP 30_4


    derived( -1 )%idxPtr => idx( -5 )

    DO i = 0, 1
        derived( i )%idxPtr => idx( (i - 4) )
        derivedAlloc( i )%idxPtr => idx( i )
    END DO


    derivedAlloc = derived

    IF (.NOT. ALLOCATED( derivedAlloc ))    ERROR STOP 40_4

    IF (SIZE( derivedAlloc ) /= 3)          ERROR STOP 50_4
    IF (LBOUND(derivedAlloc, 1) /= -1)      ERROR STOP 60_4


    PRINT '("idx(",10(I3,","),I3,")")', idx


    DO i = -5, 5
        IF (i == 0  .OR.  i == 1) THEN
            IF (idx( i ) /= -99) THEN
                CALL zzrc( (70_4 + i) )
            END IF

        ELSE IF (idx( i ) /= i) THEN
            CALL zzrc( (70_4 + i) )
        END IF
    END DO

END PROGRAM allocatedLBoundDerived01
