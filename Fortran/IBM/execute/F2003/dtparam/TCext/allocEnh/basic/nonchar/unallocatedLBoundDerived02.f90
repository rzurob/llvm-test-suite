! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/allocEnh/basic/nonchar/unallocatedLBoundDerived02.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : unallocatedLBoundDerived02 - Basic Tests:
!*                               Non-CHARACTER Array of Derived Type
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : August 29, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment where variable is an
!*                               Unallocated ALLOCATABLE Array of Derived Type
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

MODULE mBase

    TYPE tBase(N1,K1)    ! (20,16)
        INTEGER, KIND         :: K1
        INTEGER, LEN          :: N1
        REAL(K1), ALLOCATABLE :: b( : )
    END TYPE tBase

END MODULE mBase


MODULE mDerived
    USE mBase

    TYPE, EXTENDS(tBase) :: tDerived    ! (20,16)
        COMPLEX(K1) :: d( 6 )
    END TYPE tDerived

END MODULE mDerived


PROGRAM unallocatedLBoundDerived02
    USE mDerived

    INTERFACE
        SUBROUTINE CheckIt( dA )
            USE mDerived
            TYPE(tDerived(*,16)) :: dA( -5:5,0:10 )
        END SUBROUTINE CheckIt
    END INTERFACE

    INTEGER(4) :: i
    INTEGER(4) :: j
    INTEGER(4) :: k

    TYPE(tDerived(20,16)) :: d( -5:5,0:10 )
    TYPE(tDerived(:,16)), ALLOCATABLE :: dA( :,: )


    DO i = 0, 10
        DO j = -5, 5
            d( j,i )%d = (/ (CMPLX((i / k), (j / k), 16), k = 1, 6) /)

            ALLOCATE(d( j,i )%b( j:(j + 5) ),&
                SOURCE=(/ (REAL(((i / k) * j), 16), k = 1, 6) /))
        END DO
    END DO

    IF ( ALLOCATED( dA ) ) CALL zzrc( 10_4 )

    dA = d


    IF (.NOT. ALLOCATED( dA )) CALL zzrc( 20_4 )

    IF (SIZE( dA ) /= 121)     CALL zzrc( 30_4 )

    IF (SIZE(dA, 1) /= 11)     CALL zzrc( 31_4 )
    IF (SIZE(dA, 2) /= 11)     CALL zzrc( 32_4 )

    IF (LBOUND(dA, 1) /= -5)   CALL zzrc( 41_4 )
    IF (LBOUND(dA, 2) /= 0)    CALL zzrc( 42_4 )

    CALL CheckIt( dA )

END PROGRAM unallocatedLBoundDerived02


SUBROUTINE CheckIt( dA )
    USE mDerived

    TYPE(tDerived(*,16)) :: dA( -5:5,0:10 )

    INTEGER(4) :: i
    INTEGER(4) :: j
    INTEGER(4) :: k


    DO i = 0, 10
        DO j = -5, 5
            DO k = 1, 6
                IF (dA( j,i )%d( k ) /= CMPLX((i / k), (j / k), 16)) THEN
                    PRINT *, "Expecting:", CMPLX((i / k), (j / k), 16)
                    PRINT *, "dA(", j, ",", i, ")%d(", k, ") =",&
                                                    dA( j,i )%d( k )

                    CALL zzrc( (100_4 + (i * 11_4) + j) )

                ELSE IF (dA( j,i )%b( (j + k - 1) ) /=&
                            REAL(((i / k) * j), 16)) THEN
                    PRINT *, "Expecting:", REAL(((i / k) * j), 16)
                    PRINT *, "dA(", j, ",", i, ")%b(", (j + k - 1),&
                                         ") =", dA( j,i )%b( (j + k - 1) )

                    CALL zzrc( (100_4 + (i * 11_4) + j) )
                END IF
            END DO
        END DO
    END DO

END SUBROUTINE CheckIt
