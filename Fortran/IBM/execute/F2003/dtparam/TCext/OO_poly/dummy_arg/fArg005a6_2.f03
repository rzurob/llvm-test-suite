! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a6_2.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly-pointer
!                               dummy-arg to be associated only with unlimited
!                               poly-pointer actual-arg; use of sequence type
!                               and the BIND(C) types to verify the data
!                               creation)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine createObj (x, x1)
        class (*), pointer, intent(out) :: x
        class (*), intent(in) :: x1

        allocate (x, source=x1)
    end subroutine
end module

program fArg005a6_2
use m
use ISO_C_BINDING

    class (*), pointer :: x => null()
    class (*), pointer :: x1 => null()

    type seq1(n1,k1,k2,k3)    ! (20,4,8,2)
        integer, kind :: k1,k2,k3
        integer, len  :: n1
        sequence

        integer(k1)      i1
        integer(k2)      i2
        integer(k3)      i3
    end type

    type, bind(c) :: bType
        integer(C_INT) i1
        integer(C_SHORT) i2
    end type

    type (seq1(:,4,8,2)), pointer :: s1

    type (bType), pointer :: b1

    call createObj (x, seq1(20,4,8,2)(1,2,3))

    s1 => x

    if ((s1%i1 /= 1) .or. (s1%i2 /= 2) .or. (s1%i3 /= 3)) error stop 1_4

    call createObj (x1, bType (100, -1))

    b1 => x1

    if ((b1%i1 /= 100) .or. (b1%i2 /= -1)) error stop 2_4
end