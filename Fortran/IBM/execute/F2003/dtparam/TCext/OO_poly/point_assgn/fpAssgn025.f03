! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn025.f
! opt variations: -qnock -qnok -ql -qdefaultpv

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer object can be
!*                               structure component)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = ''
    end type

    type (child(4,1,15)), target, save :: c1_m
end module

module m1
use m
    type baseContainer(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()
    end type

    type anyContainer(k4)    ! (4)
        integer, kind :: k4
        class (*), pointer :: data => null()
    end type

end module

program fpAssgn025
use m1
    interface assignment (=)
        subroutine base2Unlimited (ac, bc)
        use m1
            type (anyContainer(4)), intent(out) :: ac
            type (baseContainer(4)), intent(in) :: bc
        end subroutine
    end interface
    type (anyContainer(4)) :: co_a1
    type (baseContainer(4)) :: co_b1

    type (child(4,1,15)), target :: c1

    co_b1 = baseContainer(4) (data = c1_m)

    co_a1 = co_b1

    if (.not. associated (co_a1%data, c1_m)) error stop 1_4

    co_b1%data => c1
    co_a1%data => co_b1%data

    if (.not. associated (co_a1%data, c1)) error stop 2_4
end

subroutine base2Unlimited (ac, bc)
use m1
    type (anyContainer(4)), intent(out) :: ac
    type (baseContainer(4)), intent(in) :: bc

    ac%data => bc%data
end subroutine
