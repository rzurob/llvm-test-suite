! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/OO_poly/point_assgn/fpAssgn025a.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer component;
!*                               mix of a few features; use container arrays)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 0
    end type

    type, extends(base) :: child(n2)    ! (20,4,15)
        integer, len  :: n2
        character(n2) :: name = ''
    end type

    type (child(20,4,15)), target, save :: c1_m(10)
end module

module m1
use m
    type baseContainer(k2,n3)    ! (4,20)
        integer, kind               :: k2
        integer, len                :: n3
        class(base(n3,k2)), pointer :: data => null()
    end type

    type anyContainer(k3,n4)    ! (4,20)
        integer, kind :: k3
        integer, len  :: n4
        class (*), pointer :: data => null()
    end type

end module

program fpAssgn025a
use m1
    interface assignment (=)
        pure subroutine base2Unlimited (ac, bc)
        use m1
            class (anyContainer(4,*)), intent(out) :: ac
            class (baseContainer(4,*)), intent(in) :: bc
        end subroutine
    end interface

    type (anyContainer(4,20)) :: co_a1(10)
    type (baseContainer(4,20)) :: co_b1(10)

    character*1, target :: ch1(10)

    class (child(20,4,15)), target, allocatable :: c1(:)

    allocate (c1(10))

    forall (i=1:10)
        co_a1(i)%data => ch1(i)
        co_b1(i)%data => c1_m(11-i)
    end forall

    do i = 1, 10
        if ((.not. associated (co_a1(i)%data, ch1(i))) .or. &
            (.not. associated (co_b1(i)%data, c1_m(11-i)))) error stop 1_4
    end do


    forall (i=1:10)
        co_a1(i) = co_b1(i)
    end forall

    do i = 1, 10
        if (.not. associated (co_a1(i)%data)) error stop 2_4
    end do
end

pure subroutine base2Unlimited (ac, bc)
use m1
    class (anyContainer(4,*)), intent(out) :: ac
    class (baseContainer(4,*)), intent(in) :: bc

    allocate (ac%data, source=bc%data)
end subroutine
