! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/component1/functionReturn003.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnok -qnol

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : The target of a procedure pointer is
!                              specified by function return. Do not
!                              specify proc-interface.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    contains

    function func1()
        procedure(sub2), pointer :: func1
        func1 => sub2
    end function

    function func2()
        procedure(sub3), pointer :: func2
        func2 => sub3
    end function

    subroutine sub2(i)
        integer, intent(in) :: i
        print *, "sub2", i
    end subroutine

    subroutine sub3(b, c)
        type(Base(*,4)), intent(in) :: b
        type(Base(*,4)), intent(in) :: c
        print *, "sub3", b, c
    end subroutine
end module

module n
use m
    implicit type(Base(20,4)) (p)

    type Container(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        procedure(sub2), nopass, pointer :: pp1 => null()
        procedure(sub3), nopass, pointer :: pp2 => null()
    end type
end module

program functionReturn003
use n
    implicit type(Base(20,4)) (p,f), type(Container(4,20)) (c)

    if(associated(c1%pp1)) error stop 1_4

    c1%pp1 => func1()
    if(.NOT. associated(c1%pp1)) error stop 2_4
    call c1%pp1(5)

    nullify(c1%pp1)
    if(associated(c1%pp1)) error stop 3_4

    c1%pp2 => func2()
    if(.NOT. associated(c1%pp2)) error stop 4_4
    call c1%pp2(Base(20,4)(6), Base(20,4)(7))
end
