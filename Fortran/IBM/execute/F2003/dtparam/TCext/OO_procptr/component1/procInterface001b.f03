! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/procInterface001b.f
! opt variations: -qnok -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DESCRIPTION                : Specify proc-interface using declaration
!                              type specification. The associated
!                              function is a module function. Non-poly.
!                              Intrinsic or derived type, scalar or
!                              array.
!
!                              The dummy arguments of the associated
!                              function are arrays. This test case
!                              involves sequence association where the
!                              size and shape of the actual
!                              argument do not match those of the
!                              dummy argument.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(integer), nopass, pointer :: pp1
        procedure(type(Base(4))), nopass, pointer :: pp2
    end type

    contains

    integer function func1(b)
        integer, intent(in) :: b(3:*)
        print *, "func1", b(:8)
        func1 = size(b(:8))
    end function

    type(Base(4)) function func2(b)
        type(Base(4)), intent(in) :: b(5,3)
        print *, "func2", b
        func2 = Base(4)(size(b))
    end function
end module

program procInterface001b
use m
    type(Container(4)) :: c1
    integer rv1
    type(Base(4)) rv2
    integer :: i1(3,2)
    i1 = reshape((/(i,i=1,6)/),(/3,2/),(/-1/),(/2,1/))

    c1%pp1 => func1
    c1%pp2 => func2

    rv1 = c1%pp1(i1)
    print *, "Func1", rv1

    rv2 = c1%pp2(reshape((/(Base(4)(i),i=11,25)/),(/4,3,2/), &
     (/Base(4)(-1),Base(4)(-2)/), (/2,3,1/)))
    print *, "Func2", rv2
end
