! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/declaration1/procInterface001b.f
! opt variations: -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    contains

    integer function func1(b)
        integer, intent(in) :: b(3:*)
        print *, "func1", b(:8)
        func1 = size(b(:8))
    end function

    type(Base(20,4)) function func2(b)
        type(Base(*,4)), intent(in) :: b(5,3)
        print *, "func2", b
        func2 = Base(20,4)(size(b))
    end function
end module

program procInterface001b
use m
    procedure(integer), pointer :: pp1
    procedure(func2), pointer :: pp2

    integer rv1
    type(Base(20,4)) rv2
    integer :: i1(3,2)
    i1 = reshape((/(i,i=1,6)/),(/3,2/),(/-1/),(/2,1/))

    pp1 => func1
    pp2 => func2

    rv1 = pp1(i1)
    print *, "Func1", rv1

    rv2 = pp2(reshape((/(Base(20,4)(i),i=11,25)/),(/4,3,2/), &
     (/Base(20,4)(-1),Base(20,4)(-2)/), (/2,3,1/)))
    print *, "Func2", rv2
end
