!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              procedure pointer is a dummy argument.
!                              The actual argument is a procedure
!                              pointer component. The associated
!                              procedure is either a function or a
!                              subroutine. Non-poly. Intrinsic or
!                              derived type, scalar.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type Container
        procedure(), nopass, pointer :: spp
        procedure(), nopass, pointer :: ipp
        procedure(bfunc3), nopass, pointer :: bpp
    end type

    contains

    subroutine sub1(b)
        type(Base), intent(in) :: b
        print *, b
    end subroutine

    subroutine sub2(i)
        integer, intent(in) :: i
        print *, i
    end subroutine

    type(Base) function bfunc3(b, sp, ip)
        type(Base), intent(in) :: b
        procedure(), pointer, intent(in) :: sp
        procedure(), pointer, intent(in) :: ip

        if(.NOT. associated(sp)) error stop 1_4
        call sp(b)

        if(.NOT. associated(ip)) error stop 2_4
        call ip(b%i)

        bfunc3 = Base(b%i*2)
    end function
end module

program procInterface002c
use m
    implicit type(Base) (b)

    type(Container) :: c1
    type(Base) :: rv

    c1%spp => sub1
    c1%ipp => sub2
    c1%bpp => bfunc3

    rv = c1%bpp(Base(3), c1%spp, c1%ipp)
    print *, rv
end
