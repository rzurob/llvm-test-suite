! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/component1/procInterface002c.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnok -ql

!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type Container(k2)    ! (4)
        integer, kind :: k2
        procedure(sub1), nopass, pointer :: spp
        procedure(), nopass, pointer :: ipp
        procedure(bfunc3), nopass, pointer :: bpp
    end type

    contains

    subroutine sub1(b)
        type(Base(4)), intent(in) :: b
        print *, b
    end subroutine

    subroutine sub2(i)
        integer, intent(in) :: i
        print *, i
    end subroutine

    type(Base(4)) function bfunc3(b, sp, ip)
        type(Base(4)), intent(in) :: b
        procedure(sub1), pointer, intent(in) :: sp
        procedure(), pointer, intent(in) :: ip

        if(.NOT. associated(sp)) error stop 1_4
        call sp(b)

        if(.NOT. associated(ip)) error stop 2_4
        call ip(b%i)

        bfunc3 = Base(4)(b%i*2)
    end function
end module

program procInterface002c
use m
    implicit type(Base(4)) (b)

    type(Container(4)) :: c1
    type(Base(4)) :: rv

    c1%spp => sub1
    c1%ipp => sub2
    c1%bpp => bfunc3

    rv = c1%bpp(Base(4)(3), c1%spp, c1%ipp)
    print *, rv
end
