! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/declaration1/procInterface002c.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -ql

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              procedure pointer is a dummy argument.
!                              The associated procedure is either a
!                              function or a subroutine. Non-poly.
!                              Intrinsic or derived type, scalar.
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

    contains

    subroutine sub1(b)
        type(Base(4)), intent(in) :: b
        print *, "sub1", b
    end subroutine

    subroutine isub2(i)
        integer, intent(in) :: i
        print *, "isub2", i*2
    end subroutine

    subroutine bsub3(b, sp, ip)
        type(Base(4)), intent(in) :: b
        procedure(sub1), pointer, intent(in) :: sp
        procedure(), pointer, intent(in) :: ip

        if(.NOT. associated(sp)) error stop 1_4
        call sp(b)

        if(.NOT. associated(ip)) error stop 2_4
        call ip(b%i)
    end subroutine
end module

program procInterface002c
use m
    implicit type(Base(4)) (b)

    procedure(sub1), pointer :: spp
    procedure(), pointer :: ipp
    procedure(bsub3), pointer :: bpp

    type(Base(4)) rv

    spp => sub1
    ipp => isub2
    bpp => bsub3

    call bpp(Base(4)(3), spp, ipp)
end
