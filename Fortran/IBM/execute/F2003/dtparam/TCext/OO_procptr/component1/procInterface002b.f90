! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/component1/procInterface002b.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qnol

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/component1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer component
! DRIVER STANZA              : xlf90
! DESCRIPTION                : Do not specify proc-interface. The
!                              associated procedure is either a
!                              module function or a module subroutine.
!                              Non-poly. Intrinsic or derived type,
!                              scalar or array.
!
!                              Use implicit typing to match the
!                              procedure pointeer with the target.
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
        procedure(), nopass, pointer :: spp1
        procedure(sub2), nopass, pointer :: spp2
    end type

    contains

    subroutine sub1(i)
        integer, intent(in) :: i(2,3)
        print *, "sub1", i
    end subroutine

    subroutine sub2(b, c)
        type(Base(*,4)), intent(in) :: b
        type(Base(*,4)), intent(in) :: c(5)
        print *, "sub2", b%i, c%i
    end subroutine
end module

program procInterface002b
use m
    implicit type(Base(20,4)) (b)

    b1 = Base(20,4)(5,null(),null())

    b1%spp1 => sub1
    call b1%spp1((/(i,i=1,6)/))

    b1%spp2 => sub2
    call b1%spp2(Base(20,4)(3,null(),null()),(/(Base(20,4)(i,null(),null()),i=1,5)/))
end
