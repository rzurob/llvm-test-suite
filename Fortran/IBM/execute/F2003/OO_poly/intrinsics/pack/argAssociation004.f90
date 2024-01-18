!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/22/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is a pointer or allocatable,
!                              non-poly, and is array.
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

    type, extends(Base) :: Child
        integer j
    end type
end module

program argAssociation004
use m
    type(Child), pointer :: b(:,:,:)
    type(Child), allocatable :: c(:)

    allocate(b(3,4,2), SOURCE=reshape((/(Child(i,i-1),i=1,24)/), &
     (/3,4,2/)))
    allocate(c(14), SOURCE=(/(Child(i,-i),i=1,14)/))

    call sub1(b, c)

    contains

    subroutine sub1(arg1, arg2)
        type(Child), pointer :: arg1(:,:,:)
        type(Child), allocatable :: arg2(:)

        print *, pack(arg1, MOD(arg1%i,2)==1, arg2)
    end subroutine
end
