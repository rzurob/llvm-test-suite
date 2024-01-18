!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! PROGRAMMER                 : Yong Du
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
!                              unlimited poly, and is array.
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

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1(10)
        class(*) :: arg2(:,:)
        class(*) :: arg3(:)
        class(*) :: arg4(2,2)
        class(*) :: arg5(:,:,:)

        select type(name1=>eoshift(arg5, reshape((/1,-2,1,-1/), &
         (/2,2/)), reshape(arg1,(/2,2/))))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name1=>eoshift(arg2, (/1,-1/), arg3(4:5), 2))
            type is (Child)
                print *, name1
            class default
                error stop 2_4
        end select

        select type(name1=>eoshift(arg4,(/-1,1/),(/arg2(1,2),arg3(5)/)))
            type is (Child)
                print *, name1
            class default
                error stop 3_4
        end select
    end subroutine
end module

program argAssociation003
use m
    type(Base) :: b1(10)
    type(Child) :: c1(2,3)
    class(Base), pointer :: b2(:)
    class(Child), allocatable :: c2(:,:)
    class(*), allocatable :: u1(:,:,:)

    b1 = (/ (Base(i),i=1,10) /)
    c1 = reshape((/(Child(i,i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))
    allocate(u1(3,2,2), SOURCE=reshape((/(Base(i),i=4,15)/),(/3,2,2/)))

    call sub1(b1, c1, b2, c2, u1)
end
