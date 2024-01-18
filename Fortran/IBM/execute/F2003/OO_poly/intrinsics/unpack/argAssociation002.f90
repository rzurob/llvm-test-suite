!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/unpack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/25/2005
! PRIMARY FUNCTIONS TESTED   : unpack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : VECTOR or FIELD is a dummy argument. Dummy
!                              argument is non-pointer, non-allocatable,
!                              poly, and is array.
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

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base) :: arg1(10)
        class(Base) :: arg2(:,:)
        class(Base) :: arg3(:)
        class(Base) :: arg4(2,2)

        associate(name1=>unpack((/(Base(-i),i=101,106)/), &
         MOD(arg1%i,2)==0, arg1))
            if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
            print *, name1
            print *, shape(name1)
        end associate

        select type(name1=>unpack(arg3, MOD(arg2%i,2)==1, arg2))
            type is (Child)
                print *, name1
                print *, shape(name1)
            class default
                error stop 2_4
        end select

        select type(name1=>unpack(arg3, reshape((/.TRUE.,.FALSE., &
         .FALSE.,.TRUE./),(/2,2/)), arg4))
            type is (Child)
                print *, name1
                print *, shape(name1)
            class default
                error stop 3_4
        end select
    end subroutine
end module

program argAssociation002
use m
    type(Base) :: b1(10)
    type(Child) :: c1(2,3)
    class(Base), pointer :: b2(:)
    class(Child), allocatable :: c2(:,:)

    b1 = (/ (Base(i),i=1,10) /)
    c1 = reshape((/(Child(i,i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))

    call sub1(b1, c1, b2, c2)
end
