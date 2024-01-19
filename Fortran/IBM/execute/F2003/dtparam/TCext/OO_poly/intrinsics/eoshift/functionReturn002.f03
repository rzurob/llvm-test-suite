! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/eoshift/functionReturn002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY is the return value of a type
!                              bound procedure call.
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

        contains
        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j

        contains
        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBase(:,:)
        allocate(createBase(3,4),SOURCE=reshape((/(Base(20,4)(i),i=1,12)/), &
         (/3,4/)))
    end function

    function createChild(a)
        class(Child(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChild(:,:)
        allocate(createChild(4,5),SOURCE=reshape((/(Child(20,4)(i,-i), &
         i=1,20)/),(/4,5/)))
    end function
end module

program functionReturn002
use m
    class(Base(:,4)), allocatable :: a

    allocate(Base(20,4)::a)

    select type(name1=>eoshift(a%create(), -1, &
     (/Base(20,4)(-3),Base(20,4)(-4),Base(20,4)(-5)/), 2))
        type is (Base(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(a)
    allocate(Child(20,4)::a)

    select type(name1=>eoshift(a%create(),(/1,-2,2,-1,1/), &
     (/(Child(20,4)(i,i+1),i=-5,-1)/)))
        type is (Child(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
