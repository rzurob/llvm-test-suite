!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/eoshift
! DATE                       : 02/08/2005
! PRIMARY FUNCTIONS TESTED   : eoshift
! DESCRIPTION                : ARRAY or BOUNDARY is the return value of
!                              an internal function call.
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

program functionReturn001
use m
    select type(name1=>eoshift(func1(), (/1,2,-1/), func2(), 2))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    contains

    function func1()
        class(Base), pointer :: func1(:,:)
        allocate(func1(3,4), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
         (/3,4/)))
    end function

    function func2()
        class(Base), pointer :: func2(:)
        allocate(func2(3), SOURCE=(/(Child(i,i-1),i=14,16)/))
    end function
end