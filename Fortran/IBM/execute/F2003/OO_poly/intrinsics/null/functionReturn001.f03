!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/null
! DATE                       : 03/02/2005
! PRIMARY FUNCTIONS TESTED   : null
! DESCRIPTION                : MOLD is the return value of a function
!                              call.
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

    function func2()
        class(*), allocatable :: func2(:)
        allocate(func2(8), SOURCE=(/(Child(i,i-1),i=11,18)/))
    end function
end module

program functionReturn001
use m
    if(associated(null(func1()))) error stop 1_4
    if(allocated(null(func2()))) error stop 2_4

    contains

    function func1()
        class(Base), pointer :: func1(:,:)
        allocate(func1(3,4), SOURCE=reshape((/(Child(i,-i),i=1,12)/), &
         (/3,4/)))
    end function
end
