!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY or VECTOR is the return value of a
!                              function call.
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
        class(*), pointer :: func2(:)
        allocate(func2(8), SOURCE=(/(Child(i,i-1),i=11,18)/))
    end function
end module

program functionReturn001
use m
    logical :: m1(20)
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE./)

    select type(name1=>pack(func1(), reshape(m1,(/3,4/)), func2()))
        type is (Child)
            print *, name1
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
end
