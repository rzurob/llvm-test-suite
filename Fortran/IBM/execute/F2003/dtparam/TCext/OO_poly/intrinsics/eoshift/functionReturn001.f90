! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/eoshift/functionReturn001.f
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program functionReturn001
use m
    select type(name1=>eoshift(func1(), (/1,2,-1/), func2(), 2))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    contains

    function func1()
        class(Base(4)), pointer :: func1(:,:)
        allocate(func1(3,4), SOURCE=reshape((/(Child(4)(i,-i),i=1,12)/), &
         (/3,4/)))
    end function

    function func2()
        class(Base(4)), pointer :: func2(:)
        allocate(func2(3), SOURCE=(/(Child(4)(i,i-1),i=14,16)/))
    end function
end
