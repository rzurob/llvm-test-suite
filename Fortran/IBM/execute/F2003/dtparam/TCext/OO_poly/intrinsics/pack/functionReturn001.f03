! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/pack/functionReturn001.f
! opt variations: -ql -qreuse=none

!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    contains

    function func2()
        class(*), pointer :: func2(:)
        allocate(func2(8), SOURCE=(/(Child(4)(i,i-1),i=11,18)/))
    end function
end module

program functionReturn001
use m
    logical :: m1(20)
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
     .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE.,.TRUE.,.TRUE.,.FALSE./)

    select type(name1=>pack(func1(), reshape(m1,(/3,4/)), func2()))
        type is (Child(4))
            print *, name1
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
end
