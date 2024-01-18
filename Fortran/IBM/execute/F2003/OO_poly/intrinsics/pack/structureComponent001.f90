!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a structure component, which
!                              is non-poly array. The object containing
!                              the component is a scalar.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type Base1
        integer m
        integer n
    end type

    type, extends(Base) :: Child
        type(Base) :: b1(20)
        type(Base1) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1
    type(Base1) :: b1(20)
    logical :: m1(25)

    b1 = (/ (Base1(i,i+1), i=1,20) /)
    c1%b2 = reshape((/(Base1(i,-i),i=1,20)/), (/5,5/), &
     (/Base1(88,-88),Base1(99,-99)/), (/2,1/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    associate(name1=>pack(c1%b2, reshape(m1,(/5,5/)), b1(2:16)))
        if(.NOT. same_type_as(name1, Base1(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
