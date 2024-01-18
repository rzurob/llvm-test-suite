! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/pack/structureComponent001.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type Base1(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      m
        integer(k2)      n
    end type

    type, extends(Base) :: Child    ! (4,20)
        type(Base(k1,n1)) :: b1(20)
        type(Base1(n1,k1)) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child(4,20)) :: c1
    type(Base1(20,4)) :: b1(20)
    logical :: m1(25)

    b1 = (/ (Base1(20,4)(i,i+1), i=1,20) /)
    c1%b2 = reshape((/(Base1(20,4)(i,-i),i=1,20)/), (/5,5/), &
     (/Base1(20,4)(88,-88),Base1(20,4)(99,-99)/), (/2,1/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    associate(name1=>pack(c1%b2, reshape(m1,(/5,5/)), b1(2:16)))
        if(.NOT. same_type_as(name1, Base1(20,4)(1,1))) error stop 1_4
        print *, name1
        print *, shape(name1)
    end associate
end
