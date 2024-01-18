! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/pack/structureComponent002.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a structure component, which
!                              is poly array. The object containing
!                              the component is a scalar.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type

    type Container(k2)    ! (4)
        integer, kind                      :: k2
        class(AbstractParent(k2)), pointer :: b1(:,:)
    end type
end module

program structureComponent002
use m
    type(Container(4)) :: c1
    logical :: m1(25)

    allocate(c1%b1(5,4), SOURCE=reshape((/(Base(4)(i),i=1,20)/),(/5,4/)))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>pack(c1%b1, reshape(m1, (/5,4/))))
        type is (Base(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(c1%b1)
    allocate(c1%b1(4,3), SOURCE=reshape((/(Child(4)(i,-i),i=1,12)/), &
     (/4,3/)))

    select type(name1=>pack(c1%b1, reshape(m1, (/4,3/)), &
     reshape(c1%b1, (/8/))))
        type is (Child(4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
