! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/intrinsics/pack/structureComponent003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a structure component, which
!                              is unlimited poly array. The object
!                              containing the component is a scalar.
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

    type, extends(AbstractParent) :: Base(n2,k2)    ! (4,20,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      i
    end type

    type, extends(Base) :: Child(n3,k3)    ! (4,20,20,4,20,4)
        integer, kind :: k3
        integer, len  :: n3
        integer(k3)      j
    end type

    type Container(k4,n4)    ! (4,20)
        integer, kind :: k4
        integer, len  :: n4
        class(*), pointer :: b1(:,:)
    end type
end module

program structureComponent003
use m
    class(Container(4,:)), allocatable :: c1
    logical :: m1(25)

    allocate(Container(4,20)::c1)
    allocate(c1%b1(5,4), SOURCE=reshape((/(Base(4,20,20,4)(i),i=1,20)/),(/5,4/)))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.FALSE.,.TRUE., &
           .FALSE.,.FALSE.,.TRUE.,.TRUE.,.FALSE., &
           .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE./)

    select type(name1=>pack(c1%b1, reshape(m1, (/5,4/))))
        type is (Base(4,*,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    deallocate(c1%b1)
    allocate(c1%b1(4,3), SOURCE=reshape((/(Child(4,20,20,4,20,4)(i,-i),i=1,12)/), &
     (/4,3/)))

    select type(name1=>pack(c1%b1, reshape(m1, (/4,3/)), &
     reshape(c1%b1, (/8/))))
        type is (Child(4,*,*,4,*,4))
            print *, name1
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
