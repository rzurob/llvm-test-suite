! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/pack/structureComponent005.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_poly/intrinsics/pack
! PROGRAMMER                 : Yong Du
! DATE                       : 02/21/2005
! PRIMARY FUNCTIONS TESTED   : pack
! DRIVER STANZA              : xlf90
! DESCRIPTION                : ARRAY is a structure component, which
!                              is a scalar. The object containing the
!                              component is an array and is poly.
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

    type, extends(Base) :: Child    ! (4,20)
        type(Base(k1,n1)) :: b1
        integer(k1) j
    end type
end module

program structureComponent005
use m
    class(AbstractParent(4,:)), allocatable :: c1(:,:,:)
    class(*), pointer :: v1(:)
    logical :: m1(12)

    allocate(c1(2,2,3), SOURCE=reshape((/(Child(4,20)(i,Base(4,20)(-i),-i), &
     i=101,112)/), (/2,2,3/)))
    allocate(v1(8), SOURCE=(/(Base(4,20)(i),i=1,8)/))
    m1 = (/.FALSE.,.TRUE.,.TRUE.,.FALSE.,.TRUE., &
     .TRUE.,.FALSE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

    select type(c1)
        type is (Child(4,*))
            print *, pack(c1%b1, reshape(m1,(/2,2,3/)), v1)
            associate(name1=>pack(c1%b1, reshape(m1,(/2,2,3/)), v1))
                if(.NOT. same_type_as(name1, Base(4,20)(1))) error stop 1_4
                print *, name1
                print *, shape(name1)
            end associate
        class default
            error stop 2_4
    end select
end
