! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/structureComponent001.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is a structure component, which is non-poly
!*  array. The object containing the component is a scalar.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type Base1(k2)    ! (4)
        integer, kind :: k2
        integer(k2)      m
        integer(k2)      n
    end type

    type, extends(Base) :: Child    ! (4,20)
        type(Base1(k1)) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child(4,20)) :: c1
    class(Base1(4)), pointer :: b1(:,:)
    logical :: m1(5,5)

    allocate(b1(5,5), SOURCE=reshape((/(Base1(4)(i,i+1),i=1,25)/),(/5,5/)))
    c1%b2 = reshape((/(Base1(4)(i,i-1),i=1,20)/), (/5,5/), &
     (/Base1(4)(88,99)/), (/2,1/))
    m1 = reshape((/ .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE. /), (/5,5/))

    associate(name1=>merge(c1%b2, b1, m1))
        if(.NOT. same_type_as(name1, Base1(4)(1,2))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    select type(name1=>merge(b1, c1%b2, m1))
        type is (Base1(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
