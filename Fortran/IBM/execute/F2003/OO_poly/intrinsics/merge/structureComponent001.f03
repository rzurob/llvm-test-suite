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
        type(Base1) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1
    class(Base1), pointer :: b1(:,:)
    logical :: m1(5,5)

    allocate(b1(5,5), SOURCE=reshape((/(Base1(i,i+1),i=1,25)/),(/5,5/)))
    c1%b2 = reshape((/(Base1(i,i-1),i=1,20)/), (/5,5/), &
     (/Base1(88,99)/), (/2,1/))
    m1 = reshape((/ .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE. /), (/5,5/))

    associate(name1=>merge(c1%b2, b1, m1))
        if(.NOT. same_type_as(name1, Base1(1,2))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    select type(name1=>merge(b1, c1%b2, m1))
        type is (Base1)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end