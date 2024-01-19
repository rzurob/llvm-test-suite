! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Unlimited-poly
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
        contains
        procedure :: mergeWith
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    function mergeWith(this, fs, mask)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: fs
        logical, intent(in) :: mask(:,:)
        class(AbstractParent), pointer :: mergeWith(:,:)
        allocate(mergeWith(size(mask,DIM=1),size(mask,DIM=2)), &
         SOURCE=merge(this, fs, mask))
        select type(mergeWith)
            type is (Base)
                print *, "B", mergeWith
                print *, size(mergeWith)
                print *, shape(mergeWith)
            type is (Child)
                print *, "C", mergeWith
                print *, size(mergeWith)
                print *, shape(mergeWith)
            class default
                error stop 5_4
        end select
    end function
end module

program typeBound003
use m
    class(*), pointer :: b1
    logical :: m1(3,2)

    m1 = reshape((/.FALSE.,.TRUE.,.FALSE.,.FALSE., &
     .TRUE.,.FALSE./), (/3,2/))

    allocate(b1, SOURCE=Base(7))
    select type(b1)
        type is (Base)
            select type(name1=>b1%mergeWith(Base(2), m1))
                type is (Base)
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        class default
            error stop 2_4
    end select

    deallocate(b1)
    allocate(b1, SOURCE=Child(8,9))
    select type(b1)
        type is (Child)
            select type(name1=>b1%mergeWith(Child(3,4), m1))
                type is (Child)
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 3_4
            end select
        class default
            error stop 4_4
    end select
end
