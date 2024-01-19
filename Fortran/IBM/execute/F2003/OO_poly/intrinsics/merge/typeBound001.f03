! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    Cross testing type bound.
!*    Non-poly
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

    subroutine mergeWith(this, fs, mask)
        class(AbstractParent), intent(in) :: this
        class(AbstractParent), intent(in) :: fs(:)
        logical, intent(in) :: mask
        associate(name1=>merge(this, fs, mask))
            select type(name1)
                type is (Base)
                    print *, "Base", name1
                    print *, size(name1)
                    print *, shape(name1)
                type is (Child)
                    print *, "Child", name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select
        end associate
    end subroutine
end module

program typeBound001
use m
    type(Base) :: b1
    type(Child) :: c1

    b1%i = 7
    c1%i = 8
    c1%j = 9

    call b1%mergeWith((/Base(1),Base(2),Base(3)/), .TRUE.)
    call c1%mergeWith((/Child(1,-1),Child(2,-2),Child(3,-3)/), .FALSE.)
end
