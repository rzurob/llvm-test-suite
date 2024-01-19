! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (defined operator used as selector)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(4) id
    end type

    type, extends(base) :: child
        character (15) :: name
    end type

    interface operator(.gen.)
        class (base) function genData (b1)
        import base
            allocatable genData
            class (base), intent(in) :: b1
        end function
    end interface
end module


program fselType503a
use m
    select type (x => .gen. (base(10)))
        type is (base)
            print *, x
        type is (child)
            print *, x
    end select

    select type (x => .gen. (child (1, 'xlftest')))
        type is (base)
            print *, x
        type is (child)
            print *, x
    end select
end

class (base) function genData (b1)
use m, only : base
    allocatable genData
    class (base), intent(in) :: b1

    allocate (genData, source=b1)
end function
