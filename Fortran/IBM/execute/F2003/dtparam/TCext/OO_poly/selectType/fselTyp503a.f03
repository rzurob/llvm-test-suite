! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/OO_poly/selectType/fselTyp503a.f
! opt variations: -qnock -ql

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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface operator(.gen.)
        class (base(4)) function genData (b1)
        import base
            allocatable genData
            class (base(4)), intent(in) :: b1
        end function
    end interface
end module


program fselType503a
use m
    select type (x => .gen. (base(4)(10)))
        type is (base(4))
            print *, x
        type is (child(4,1,*))
            print *, x
    end select

    select type (x => .gen. (child(4,1,15) (1, 'xlftest')))
        type is (base(4))
            print *, x
        type is (child(4,1,*))
            print *, x
    end select
end

class (base(4)) function genData (b1)
use m, only : base
    allocatable genData
    class (base(4)), intent(in) :: b1

    allocate (genData, source=b1)
end function
