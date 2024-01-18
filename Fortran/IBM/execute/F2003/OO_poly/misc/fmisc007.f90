! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items(defect 289042)
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
    type base1
        integer(4) id
    end type

    type base2
        character(20) name
    end type
end module

program fmisc007
use m

    interface makeData
        type (base1) function makeBase1 (i)
        use m
            integer(4),intent(in) :: i
        end function

        type (base2) function makeBase2 (name)
        use m
            character(*), intent(in) :: name
        end function
    end interface

    associate (x => makeData (1), x1 => makeData('x1'))
        print *, x, x1
    end associate

end

type (base1) function makeBase1 (i)
use m
    integer(4),intent(in) :: i

    makeBase1%id = i
end function

type (base2) function makeBase2 (name)
use m
    character(*), intent(in) :: name

    makeBase2%name = name
end function
