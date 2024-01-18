! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 290654)
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

program fmisc007a
use m

    interface makeData
        type (base1) function makeBase1 (i, n)
        use m
            integer(4),intent(in) :: i, n
            dimension makeBase1(n)
        end function

        type (base2) function makeBase2 (name, n)
        use m
            character(*), intent(in) :: name
            integer(4),intent(in) :: n
            dimension makeBase2 (n)
        end function
    end interface

    associate (x => makeData (1,2), x1 => makeData('x1', 3))
        print *, x, x1
    end associate

    print *, makeData (1,2), makeData('x1', 3)
end

type (base1) function makeBase1 (i, n)
use m
    integer(4),intent(in) :: i, n
    dimension makeBase1(n)

    makeBase1%id = (/(j,j=i,i+n-1)/)
end function

type (base2) function makeBase2 (name, n)
use m
    character(*), intent(in) :: name
    integer(4),intent(in) :: n
    dimension makeBase2(n)

    makeBase2%name = (/(trim(name) // '_' // char(ichar('0')+i), i=1,n)/)
end function
