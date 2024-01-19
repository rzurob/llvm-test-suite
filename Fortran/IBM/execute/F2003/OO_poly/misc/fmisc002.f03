! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 283383)
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
        integer*4 :: id = 0
    end type
end module

program fmisc002
use m
    interface
        function makeBaseArray (id, n)
        use m
            integer*4, intent(in) :: id, n
            type (base) :: makeBaseArray (n)
        end function
    end interface

    type (base) :: b1(2)

    b1 = makeBaseArray (10, 2)
    print *, b1
    print *, makeBaseArray (10, 2)
end

function makeBaseArray (id, n)
use m
    integer*4, intent(in) :: id, n
    type (base) :: makeBaseArray (n)

    makeBaseArray%id = id
end function

