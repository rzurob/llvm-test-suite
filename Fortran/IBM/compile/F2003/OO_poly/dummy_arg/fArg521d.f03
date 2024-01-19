! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : array constructor (all elements in the array
!                               constructor shall have the same declared type)
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
        integer*4 :: id
    end type

    type, extends (base) :: child
        character*20 :: name
    end type

    interface makeData
        function makeBaseObj (id, n)
        import base
            integer*4, intent(in) :: id, n
            type (base) :: makeBaseObj (n)
        end function

        function makeChildObj (id, name, n)
        import child
            integer*4, intent(in) :: id, n
            character(*), intent(in) :: name
            type (child) :: makeChildObj (n)
        end function
    end interface
end module

program fArg521d
use m
    print *, (/makeData (1, 3), makeData (2, 'childData', 2)/)
end
