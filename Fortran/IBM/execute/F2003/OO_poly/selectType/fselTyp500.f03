! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (a test using unlimited poly
!                               function return)
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

program fselTyp500

    interface
        function producePtrForAnything (x)
            class (*), pointer :: producePtrForAnything
            class (*), intent(in) :: x
        end function
    end interface

    select type (x => producePtrForAnything (100_8))
        type is (integer(8))
            print *, x
        type is (integer(4))
            print *, x * 2
    end select

end

function producePtrForAnything (x)
    class (*), pointer :: producePtrForAnything
    class (*), intent(in) :: x

    allocate (producePtrForAnything, source=x)
end function