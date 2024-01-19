! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/02/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 320967)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    contains

    character(:) function genMode (i)
        integer, intent(in) :: i

        pointer :: genMode

        if (i >= 0) then
            allocate (genMode, source='COMMA')
        else
            allocate (genMode, source='POINT ')
        end if
    end function
end module


use m
    write (*, '(f10.2)', decimal=genMode(10)) 1.2
    write (*, '(f10.2)', decimal=genMode(-10)) 1.2
end
