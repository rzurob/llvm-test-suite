! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316639)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(3) :: char
    char = '123'

    associate (item=>fun(char))
        if (item /= '123') error stop 1_4
    end associate

    contains

    function fun(char)
        character(*) :: char
        character(len(char)) :: fun
        fun = char
    end function
end
