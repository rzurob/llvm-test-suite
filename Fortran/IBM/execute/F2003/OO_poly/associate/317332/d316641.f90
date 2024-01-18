!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316641)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(3) :: char(3)
    char = '123'

    associate (item=>fun(char, 30))
        if (len(item) /= 30) error stop 1_4
        if (size(item) /= 3) error stop 2_4

        if (any (item /= '123')) error stop 3_4

        print *, item
    end associate

    contains

    function fun(char, n)
        character(*) :: char(3)
        integer n
        character(n) :: fun(3)
        fun = char
    end function
end

