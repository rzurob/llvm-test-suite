! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316642)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(3) :: char(3, 5)

    char = '123'

    associate (x=>fun(char))
        associate (item => x(1:2)//'aa')
            if (size(item) /= 2) error stop 1_4
            if (len(item) /= 6) error stop 2_4

            if (any(item /= '123 aa')) error stop 3_4
        end associate
    end associate

    contains

    function fun(char)
       character(3) :: char(3, 5)
       character(:), allocatable :: fun(:)

       fun =maxval(char//' ', dim=1)
    end function
end

