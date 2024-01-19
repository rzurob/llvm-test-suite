! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/27/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 312070)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc043a1

    external p1
    character(10) p1

    call printP (10, p1)

    contains

    subroutine printP (n, p1)
        procedure(character(n)) p1

        print *, p1()
    end subroutine
end

character(*) function p1()
    p1 = 'xlftest test case 101'
end function
