! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 316643)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    call sub('123')

    contains

    subroutine sub(char)
        character(*) :: char

        associate (item=>char//'abc')
            if (item /= '123abc') error stop 1_4

            print *, item
        end associate
    end subroutine
end

