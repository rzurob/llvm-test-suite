! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/28/2006
!*
!*  DESCRIPTION                : miscellaneous(defect 316638)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    character(3), pointer :: char
    character(3), target :: tchar

    tchar = '123'
    char => tchar

    call sub(char)


    contains
    subroutine sub(char)
        character(*), pointer :: char

        if (.not. associated(char)) error stop 1_4

        associate (item=>char//'abc')
            if (len(item) /= 6) error stop 2_4

            if (item /= '123abc') error stop 3_4
        end associate
    end subroutine
end


