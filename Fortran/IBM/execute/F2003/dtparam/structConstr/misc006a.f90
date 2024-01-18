! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/07/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 317035)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program misc006a
    type A
        character(21) :: name
    end type

    type (A) a1

    a1 = A(genString(2))

    if (a1%name /= genString(2)) error stop 1_4

    a1%name = genString(2)
    if (a1%name /= genString(2)) error stop 2_4

    contains

    recursive character(i*10) function genString (i)
        integer, intent(in) :: i

        if (i <= 0) then
            return
        else
            genString = genString(i-1) // repeat(char(ichar('A')+i-1), 10)
        end if
    end function
end

