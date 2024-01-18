! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous item (defect 293688)
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

program fmisc014

    character(10) :: c1

    if (createString ('abcd', 2) /= 'ab') error stop 1_4

    c1 = createString ('abcdefghijklmn nmlkji', 5)

    if (c1 /= 'abcde') error stop 2_4

    contains

    character(l) function createString (c, l)
        character(*) c
        integer l

        allocatable createString

        character(l) x

        x = c

        allocate (createString)

        createString = x
    end function

end
