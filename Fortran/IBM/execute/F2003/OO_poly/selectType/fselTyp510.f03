! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/06/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (select type used in pure
!                               procedure)
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

program fselTyp510
    call xyz (10)

    if (abc ((/100_8, 2_8/)) /= 1) error stop 1_4
    if (abc ((/1.0/)) /= 0) error stop 2_4

    contains

    pure subroutine xyz (x)
        class (*), intent(in) :: x

        select type (x)
            type is (integer)
            class default
        end select
    end subroutine

    pure function abc (x)
        class (*), intent(in) :: x(*)

        select type (x)
            type is (real)
                abc = 0
            class default
                abc = 1
        end select
    end function
end