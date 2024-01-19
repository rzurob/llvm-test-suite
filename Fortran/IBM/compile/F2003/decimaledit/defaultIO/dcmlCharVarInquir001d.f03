! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Diagnostic test on using expressions (not
!                               variable) for DECIMAL= in inquire statement.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dcmlCharVarInquir001d
    character(*), parameter :: decMode(2) = (/'POINT', 'COMMA'/)

    character(20) c1(10)

    inquire(1, decimal=decMode(1))      !<-- illegal

    inquire (2, decimal=getDecMode(10))   !<-- illegal

    inquire (2, decimal=c1(1:1))   !<-- illegal

    inquire (2, decimal=''//c1(1))   !<-- illegal

    contains

    character(:) function getDecMode(i)
        allocatable :: getDecMode

        allocate (character(i) :: getDecMode)
    end function

    subroutine bad (x)
        class(*), intent(in) :: x

        select type (x)
            type is (character(*))
                inquire (1, decimal=x)  !<-- illegal

        end select
    end subroutine
end
