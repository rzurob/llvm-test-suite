!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318782)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type base
        real(8) :: d
    end type

    type container
        type(base), allocatable :: b
    end type

    contains

    type(container) function genCo (d1)
        pointer :: genCo

        real(8), intent(in) :: d1

        allocate(genCo, source=container(base(d1)))

    end function

    subroutine printCo (co)
        type(container), value :: co

        if (allocated(co%b)) then
            write(*, '(f12.5)') co%b%d
        end if
    end subroutine
end module

use m
    call printCo (genCo(1.2d0))
end
