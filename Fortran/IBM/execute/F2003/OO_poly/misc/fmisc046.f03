! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/29/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 312602)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        procedure(p), pass(b), pointer :: p1
    end type

    contains

    subroutine p (a, b, c)
        class(base), intent(in) :: b

        write (*, '(2f12.2)') a, c
    end subroutine

end module

program fmisc046
use m
    type (base) b1

    b1%p1 => p

    call b1%p1(1.0, 3.0)
end
