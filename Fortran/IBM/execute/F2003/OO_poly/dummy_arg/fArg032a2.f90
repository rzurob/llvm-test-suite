! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (OPTINAL dummy arg)
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

module m
    type base
        integer*4 id
    end type

    contains

    subroutine test1 (x)
        class (base), pointer, optional :: x(:)

        call cba (x, present(x))
    end subroutine

    subroutine cba (x, l)
        class (base), pointer, optional :: x(:)
        logical, intent(in) :: l

        if (present(x) .neqv. l) error stop 1_4
    end subroutine
end module

program fArg032a2
use m
    class (base), pointer :: b1(:) => null()

    call test1 (b1)

    call test1
end

