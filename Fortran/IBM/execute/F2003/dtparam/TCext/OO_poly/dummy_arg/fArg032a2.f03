! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg032a2.f
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    contains

    subroutine test1 (x)
        class (base(4)), pointer, optional :: x(:)

        call cba (x, present(x))
    end subroutine

    subroutine cba (x, l)
        class (base(4)), pointer, optional :: x(:)
        logical, intent(in) :: l

        if (present(x) .neqv. l) error stop 1_4
    end subroutine
end module

program fArg032a2
use m
    class (base(4)), pointer :: b1(:) => null()

    call test1 (b1)

    call test1
end
