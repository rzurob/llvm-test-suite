! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg508.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/30/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arg-association (INTENT(OUT) poly-dummy-arg
!*                               should initialize the actual arg if default
!*                               initialization exists)
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
        integer(k1)   :: id = 0
    end type
end module

program fArg508
use m
    type (base(4)) :: b1 = base(4) (10)

    call test1 (b1, -1)

    if (b1%id /= -1) error stop 2_4
    contains

    subroutine test1 (b, i)
        class (base(4)), intent(OUT) :: b
        integer*4, intent(in) :: i

        if (b%id /= 0) error stop 1_4

        b%id = i
    end subroutine
end
