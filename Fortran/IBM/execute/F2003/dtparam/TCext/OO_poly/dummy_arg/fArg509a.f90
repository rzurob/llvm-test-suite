! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg509a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (INTENT(OUT) attribute on
!                               assumed-shape array)
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
        integer(k1)   :: id = -1
    end type

    contains

    subroutine abc (b)
        class (base(4)), intent(out) :: b(2:)

    end subroutine
end module

program fArg509a
use m
    type (base(4)) :: b1 (3)
    b1%id = (/1,2,3/)

    call abc (b1)

    if (any (b1%id /= -1)) error stop 1_4
end
