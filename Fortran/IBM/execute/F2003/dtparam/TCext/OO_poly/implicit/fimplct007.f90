! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/implicit/fimplct007.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/16/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : IMPLICIT (implied entity's binding can be
!*                               invoked)
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
        integer(k1)   :: id = 1

        contains

        procedure :: print => printBase
        procedure, pass (b) :: addId => addVal2ID
    end type

    contains

    subroutine printBase(b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine addVal2ID (i, b)
        class (base(4)), intent(inout) :: b
        intent(in) ::i

        b%id = b%id + i
    end subroutine
end module

program fimplct007
use m
    implicit type(base(4)) (b)

    call b1%print

    call b2%addId (9)

    call b2%print
end
