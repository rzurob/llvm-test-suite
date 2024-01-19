! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg014.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (assumed-size array; basic
!                               test on poly-dummy-arg)
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
    end type

    contains

    subroutine abc (b)
        class (base(4)), intent(in) :: b(*)

        print *, b(:3)%id
    end subroutine
end module

use m
    type (base(4)) b1 (2, 3)

    type (base(4)) b2 (-1:4)

    b1%id = reshape ((/1,2,3,4,5,6/), (/2,3/))
    b2%id = (/1,2,3,4,5,6/)

    call abc(b1)

    call abc (b2(0))
end
