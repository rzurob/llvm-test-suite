! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg503.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : arg-association (value attribute for dummy-arg)
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
        integer(k1)   :: id
    end type

    contains

    subroutine printBase (b)
        type (base(4)), value :: b

        call printBaseNoVal (b)
    end subroutine

    subroutine printBaseNoVal (b)
        class (base(4)) :: b

        print *, b%id
    end subroutine

end module

program fArg503
use m
    type (base(4)) b1

    b1 = base(4) (10)

    call printBase(b1)
end
