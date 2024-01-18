! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg009a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; test the
!                               pointer component's association status)
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
        integer, kind        :: k1
        integer(k1), pointer :: id => null()
    end type

    integer*4, target :: id_m = 1
end module

program fArg009a1
use m
    class (base(4)), pointer :: b1

    allocate (b1, source=base(4)(id_m))

    call abc(b1)

    if (id_m /= 100) error stop 1_4


    call cba (b1)

    if (id_m /= 10) error stop 2_4

    deallocate (b1)

    contains

    subroutine abc (b)
        type (base(4)), value :: b

        if (.not. associated (b%id, id_m)) error stop 10_4

        b%id = 100
    end subroutine

    subroutine cba (b)
        class (base(4)) :: b

        if (.not. associated (b%id, id_m)) error stop 11_4

        call abc (b)

        b%id = b%id/10
    end subroutine
end
