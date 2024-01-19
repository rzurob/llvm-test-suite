! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005d9.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (if the dummy-arg is a
!*                               pointer, the actual arg shall be a pointer)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type
end module

program fArg005d9
use m
    class (base(4,:)), allocatable :: b1
    class (*), allocatable :: x1

    call abc (b1)

    call cba (x1)

    contains

    subroutine abc (b)
        class (base(4,:)), pointer :: b
    end subroutine

    subroutine cba (x)
        class (*), pointer :: x
    end subroutine
end
