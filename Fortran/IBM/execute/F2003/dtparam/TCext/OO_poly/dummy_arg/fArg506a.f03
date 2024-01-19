! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg506a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/21/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (allocatable component
!*                               deallocated for INTENT(OUT) actual arg)
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
        integer, kind            :: k1
        integer(k1), allocatable :: data
    end type

end module

program fArg506a
use m
    type (base(4)) :: b1

    allocate (b1%data)

    if (.not. allocated (b1%data)) error stop 1_4

    call abc (b1)

    if (allocated (b1%data)) error stop 3_4

    contains

    subroutine abc (c)
        class (base(4)), intent(out) :: c

        if (allocated(c%data)) error stop 2_4
    end subroutine
end
