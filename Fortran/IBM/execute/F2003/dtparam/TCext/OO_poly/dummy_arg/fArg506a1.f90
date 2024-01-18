! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg506a1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (allocatable components
!                               for dummy-arg with INTENT(OUT))
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

    type dataType(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data(:,:)
        class (*), allocatable :: data2(:)
    end type

end module

program fArg506a1
use m
    type (dataType(4)) :: b1

    allocate (base(4):: b1%data(2, 3), b1%data2(3))

    call abc (b1)

    if (allocated (b1%data)) error stop 5_4
    if (allocated (b1%data2)) error stop 6_4

    contains

    subroutine abc (c)
        class (dataType(4)), intent(out) :: c

        if (allocated(c%data)) error stop 1_4

        if (allocated(c%data2)) error stop 2_4
    end subroutine
end
