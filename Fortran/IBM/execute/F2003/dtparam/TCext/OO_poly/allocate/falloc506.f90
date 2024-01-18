! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc506.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (allocate statement deemed to set the
!                               return value in a function; defect 289943; a
!                               clean compilation is expected for this test
!                               case)
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
        class (*), pointer :: data
    end type

    contains

    type (base(4,20)) function createBase (data)
        class (*), intent(in) :: data

        !! this statement sets the value for return result
        allocate (createBase%data, source=data)
    end function
end module

program falloc506
use m
    type (base(4,20)) :: b1

    b1 = createBase ((1.0d0, 0.0d0))

    if (.not. associated (b1%data)) error stop 1_4
end
