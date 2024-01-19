! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc005a3_1.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (variables with allocatable component
!                               in source-expr)
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
        integer(k1), allocatable :: data (:)
    end type
end module

program falloc005a3_1
use m
    type (base(4)) :: b1
    type (base(4)), allocatable :: b2
    class (base(4)), pointer :: b3

    allocate (b2, source=b1)

    allocate (b3, source=b1)

    if (allocated (b2%data) .or. allocated (b3%data)) error stop 1_4
end
