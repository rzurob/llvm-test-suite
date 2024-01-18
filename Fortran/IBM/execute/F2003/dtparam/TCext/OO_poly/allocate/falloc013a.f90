! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc013a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (each allocate object that was not
!*                              successfully allocated shall retain its previous
!                               allocation status or pointer association status)
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
    type base(k1)    ! (8)
        integer, kind        :: k1
        integer(k1), pointer :: data (:,:,:)
    end type
end module

program falloc013a
use m
    integer(4), parameter :: maxInt = 2**9

    integer error

    type (base(8)) :: b1

    nullify (b1%data)

    error = 0

    !! the required size of the allocate statement exceeds the heap memory
    !setting
    allocate (b1%data(maxInt,maxInt,maxInt), source=100_8, stat=error)

    if (error /= 1) error stop 1_4

    if (associated (b1%data)) error stop 2_4
end
