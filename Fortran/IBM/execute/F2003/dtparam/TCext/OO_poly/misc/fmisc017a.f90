! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc017a.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellanenous items (defect 288921)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id
    end type

    contains

    integer(4) function IDSum (b)
        type (base(*,4)), intent(in) :: b (2,2)

        associate (x => b)
            IDSum = sum (x%id)
        end associate
    end function
end module

program fmisc017a
use m
    type (base(20,4)) :: b1(5)

    b1%id = (/1,2,3,4,5/)

    if (IDSum (b1(2)) /= 14) error stop 1_4
end

