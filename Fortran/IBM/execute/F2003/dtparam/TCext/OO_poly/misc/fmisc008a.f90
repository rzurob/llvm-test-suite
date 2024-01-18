! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/misc/fmisc008a.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 286297)
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
end module

program fmisc008a
use m
type (base(4)) :: c (2:10)

associate (x => c%id)
    x = 10

    if ((any (x /= 10)) .or. (size(x) /= 9)) error stop 1_4

    print *, x
end associate

end

