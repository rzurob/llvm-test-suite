! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self /tstdev/OO_poly/misc/fmisc010b1_0.f
! opt variations: -qck -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/11/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 291510)
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

    type, extends(base) :: child(n1)    ! (4,15)
        integer, len  :: n1
        character(n1) :: name
    end type

    type dataType(k2)    ! (4)
        integer, kind                :: k2
        class(base(k2)), allocatable :: data
    end type
end module

program fmisc010b1_0
use m
    type (dataType(4)) :: d1 (10)

    d1 = (/(dataType(4)(child(4,15)(i, 'temp')), i = 1, 10)/)
end
