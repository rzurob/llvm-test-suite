! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp /tstdev/OO_poly/misc/fmisc006.f
! opt variations: -qnol -qdeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items (defect 283328)
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

    function replicateBase (b)
        type (base(*,4)), intent(in) :: b
        type (base(20,4)), pointer :: replicateBase

        type (base(20,4)), target, save :: temp

        temp%id = b%id

        replicateBase => temp
    end function

    function replicateBase1 (b)
        type (base(*,4)), intent(in) :: b
        type (base(20,4)), allocatable :: replicateBase1

        allocate (replicateBase1)

        replicateBase1%id = b%id
    end function
end module

use m
    type (base(20,4)), save :: b1, b2

    b1%id = 10

    b2 = replicateBase (b1)

    print *, b2, replicateBase (b1)

    b2 = replicateBase1 (b1)

    print *, b2, replicateBase1 (b1)

    print *, 'end'
end
