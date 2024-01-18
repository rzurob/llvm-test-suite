! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd511.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (test variables with SAVE
!*                               attributes)
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
        private
        integer(k1)   :: count = 0

        contains

        procedure :: increase => increaseBaseCount
        procedure :: total => baseCount
    end type

    contains

    subroutine increaseBaseCount (b)
        class (base(*,4)), intent(inout) :: b

        b%count = b%count + 1
    end subroutine

    integer*4 function baseCount (b)
        class (base(*,4)), intent(in) :: b

        baseCount = b%count
    end function
end module

program ftpbnd511
use m
    interface
        integer*4 function addCount()
        end function
    end interface

    do i = 1, 100
        if (addCount() /= i) error stop 1_4
    end do

    if (addCount() /= 101) error stop 2_4
end

integer*4 function addCount()
use m, only : base
    type (base(20,4)), save :: b1 = base(20,4)()

    call b1%increase

    addCount = b1%total()
end function
