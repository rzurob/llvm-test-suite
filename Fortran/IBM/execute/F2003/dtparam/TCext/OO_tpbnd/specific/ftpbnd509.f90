! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specific/ftpbnd509.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (external procedure as type
!*                               bound; pass binding; a simpliest test case)
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

        contains

        procedure :: print => printBase
    end type

    interface
        subroutine printBase (b)
        import base
            class (base(4)), intent(in) :: b
        end subroutine
    end interface
end module

program ftpbnd509
use m
    type (base(4)), target :: b1
    class (base(4)), pointer :: b_ptr

    b_ptr =>  b1

    b_ptr%id = 100

    call b1%print

    call b_ptr%print
end

subroutine printBase (b)
use m, only : base
    class (base(4)), intent(in) :: b

    print *, b%id
end subroutine
