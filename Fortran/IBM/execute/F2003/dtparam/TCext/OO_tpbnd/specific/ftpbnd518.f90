! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_tpbnd/specific/ftpbnd518.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (type-bound used as
!*                               specification expression)
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
        CONTAINS

        procedure, nopass :: lbound => lowerBound
        procedure, nopass :: ubound => upperBound
    end type

    contains

    integer function lowerBound (x)
        class (*), intent(in) :: x(:)

        lowerBound = lbound(x, 1)
    end function

    integer function upperBound (x)
        class (*), intent(in) :: x(:)

        upperBound = ubound(x, 1)
    end function
end module

program ftpbnd518
use m
    real*4 :: r1 (3:10)
    real*4, pointer :: r2(:)

    type (base(4,20)) :: b1

    call creatArray (r2, b1%lbound(r1), b1%ubound(r1))

    if (size (r2) /= size(r1)) error stop 1_4

    deallocate (r2)

    contains

    subroutine creatArray (r, lb, ub)
        real*4, pointer, intent(out) :: r(:)
        integer, intent(in) :: lb, ub

        allocate (r(ub-lb+1))
    end subroutine
end
