!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn001ab.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (unlimited polymorphic
!*                               pointer array assignment; test size(),
!*                               lbound() and ubound() intrinsics)
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

program fpAssgn001ab
    type base
        integer*4 :: i1
    end type

    type, extends (base) :: child

    end type

    class (*), pointer :: x(:), x1(:)

    class (base), pointer :: b_ptr(:)

    integer*4, target :: i(0:10)
    type (base), target :: b1 (1:10)
    type (child), target :: b2 (-10:1)

    x1 => i
    x => x1

    if ((size(x) /= 11) .or. (lbound(x, 1) /= 0) .or. &
        (ubound(x, 1) /= 10)) error stop 1_4

    x1 => b1
    x => x1

    if ((size(x) /= 10) .or. (lbound(x,1) /= 1) .or. (ubound(x,1) /= 10)) error stop 2_4

    x1 => b2
    x => x1

    if ((size(x) /= 12) .or. (lbound(x,1) /=-10) .or. (ubound(x,1) /=1)) error stop 3_4

    b_ptr => b2

    x1 => b_ptr
    x => x1

    if ((size(x) /= 12) .or. (lbound(x,1) /=-10) .or. (ubound(x,1) /=1)) error stop 4_4


    call test1 (b_ptr, -10, 1)

    call test1 (b1, 1, 10)

    contains

    !This subroutine test the (l/u)bound() and size()
    subroutine test1 (a, lb, ub)
        class (base), intent(in), target :: a(0:)
        integer*4, intent(in) :: lb, ub

        class (*), pointer :: x1(:)

        x1 => a

        if ((lbound(x1,1) /= 0) .or. (ubound(x1,1) /= ub-lb)) error stop 10_4

        if (size(x1) /= ub-lb+1) error stop 11_4
    end subroutine
end
