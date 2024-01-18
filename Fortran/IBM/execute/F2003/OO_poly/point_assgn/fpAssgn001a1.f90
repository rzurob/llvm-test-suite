!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn001a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!*                               assigned to various types; use associated()
!*                               intrinsic to verify; scalars)
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
    type base
        integer id
    end type
end module

program fpAssgn001a1
use m
    class (*), pointer :: x => null()
    class (*), pointer :: x1 => null()

    integer*4, target :: y = 10
    type (base), target :: b1

    x => y

    if (.not. associated (x)) error stop 1_4

    if (.not. associated (x, y)) error stop 2_4

    x => b1

    if (.not. associated (x)) error stop 3_4
    if (.not. associated (x, b1)) error stop 4_4

    x => x1

    if (associated (x) .or. associated (x1)) error stop 5_4

    x1 => y

    x => x1

    if ((.not. associated (x1, x)) .or. (.not. associated (x, y))) error stop 6_4
end
