!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a12.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (function reference as the
!                               source-expr in ALLOCATE; use intrinsic types and
!                               implicit interface)
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

program falloc005a12
    real(8) f1
    complex(4) cx1

    logical precision_r8, precision_x8
    real(8), allocatable :: r1
    complex(4), pointer :: c1

    allocate (r1, source=f1(100_4))

    allocate (c1, source= cx1(1_4, 2_4))

    if (.not. precision_r8 (r1, 1.0d3)) error stop 1_4

    if (.not. precision_x8 (c1, (1.0e0, 5.0e-1))) error stop 2_4
end

real(8) function f1 (i)
    integer(4), intent(in) :: i

    f1 = sqrt(real(i,8)) * i
end function

complex(4) function cx1 (r,i)
    integer(4), intent(in) :: r, i

    cx1 = (1.0/real(r,4), 1.0/real(i,4))
end function
