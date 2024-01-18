!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn003a3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (tests on ASSOCIATED()
!*                               using size 1 array and different strides)
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

program fpAssgn003a3
    class (*), pointer :: x(:), x1(:), x2(:)

    type p
        integer i
    end type

    type (p), target :: p1 (100)

    x => p1(2:3:2)%i

    if (size (x) /= 1) error stop 1_4

    if (.not. associated (x, p1(2:2)%i)) error stop 2_4

    x => p1(::10)%i

    x1 => p1(::2)%i

    x2 => x1(::5)

    if (.not. associated (x, x2)) error stop 3_4
end
