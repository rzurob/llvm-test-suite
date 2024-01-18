!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn011a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (self assginment for
!*                               unlimited polymorphic pointers)
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

program fpAssgn011a
    logical, target :: l1

    character (10), target :: c1(10)

    real*4, pointer :: r1(:)

    class (*), pointer :: x, x1 (:)

    x => l1

    x => x

    if (.not. associated (x, l1)) error stop 1_4

    x1 => c1

    x1 => x1

    if (.not. associated (x1, c1)) error stop 2_4

    if (size(x1) /= 10) error stop 3_4

    x => x1(5)

    x => x

    if (.not. associated (x, c1(5))) error stop 4_4
end
