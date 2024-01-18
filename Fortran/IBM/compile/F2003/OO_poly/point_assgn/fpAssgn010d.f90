!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fpAssgn010d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (C723: variable shall
!*                               have either the TARGET or POINTER attribute,
!*                               and shall not be an array section with a vector
!*                               subscript; unlimited poly-pointer array with
!*                               intrinsics)
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

program fpAssgn010d

    class (*), pointer :: x(:), x1(:)
    class (*), allocatable :: x2(:)

    integer*4, target :: i1(100)
    integer*4, target :: i2(4) = (/2, 5, 10, 20/)

    x => i1 (i2)    !! this is illegal

    x1 => x(i2)     !! this is illegal

    x1 => x2        !! this is illegal

    x => i2 ((/1/))   !! this is illegal
end
