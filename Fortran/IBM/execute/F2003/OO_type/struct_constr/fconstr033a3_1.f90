!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr033a3_1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (unlimited
!                               poly-allocatable component in structure
!                               constructor; use array sections as the data
!                               source)
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

program fconstr033a3_1
    class(*), allocatable :: x1(:)

    type base
        class(*), allocatable :: data(:)
    end type

    type (base) :: b1

    allocate (x1(0:10), source=(/(i,i=0,10)/))

    b1 = base(x1(2::3))

    if (.not. allocated (b1%data)) error stop 1_4

    if ((lbound(b1%data,1) /= 1) .or. (ubound(b1%data,1) /= 3)) error stop 2_4
end
