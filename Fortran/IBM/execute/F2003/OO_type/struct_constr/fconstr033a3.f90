!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr033a3.f
! %VERIFY: fconstr033a3.out:fconstr033a3.vf
! %STDIN:
! %STDOUT: fconstr033a3.out
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
!                               poly-allocatable array component in structure
!                               constructor; use integer types as the dynamic
!                               types for the data-source; use array sections)
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

program fconstr033a3
    class(*), allocatable :: x1(:)

    class (*), pointer :: x2(:)

    type base
        class(*), allocatable :: data(:)
    end type

    type (base) :: b1

    integer(8), target :: i1(0:10)

    allocate (x1(0:10), source=(/(i,i=0,10)/))

    i1 = (/(i,i=0,10)/)

    !! test the array section using x1
    b1 = base(x1(2::3))

    if (.not. allocated (b1%data)) error stop 1_4

    if ((lbound(b1%data,1) /= 1) .or. (ubound(b1%data,1) /= 3)) error stop 2_4

    select type (y1 => b1%data)
        type is (integer)
            print *, y1
    end select

    !! test the array section using x2
    x2 => i1(2::3)

    b1 = base (x2)

    if (.not. allocated (b1%data)) error stop 3_4

    if ((lbound(b1%data,1) /= 1) .or. (ubound(b1%data,1) /= 3)) error stop 4_4

    select type (y1 => b1%data)
        type is (integer(8))
            print *, y1
    end select
end
