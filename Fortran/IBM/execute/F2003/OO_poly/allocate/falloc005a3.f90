!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc005a3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (variables with allocatable component
!                               in source-expr)
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
        integer(4), allocatable :: data (:)
    end type
end module

use m
    type (base) :: b1
    type (base), allocatable :: b2
    class (base), pointer :: b3

    allocate (b1%data(2:3), source=(/2,3/))

    allocate (b2, source=b1)

    if (.not. allocated (b2%data)) error stop 1_4

    if ((lbound (b2%data, 1) /= 2) .or. (ubound (b2%data, 1) /= 3)) error stop 2_4

    if (any (b2%data /= (/2,3/))) error stop 3_4


    allocate (b3, source=b1)

    if (.not. allocated (b3%data)) error stop 4_4

    if ((lbound (b3%data, 1) /= 2) .or. (ubound (b3%data, 1) /= 3)) error stop 5_4

    if (any (b3%data /= (/2,3/))) error stop 6_4
end

