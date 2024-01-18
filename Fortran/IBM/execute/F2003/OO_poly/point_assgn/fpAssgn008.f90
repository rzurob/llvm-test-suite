!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (unlimited poly-pointer
!                               as the structure component; used in pointer
!                               assignment)
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
    end type

    type, extends(base) :: child
        integer*4 :: id
    end type
end module

module m1
    type container
        class(*), pointer :: data => null()
    end type
end module

program fpAssgn008
use m
use m1
    type(container) :: c, cc

    integer*4, target :: i
    type (base), target :: b1
    type (child), target :: c1

    c%data => i

    if (.not. associated (c%data, i)) error stop 1_4

    nullify (c%data)

    c%data => b1

    if ((.not. associated (c%data)) .or. associated (c%data, b1)) error stop 2_4

    c%data => c1

    if (.not. associated (c%data, c1)) error stop 3_4

    cc = c

    if (.not. associated (cc%data, c1)) error stop 4_4

    c%data => c%data

    if (.not. associated (c%data, c1)) error stop 5_4
end
