!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext045.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : EXTENDS (test that derived type introduced from
!                               other modules can be extended)
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

    type (base) empty
end module

module m1
use m
    type t
        real(4) :: x
    end type
end module

module m2
use m1
    type, extends (base) :: child
        integer(4) id
    end type
end module

program fext045
use m2
    class (base), allocatable :: b1

    if (.not. same_type_as (b1, empty)) error stop 1_4

    allocate (b1, source=child (id=100))

    if (sizeof (b1) /= 4) error stop 2_4

    if (.not. extends_type_of (b1, empty)) error stop 3_4
end
