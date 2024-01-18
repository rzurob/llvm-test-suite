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
! %POSTCMD: dcomp falloc001d1_1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (C624: if a type-spec appears, it
!                               shall specify a type with which each
!                               allocate-object is type-compatible)
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
        integer*4 :: id
    end type

    type, extends (base) :: child
        character*20 :: name
    end type
end module

program falloca001d1_1
use m
    type (base), pointer :: b1

    class (child), allocatable :: c1
    class (*), pointer :: x(:)

    allocate (child :: b1)              !<-- illegal
    allocate (base :: x(100), c1)       !<-- illegal
end
