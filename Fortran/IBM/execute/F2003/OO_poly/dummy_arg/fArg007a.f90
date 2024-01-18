!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg007a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (nullified pointer and
!*                               unallocated allocatables allowed in the
!*                               intrinsic function same_type_as)
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

    class (base), pointer :: b_ptr => null()

    class (base), allocatable :: b_alloc(:)
end module

program fArg007a
use m
    class (*), pointer :: x1 (:)
    class (*), allocatable :: x2

    nullify (x1)

    if (.not. same_type_as (b_ptr, b_alloc)) error stop 1_4

    if (same_type_as (x1, x2)) error stop 2_4
end
