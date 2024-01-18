!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd501.f
! %VERIFY: ftpbnd501.out:ftpbnd501.vf
! %STDIN:
! %STDOUT: ftpbnd501.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (nopass binding for
!*                               disassociated pointers and unallocated
!*                               allocatables, poly or nonpoly; test both module
!*                               data and main program data)
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
        contains

        procedure, nopass :: print => printBase
    end type

    type, extends (base) :: child
        contains

        procedure, nopass ::  print => printChild
    end type

    type (child), allocatable :: c1_m(:)
    class (base), allocatable :: b1_m
    class (child), pointer :: c2_m (:) => null()

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module


program ftpbnd501

use m
    class (base), pointer :: b_ptr =>null()

    type (child), pointer :: c1

    type p
        type(base), pointer :: b1
    end type

    type (p) :: p1

    type (child), allocatable :: c_alloc(:)
    class (base), allocatable :: b_alloc
    class (child), allocatable :: c_alloc2

    p1 = p (null())
    c1 => null()

    call b_ptr%print
    call p1%b1%print
    call c1%print

    b_ptr => c1
    call b_ptr%print

    call c_alloc%print

    call b_alloc%print

    call c_alloc2%print

    call c1_m%print
    call b1_m%print
    call c2_m%print
    !! the printouts are: base, base, child, base, child, base, child, child,
    !base, child
end
