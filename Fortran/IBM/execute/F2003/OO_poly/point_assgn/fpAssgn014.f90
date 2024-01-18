!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn014.f
! %VERIFY: fpAssgn014.out:fpAssgn014.vf
! %STDIN:
! %STDOUT: fpAssgn014.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (module data pointers'
!                               dynamic types)
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

        contains

        procedure, nopass :: print => printBase
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure, nopass :: print => printChild
    end type

    class (base), pointer :: b1_m
    class (base), pointer :: b2_m (:) => null()

    DATA b1_m /null()/

    class (base), allocatable, target :: b3_m (:)

    contains

    subroutine printBase
        print *, 'base'
    end subroutine

    subroutine printChild
        print *, 'child'
    end subroutine
end module

program fpAssgn014
use m
    call b1_m%print
    call b2_m%print
    call b3_m%print

    allocate (b3_m(2:3), source=child(1,'b3_m'))

    b1_m => b3_m (3)

    b2_m => b3_m (2:2)

    call b1_m%print
    call b2_m%print
    call b3_m%print

    deallocate (b3_m)

    call b3_m%print

    nullify (b2_m)

    call b2_m%print
end
