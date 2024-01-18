!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg026a_1.f
! %VERIFY: fArg026a_1.out:fArg026a_1.vf
! %STDIN:
! %STDOUT: fArg026a_1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (implicit interface)
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

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg026a_1
use m
    type (base) :: b1 (10)
    class (base), allocatable :: b2 (:)
    type (child), target :: c1 (5)

    class (base), pointer :: b3(:)

    b1 = (/(base (i), i=10,1,-1)/)
    c1 = (/(child(i, 'c1'), i=1,5)/)

    allocate (b2 (5), source=c1)

    allocate (b3 (3))

    b3%id = (/10,20,30/)


    call print3 (b1)

    call print3 (b2)

    call print3 (b3)

    deallocate (b3, b2)
end

subroutine print3 (b)
    use m
    type (base), intent(in) :: b(*)

    call b(1)%print
    call b(2)%print
    call b(3)%print
end subroutine
