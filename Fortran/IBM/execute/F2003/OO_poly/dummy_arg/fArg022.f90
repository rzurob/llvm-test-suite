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
! %GROUP: fArg022.f
! %VERIFY: fArg022.out:fArg022.vf
! %STDIN:
! %STDOUT: fArg022.out
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
!*  DATE                       : 06/02/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-actual-arg
!                               associated with nonpoly-dummy-arg with
!                               INTENT(OUT) attribute)
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

    type, extends(child) :: gen3
        logical*1 :: isSet

        contains

        procedure :: print => printGen3
    end type

    contains

    subroutine modifyChild (c, id, name)
        type (child), intent(out) :: c
        integer*4, intent(in) :: id
        character(*), intent(in) :: name

        c = child (id, name=name)
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printGen3 (b)
        class (gen3), intent(in) :: b

        print *, b%id, b%name, b%isSet
    end subroutine

    subroutine modifyBase (b, id)
        type (base), intent(out) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine
end module

program fArg022
use m
    class (child), allocatable, target :: c1(:)

    class (base), pointer :: b1

    allocate (c1(2), source=gen3(1,'test_c1', .true.))

    call c1(1)%print
    call c1(2)%print

    call modifyChild (c1(1), 10, 'c1_1')

    b1 => c1(2)

    call modifyBase (b1, 20)

    call c1(1)%print
    call c1(2)%print
end
