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
! %GROUP: fArg514a2.f
! %VERIFY: fArg514a2.out:fArg514a2.vf
! %STDIN:
! %STDOUT: fArg514a2.out
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
!*  DATE                       : 06/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (argument keyword used for
!                               type bounds)
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
    type base1
        integer(4) :: id

        contains

        procedure, pass (b1) :: print2 => printB1B2
        procedure :: print => printBase1
    end type

    type base2
        character(20) :: name

        contains

        procedure, pass (b2) :: print2 => printB1B2
        procedure :: print => printBase2
    end type

    contains

    subroutine printB1B2 (b1, b2)
        class (base1), intent(in) :: b1
        class (base2), intent(in) :: b2

        call b1%print
        call b2%print
    end subroutine

    subroutine printBase1 (b)
        class (base1), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBase2 (b)
        class (base2), intent(in) :: b

        print *, b%name
    end subroutine
end module


module m1
use m
    type, extends(base1) :: child1
        character(20) :: name

        contains

        procedure :: print => printChild1
        procedure, pass(b1) :: print2 => printB1ThenB2
    end type

    type, extends(base2) :: child2
        integer(4) :: id

        contains

        procedure :: print => printChild2
        procedure, pass(b2) :: print2 => printB2ThenB1
    end type

    contains

    subroutine printB1ThenB2 (b1, b2)
        class (child1), intent(in) :: b1
        class (base2), intent(in) :: b2

        call b1%print
        call b2%print
    end subroutine

    subroutine printB2ThenB1 (b1, b2)
        class (base1), intent(in) :: b1
        class (child2), intent(in) :: b2

        call b2%print
        call b1%print
    end subroutine


    subroutine printChild1 (b)
        class (child1), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printChild2 (b)
        class (child2), intent(in) :: b

        print *, b%name, b%id
    end subroutine
end module


program fArg514a2
use m1
    type (child1) :: c1 = child1 (1, 'c1')
    type (child2) :: c2

    type (base1) :: b1
    type (base2) :: b2

    c2%name = 'c2'
    c2%id = 2

    b1%id = 10
    b2%name = 'b2'

    call c1%print2 (b2 = child2('temp', 2))

    call c2%print2 (b1 = c1)

    call b1%print2 (c2)

    call b2%print2 (c1)
end
