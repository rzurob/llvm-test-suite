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
! %GROUP: fArg005a3_3.f
! %VERIFY: fArg005a3_3.out:fArg005a3_3.vf
! %STDIN:
! %STDOUT: fArg005a3_3.out
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
!*  DATE                       : 05/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (this is a variation of
!*                               test case fArg005a3_2.f; deallocate statements
!*                               are removed before calling ALLOCATE for the
!*                               same pointer to expose some of the problems
!*                               doscovered during the development)
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

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine copyBase (b, b1)
        import base
            class (base), pointer, intent(out) :: b
            class (base), intent(in) :: b1
        end subroutine

        subroutine copyBaseArray (b, b1)
        import base
            class (base), pointer, intent(out) :: b(:)
            class (base), intent(in) :: b1 (:)
        end subroutine
    end interface

    class (base), pointer :: b1_m, b2_m(:)

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

program fArg005a3_2
use m
    class (base), pointer :: b1, b2(:)
    type (child) :: c1(3)
    type (base) :: b3(2)

    allocate (b1, source=child(1, 'b1_pointer'))

    call copyBase (b1_m, b1)

    call b1_m%print


    allocate (b1)

    b1%id = 2

    call b1%print

    call copyBase (b1_m, b1)

    call b1_m%print

    deallocate (b1, b1_m)

    c1%id = (/3,4,5/)
    c1%name = (/'c1_3', 'c1_4', 'c1_5'/)

    allocate (b2(3), source=c1)

    call copyBaseArray (b2_m, b2)

    if (size(b2_m) /= 3) error stop 1_4


    call b2_m(1)%print
    call b2_m(2)%print
    call b2_m(3)%print


    b3%id = (/6, 7/)

    allocate (b2(2), source=b3)

    call copyBaseArray (b2_m, b2)

    if (size(b2_m) /= 2) error stop 2_4


    call b2_m(1)%print
    call b2_m(2)%print

    deallocate (b2, b2_m)
end


subroutine copyBase (b, b1)
use m, only : base
    class (base), pointer, intent(out) :: b
    class (base), intent(in) :: b1

    allocate (b, source=b1)
end subroutine


subroutine copyBaseArray (b, b1)
use m, only : base
    class (base), pointer, intent(out) :: b (:)
    class (base), intent(in) :: b1 (:)

    allocate (b(size(b1)), source=b1)
end subroutine
