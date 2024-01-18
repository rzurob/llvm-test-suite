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
! %GROUP: falloc005a2.f
! %VERIFY: falloc005a2.out:falloc005a2.vf
! %STDIN:
! %STDOUT: falloc005a2.out
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
!*  DATE                       : 07/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (defined binary operator used in
!                               source-expr in ALLOCATE stmt)
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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator (+)
        function addB1B2 (b1, b2)
        import base
            type (base), intent(in) :: b1, b2
            type (base) addB1B2
        end function

        function addC1C2 (c1, c2)
        import child
            type (child), intent(in) :: c1, c2
            type (child) addC1C2
        end function
    end interface

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

type (base) function addB1B2 (b1, b2)
use m, only: base
    type (base), intent(in) :: b1, b2

    addB1B2%id = b1%id + b2%id
end function

type (child) function addC1C2 (c1, c2)
use m, only: child
    type (child), intent(in) :: c1, c2

    addC1C2%id = c1%id + c2%id
    addC1C2%name = trim (c1%name)//' '//trim(c2%name)
end function

program falloc005a2
use m
    class (base), pointer :: b1
    class (base), allocatable :: b2, b3(:)

    allocate (b1, source=child(1,'xlftest')+child(100, '101'))

    allocate (b2, source=base(10)+base(1))

    allocate (b3(10:11), source=b2 + base(100))

    call b1%print

    call b2%print

    call b3(10)%print
    call b3(11)%print

    deallocate (b1, b2, b3)
end
