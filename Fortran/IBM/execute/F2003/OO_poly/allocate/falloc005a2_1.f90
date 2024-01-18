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
! %GROUP: falloc005a2_1.f
! %VERIFY: falloc005a2_1.out:falloc005a2_1.vf
! %STDIN:
! %STDOUT: falloc005a2_1.out
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
!*  DESCRIPTION                : ALLOCATE (defined binary operator used in the
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
        integer(4) :: id = -1

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

        function addB1C1 (b1, c1)
        import base, child
            type (child) addB1C1
            type (child), intent(in) :: c1
            type (base), intent(in) :: b1
        end function

        function addC1B1 (c1, b1)
        import base, child
            type (child) addC1B1
            type (child), intent(in) :: c1
            type (base), intent(in) :: b1
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

type (child) function addB1C1 (b1, c1)
use m, only: base, child
    type (child), intent(in) :: c1
    type (base), intent(in) :: b1

    addB1C1%id = b1%id + c1%id
    addB1C1%name = c1%name
end function


type (child) function addC1B1 (c1, b1)
use m, only: base, child, addB1C1
    type (child), intent(in) :: c1
    type (base), intent(in) :: b1

    addC1B1 = addB1C1 (b1, c1)
end function

program falloc005a2_1
use m
    type (child), allocatable :: c1, c2(:)
    type (base), pointer :: b1

    allocate (c1, source=child(1,'xlftest') + base (10) + child(90, '101'))

    allocate (c2(2), source=base(10) + base(2) + child(8, 'xlftest'))

    allocate (b1, source=c1%base+base()+base(0))

    call c1%print

    call c2(1)%print
    call c2(2)%print

    call b1%print
end
