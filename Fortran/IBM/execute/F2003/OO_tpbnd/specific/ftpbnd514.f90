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
! %GROUP: ftpbnd514.f
! %VERIFY: ftpbnd514.out:ftpbnd514.vf
! %STDIN:
! %STDOUT: ftpbnd514.out
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
!*  DATE                       : 03/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (non_overridable bindings
!*                               use in program; with PRIVATE components)
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
        integer*4, private :: id

        contains

        procedure, non_overridable :: assgnID => assgnID2Base
        procedure, non_overridable :: getID => getBaseID
        procedure :: print => printBase
    end type

    type, extends(base) :: child
        logical*2 :: flag

        contains

        procedure :: print => printChild
    end type

    contains

    integer*4 function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function

    subroutine assgnID2Base (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = i
    end subroutine

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%getID()
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%getID(), b%flag
    end subroutine
end module

module m1
use m, only : child

    type, extends(child) :: gen3
        character*20, private :: name

        contains

        procedure, non_overridable :: getName => getGen3Name
        procedure, non_overridable :: assgnName => assgnGen3Name
        procedure :: print => prinGen3
    end type

    contains

    character(20) function getGen3Name (g)
        class (gen3), intent(in) :: g

        getGen3Name = g%name
    end function

    subroutine assgnGen3Name (g, c)
        class (gen3), intent(inout) :: g
        character(*), intent(in) :: c

        g%name = c
    end subroutine

    subroutine prinGen3 (b)
        class (gen3), intent(in) :: b

        print *, b%getID(), b%flag, b%getName()
    end subroutine
end module

program ftpbnd514
use m
use m1, only : gen3

    class (base), pointer :: b_ptr
    type (child), target :: c1
    type (gen3), target :: g1

    call c1%assgnID (10)
    c1%flag = .false.

    call g1%assgnID (20)
    call g1%assgnName ('g1')
    g1%flag = (1< 10)

    if (c1%getID() /= 10) error stop 1_4

    if (g1%getID() /= 20) error stop 2_4

    if (g1%getName() /= 'g1') error stop 3_4

    call c1%print
    call g1%print
    call g1%child%base%print

    b_ptr => g1

    if (b_ptr%getID() /= 20) error stop 4_4

    call b_ptr%print
end
