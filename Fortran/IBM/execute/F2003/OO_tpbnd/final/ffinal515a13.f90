!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/14/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of the temp created by
!                               the structure constructor in the pointer
!                               assignment statement)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id

        contains

        final :: finalizeBase
        procedure, nopass :: makeData => produceBasePtr
    end type

    type, extends(base) :: child
        character(10) :: name

        contains

        final :: finalizeChild
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    class (base) function produceBasePtr (b)
        pointer produceBasePtr
        class (base), intent(in) :: b

        allocate (produceBasePtr, source=b)
    end function
end module


program ffinal515a13
use m
    class (base), pointer :: b1, b2

    nullify (b2)

    b1 => b2%makeData(b2%makeData(child (1, 'test')))

    !! verify the results
    select type (b1)
        type is (child)
            print *, b1
        class default
            error stop 1_4
    end select
end
