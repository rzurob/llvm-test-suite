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
!*  DESCRIPTION                : final sub (finalization of the temps after the
!                               select type construct; test arrays)
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

        final :: finalizeBase, finalizeBaseArray1
        procedure, nopass :: makeDataArray => produceBaseAlloc
    end type

    type, extends (base) :: child
        character(10) name

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(in) :: b(:)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child), intent(in) :: c(:)

        print *, 'finalizeChildArray1'
    end subroutine

    class (base) function produceBaseAlloc (b, n)
        allocatable :: produceBaseAlloc(:)
        class (base), intent(in) :: b
        intent(in) :: n

        allocate (produceBaseAlloc(n), source=b)
    end function
end module

program ffinal514b4
use m
    class (base), pointer :: b1
    type (child) :: c1

    nullify (b1)
    c1%id = 1
    c1%name = 'test'

    select type (x => b1%makeDataArray(c1, 2))
        type is (child)
            print *, x
        class default
            error stop 1_4
    end select

    print *, 'end'
end
