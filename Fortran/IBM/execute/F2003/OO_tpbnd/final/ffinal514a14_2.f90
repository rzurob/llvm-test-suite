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
!*  DATE                       : 02/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of temporaries created
!                               by function call in associate construct)
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
        integer(4) :: id

        contains

        final :: finalizeBase, finalizeBaseRank1
    end type

    type, extends (base) :: child
        character(20) :: name

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeBaseRank1 (b)
        type (base), intent(in) :: b (:)

        print *, 'finalizeBaseRank1'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(in) :: c (:)

        print *, 'finalizeChildRank1'
    end subroutine
end module

module m1
use m
    interface makeData
        type (base) function makeBaseArray (i, n)
        use m
            dimension makeBaseArray (n)
            integer(4), intent(in) :: i, n
        end function

        type (child) function makeChildArray (i, name, n)
        use m
            dimension makeChildArray (n)
            integer(4), intent(in) :: i, n
            character(*), intent(in) :: name
        end function
    end interface
end module

program ffinal514a14_2
use m1
    associate (x => makeData (10, 2))
    end associate

    print *, 'test 2'

    associate (x1 => makeData (20, 'child_type_temp', 3))
    end associate

    print *, 'end'
end

type (base) function makeBaseArray (i, n)
use m
    dimension makeBaseArray (n)
    integer(4), intent(in) :: i, n

    makeBaseArray%id = i
end function


type (child) function makeChildArray (i, name, n)
use m
    dimension makeChildArray (n)
    integer(4), intent(in) :: i, n
    character(*), intent(in) :: name

    makeChildArray%id = i
    makeChildArray%name = name
end function
