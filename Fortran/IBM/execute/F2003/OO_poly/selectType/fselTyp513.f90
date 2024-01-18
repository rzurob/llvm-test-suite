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
!*  DATE                       : 05/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : select type (IO and private component with
!                               select type)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        private
        integer i
    end type

    contains

    subroutine printBase (b)
        class(base), intent(in) :: b

        select type (b)
            type is (base)
                print *, b
        end select
    end subroutine

    subroutine setVal (b, i)
        class (base), intent(inout) :: b
        integer, intent(in) :: i

        b%i = i
    end subroutine
end module

program fselTyp513
use m
    type(base) :: b1

    call setVal (b1, 100)

    call printBase (b1)
end
