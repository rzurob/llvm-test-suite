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
! %GROUP: ffuncRet001a1.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 11/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : function result (poly-function return results;
!                               use select type construct to verify the data
!                               type)
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
        integer(8) id
    end type

    type, extends(base) :: child
        character(15) :: name
    end type

    interface
        class(*) function producePtrOfAnyType (x)
            pointer producePtrOfAnyType
            class (*), intent(in) :: x
        end function
    end interface

end module

program ffuncRet001a1
use m

    class (*), allocatable :: x0

    allocate (x0, source=child(20, 'test2'))

    select type (x => producePtrOfAnyType (100_8))
        type is (integer(8))
            if (x /= 100) error stop 10_4
        class default
            error stop 1_4
    end select

    select type (x => producePtrOfAnyType (child(10, 'test1')))
        class is (base)
            select type (y => x)
                type is (child)
                    if ((y%id /= 10) .or. (y%name /= 'test1')) error stop 11_4
                class default
                    error stop 3_4
            end select
        class default
            error stop 2_4
    end select

    select type (x => producePtrOfAnyType (x0))
        type is (base)
            error stop 4_4
        type is (child)
            if ((x%id /= 20) .or. (x%name /= 'test2')) error stop 12_4
        class default
            error stop 5_4
    end select
end


class (*) function producePtrOfAnyType (x)
    pointer producePtrOfAnyType
    class (*), intent(in) :: x

    allocate (producePtrOfAnyType, source=x)
end function
