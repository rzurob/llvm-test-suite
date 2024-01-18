!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffuncRet001a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return (unlimited poly function return
!                               results)
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

program ffuncRet001a
use ISO_C_BINDING

    interface
        class(*) function producePtrOfAnyType (x)
            pointer producePtrOfAnyType
            class (*), intent(in) :: x
        end function
    end interface

    type, bind(c) :: bType
        integer(c_short) :: i
        integer(c_int) :: j
    end type

    type (bType), pointer :: b1

    associate (x => producePtrOfAnyType (bType(10, 20)))
        b1 => x

        if ((b1%i /= 10) .or. (b1%j /= 20)) error stop 1_4
    end associate
end


class (*) function producePtrOfAnyType (x)
    pointer producePtrOfAnyType
    class (*), intent(in) :: x

    allocate (producePtrOfAnyType, source=x)
end function
