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
!*  DATE                       : 11/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : CLASS keyword (entry statement used in external
!                               subprogram)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
    end type

    interface genData
        class(base) function genPtr(i)
        import
            pointer genPtr
        end function

        class(base) function genNULL ()
        import
            pointer genNULL
        end function
    end interface
end module

function genPtr(i) result (returnVal)
use m, only: base
    class(base), pointer :: returnVal

    allocate (returnVal, source=base(i))
    return

    entry genNull () result (returnVal)
    nullify (returnVal)

end function


program fclass023
use m
    class(base), pointer :: b1, b2

    b1 => genData(10)
    b2 => genData()

    if ((.not. associated(b1)) .or. associated(b2)) error stop 1_4

    if (b1%id /= 10) error stop 2_4

end
