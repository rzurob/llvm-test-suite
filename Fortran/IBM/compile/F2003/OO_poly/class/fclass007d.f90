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
!*  DATE                       : 04/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : CLASS keyword (un-definable pointer used as the
!                               actual-arg to be associated with INTENT(INOUT)
!                               dummy-arg)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    class (*) function makeData (x)
        class (*), intent(in) :: x

        pointer makeData

        allocate (makeData, source=x)
    end function
end module

program fclass007d
use m
    call bad1 (makeData(1.0))       !<-- illegal

    contains

    subroutine bad1 (x)
        class (*), pointer, intent(inout) :: x
    end subroutine
end
