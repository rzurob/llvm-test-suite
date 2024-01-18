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
!*  DATE                       : 04/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : data pointer assignment (type-bound is a
!                               function that returns a pinter)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn027a
    type base
        class (*), pointer :: data(:)

        contains

        procedure, nopass :: makeDataPtr
    end type

    interface
        class(*) function makeDataPtr (x)
            class (*), intent(in) :: x(:)
            pointer makeDataPtr(:)
        end function
    end interface

    class (base), allocatable :: b1

    allocate (b1)

    b1%data => b1%makeDataPtr((/1,2,3,4,5,6,7,8/))

    if (.not. associated (b1%data)) error stop 1_4

    if ((lbound(b1%data,1) /= 0) .or. (ubound(b1%data,1) /= 7)) error stop 2_4

    select type (x => b1%data)
        type is (integer)
            if (any(x(0::2) /= (/1,3,5,7/))) error stop 3_4
        class default
            error stop 5_4
    end select
end


class (*) function makeDataPtr (x)
    class (*), intent(in) :: x(:)
    pointer makeDataPtr(:)

    allocate (makeDataPtr(0:size(x)-1), source=x)
end function
