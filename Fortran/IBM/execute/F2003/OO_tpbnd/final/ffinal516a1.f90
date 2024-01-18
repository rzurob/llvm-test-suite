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
!*  DATE                       : 02/16/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of the structure
!                               constructor used in the specification
!                               expression)
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
        integer(selected_int_kind(3)), allocatable :: id(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (allocated (b%id)) then
            print *, 'deallocating id of', size(b%id), 'elements'

            deallocate (b%id)
        end if
    end subroutine

    pure integer function arraySize (b)
        class (base), intent(in) :: b

        if (allocated(b%id)) then
            arraySize = size (b%id)
        else
            arraySize = 0
        end if
    end function
end module

program ffinal516a1
use m
    class (*), allocatable :: x

    integer array1(3)

    array1 = (/1,2,3/)

    allocate (character(4*arraySize(base(array1))) :: x)

    print *, 'after allocating'

    select type (x)
        type is (character(*))
            if (len (x) /= 12) error stop 1_4

            write (x, '(3i4)') array1

            write (*, '(a)') x
        class default
            error stop 4_4
    end select
end
