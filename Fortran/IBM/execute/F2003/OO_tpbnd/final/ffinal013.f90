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
!*  DATE                       : 02/10/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : final sub (finalization of a linked-list)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
    type base
        integer*4 :: id

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase', b%id
    end subroutine
end module

module m
    type container
        type (container), pointer :: next => null()
        class (*), pointer :: data => null()

        contains

        FINAL :: finalizeContainer
    end type

    contains

    subroutine finalizeContainer (c)
        type (container), intent(inout), target :: c

        type (container), pointer :: iterator

        iterator => c

        do while (associated(iterator))
            if (.not. associated(iterator%data)) exit

            deallocate (iterator%data)
            iterator => iterator%next
        end do
    end subroutine
end module

program ffinal013
use m
use m1

    type (base), pointer :: b_ptr1, b_ptr2, b_ptr3

    type (container), allocatable :: list
    type (container), target :: node2, node3

    allocate (b_ptr1, b_ptr2, b_ptr3)
    allocate (list)

    b_ptr1%id = 1
    b_ptr2%id = 2
    b_ptr3%id = 3

    node3%data => b_ptr3

    node2%data => b_ptr2
    node2%next => node3
    

    list%data  => b_ptr1
    list%next  => node2

    !! now invoke the container's finalizer
    print *, 'test'

    deallocate (list)

    print *, 'end'
end
