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
!*  DATE                       : 02/15/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (intent(out) attr. for
!                               variable size dummy array)
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
        real(8), pointer :: data => null()

        contains

        final :: finalizeBase, finalizeBaseArray1
    end type

    contains
    
    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%data)) deallocate (b%data)
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(inout) :: b(:)

        print *, 'finalizeBaseArray1'

        do i = 1, size(b)
            if (associated (b(i)%data)) then
                write (*, '(a, f10.2)') 'deallocating data: val = ', b(i)%data
                deallocate (b(i)%data)

            end if
        end do
    end subroutine
end module

program fArg606
use m
    class (base), allocatable :: b1(:)

    allocate (b1(10))

    do i = 1, 5
        allocate (b1(i)%data, source=1.0_8*i)
    end do

    call test1 (b1, 10)

    do i = 1, 10
        write (*, '(f10.2)') b1(i)%data
    end do

    contains

    subroutine test1 (b, i)
        class (base), intent(out) :: b(i)
        integer, intent(in) :: i

        do j = 1, 10
            if (associated (b(j)%data)) error stop 10_4

            allocate (b(j)%data, source=10.0_8*j)
        end do
    end subroutine
end
