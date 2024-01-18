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
!*  DATE                       : 02/06/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : finalization (elemental final binding get
!                               called if no rank matched for the final subs)
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
        integer(4), pointer :: id

        contains

        FINAL :: finalizeBase
        final :: finalizeBaseArray1
    end type

    contains

    elemental subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated (b%id)) then
            deallocate (b%id, stat=iFail)

            !! if the target is not eligible to be deallocated then nullify it
            if (iFail == 2) nullify (b%id)
        end if
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base), intent(inout) :: b(:)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine test1 (b)
        class (base), intent(out) :: b(:,:)

        do j = 1, size(b,2)
            do i = 1, size(b,1)
                if  (associated (b(i,j)%id)) then
                    call zzrc(int(10*i+j, 4))
                end if
            end do
        end do
    end subroutine
end module

program ffinal004
use m
    type (base) b1(2,2)
    type (base), allocatable :: b2(:,:)

    integer(4), target :: i1, i2

    nullify (b1(1,1)%id, b1(2,2)%id)

    allocate (b1(2,1)%id, source = 10)

    b1(1,2)%id => i1

    call test1 (b1)

    allocate (b2(3,2))

    allocate (b2(1,1)%id, b2(3,2)%id)

    nullify (b2(2,2)%id, b2(3,1)%id)

    b2(2,1)%id => i1
    b2(1,2)%id => i2

    call test1 (b2)
end
