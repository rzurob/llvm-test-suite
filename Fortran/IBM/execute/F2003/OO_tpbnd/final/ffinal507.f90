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
!*  DESCRIPTION                : final sub (finalization of the zero-size array)
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
        integer, pointer :: id => null()

        contains

        final :: finalizeBase
    end type

    type, extends(base) :: child
        character(20), pointer :: name => null()

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    interface assignment(=)
        module procedure b1Assgnb2
    end interface

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        print *, 'finalizeBase'

        if (associated (b%id)) then
            print *, 'deallocate id'

            deallocate (b%id)
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        print *, 'finalizeChild'

        if (associated (c%name)) then
            print *, 'deallocate name'

            deallocate (c%name)
        end if
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child), intent(inout) :: c(:)

        print *, 'finalizeChildArray1'

        do i = 1, size (c)
            call finalizeChild (c(i))
        end do
    end subroutine

    subroutine b1AssgnB2 (b1, b2)
        class (base), intent(out) :: b1(:)
        class (base), intent(in) :: b2(:)

        if (.not. same_type_as (b1, b2)) error stop 10_4

        if (size (b1) /= size(b2)) return

        do i = 1, size (b2)
            if (associated (b2(i)%id)) allocate (b1(i)%id, source=b2(i)%id)

            select type (b1)
                type is (base)
                type is (child)
                    select type (b2)
                        type is (child)
                            if (associated (b2(i)%name)) &
                                    allocate (b1(i)%name, source=b2(i)%name)
                        class default
                            error stop 14_4
                    end select
                class default
                    error stop 15_4
            end select
        end do
    end subroutine
end module

program ffinal507
use m
    class (base), allocatable :: b1(:), b2(:)

    allocate (child :: b1(1:0), b2(2))

    allocate (b2(1)%id, b2(2)%id)

    b1 = b2

    print *, 'end'
end
