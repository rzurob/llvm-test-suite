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
! %GROUP: fpAssgn023a4.f
! %VERIFY: fpAssgn023a4.out:fpAssgn023a4.vf
! %STDIN:
! %STDOUT: fpAssgn023a4.out
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
!*  DATE                       : 07/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : data pointer assignment (unlimited
!                               poly-pointer's finalization when used as
!                               structure component)
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

module m1
    type base
        integer*4 :: id
    end type

    type, extends (base) :: child
        integer*4, pointer :: data (:) => null()

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    type (child), save :: c1_m, c2_m

    contains

    subroutine finalizeChild (c)
        type (child), intent(inout) :: c

        if (associated (c%data)) then
            print *, 'deallocating data finalizeChild'
            deallocate (c%data)
        end if
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child), intent(inout) :: c(:)

        do i = 1, size (c)
            if (associated (c(i)%data)) then
                print *, 'deallocating', i, 'element of child'
                deallocate (c(i)%data)
            end if
        end do
    end subroutine
end module

module m
use m1
    type container
        class (*), pointer :: data (:) => null()

        contains

        final :: freeData, freeDataRank1
    end type

    contains

    subroutine freeData (co)
        type (container), intent(inout) :: co

        print *, 'freeData'
        if (associated (co%data)) then
            deallocate (co%data)
        end if
    end subroutine

    subroutine freeDataRank1 (co)
        type (container), intent(inout) :: co (:)

        print *, 'freeDataRank1'
        do i = 1, size (co)
            call freeData (co(i))
        end do
    end subroutine
end module


!! subroutine tests the scalar of type container
subroutine abc
use m
use m1
    class (base), pointer :: b1 (:)

    type (container) :: co

    allocate (b1(2), source=(/c1_m, c2_m/))

    co%data => b1
end subroutine


!! subroutine tests the rank-one array of type container
subroutine abc1
use m
use m1
    class (base), pointer :: b1(:), b2(:)

    type (container) :: co(3)

    allocate (b1(2:3), source=(/c1_m, child(1,null())/))
    allocate (b2(2:2), source=c2_m)


    co(1)%data => b1
    co(2)%data => b2
end subroutine


program fpAssgn023a4
use m1
    integer*4, pointer :: i1 (:), i2(:)

    allocate (i1(5), i2(3))

    c1_m%data => i1
    c2_m%data => i2

    print *, 'calling abc'

    call abc


    allocate (i1(5), i2(3))
    c1_m%data => i1
    c2_m%data => i2

    print *, 'calling abc1'

    call abc1
end
