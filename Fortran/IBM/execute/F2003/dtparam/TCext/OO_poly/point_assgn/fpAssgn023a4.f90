! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn023a4.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends (base) :: child    ! (4)
        integer(k1), pointer :: data (:) => null()

        contains

        final :: finalizeChild, finalizeChildRank1
    end type

    type (child(4)), save :: c1_m, c2_m

    contains

    subroutine finalizeChild (c)
        type (child(4)), intent(inout) :: c

        if (associated (c%data)) then
            print *, 'deallocating data finalizeChild'
            deallocate (c%data)
        end if
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(4)), intent(inout) :: c(:)

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
    type container(k2)    ! (4)
        integer, kind :: k2
        class (*), pointer :: data (:) => null()

        contains

        final :: freeData, freeDataRank1
    end type

    contains

    subroutine freeData (co)
        type (container(4)), intent(inout) :: co

        print *, 'freeData'
        if (associated (co%data)) then
            deallocate (co%data)
        end if
    end subroutine

    subroutine freeDataRank1 (co)
        type (container(4)), intent(inout) :: co (:)

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
    class (base(4)), pointer :: b1 (:)

    type (container(4)) :: co

    allocate (b1(2), source=(/c1_m, c2_m/))

    co%data => b1
end subroutine


!! subroutine tests the rank-one array of type container
subroutine abc1
use m
use m1
    class (base(4)), pointer :: b1(:), b2(:)

    type (container(4)) :: co(3)

    allocate (b1(2:3), source=(/c1_m, child(4)(1,null())/))
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
