! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal532k
!*
!*  DATE                       : 2007-11-11 (original: 06/28/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : type bound
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of allocated pointer
!*                               component in INTENT(OUT) dummy-arg)
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

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id
    end type

    type container (kcontainer_1) ! kcontainer_1=4
       integer, kind :: kcontainer_1
        class (base(kcontainer_1)), pointer :: data(:) ! tcx: (kcontainer_1)

        contains

        final :: finalizeData, finalizeDataRank1
    end type

    contains

    elemental subroutine finalizeData (d)
        type (container(4)), intent(inout) :: d ! tcx: (4)
        integer(4) error

        if (associated (d%data)) deallocate (d%data, stat=error)
    end subroutine

    subroutine finalizeDataRank1 (d)
        type (container(4)), intent(inout) :: d (:) ! tcx: (4)

        integer(4) error

        do i = 1, size (d)
            if (associated (d(i)%data)) deallocate (d(i)%data, stat=error)
        end do
    end subroutine
end module

module m1
use m
    contains

    subroutine test1 (d)
        class (container(4)), intent(out) :: d ! tcx: (4)

        if (associated (d%data)) error stop 101_4
    end subroutine

    integer(4) function test2 (d)
        type (container(4)), intent(out) :: d (:) ! tcx: (4)

        test2 = 0

        do i = 1, size (d)
            if (associated (d(i)%data))     test2 = test2 + 1
        end do
    end function

    subroutine test3 (d)
        class (container(4)), intent(out) :: d (:,:) ! tcx: (4)

        do i = 1, ubound(d, 1)
            do j = 1, ubound (d, 2)
                if (associated (d(i,j)%data)) error stop 3_4
            end do
        end do
    end subroutine
end module

program ffinal532k
use m1
    class (container(4)), allocatable :: co1(:) ! tcx: (4)
    type (container(4)) co2 ! tcx: (4)

    class (container(4)), pointer :: co3 (:,:) ! tcx: (4)

    allocate (co1(3), co2%data(2), co3 (2,2))

    allocate (co1(1)%data(3), co1(2)%data(2), co1(3)%data(1))

    allocate (co3(1,1)%data(2), co3(2,2)%data(3), co3(2,1)%data(4))

    co3(1,2)%data => null()

    call test1 (co2)

    if (test2 (co1) /= 0) error stop 2_4

    call test3 (co3)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 1 changes
! type: container - added parameters (kcontainer_1) to invoke with (4) / declare with (4) - 8 changes
