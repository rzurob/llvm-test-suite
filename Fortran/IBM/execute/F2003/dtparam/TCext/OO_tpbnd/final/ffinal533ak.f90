! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal533ak
!*
!*  DATE                       : 2007-11-12 (original: 02/15/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (deallocate the allocated allocatable
!                               subobjects)
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        real(kbase_1), allocatable :: data(:)

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base(8)), intent(inout) :: b ! tcx: (8)

        print *, 'finalizeBase'

        if (allocated (b%data)) deallocate (b%data)
    end subroutine
end module

module m1
use m
    type A (kA_1) ! kA_1=8
       integer, kind :: kA_1
        class(base(kA_1)), allocatable :: b1 ! tcx: (kA_1)
    end type

    type B (kB_1) ! kB_1=8
       integer, kind :: kB_1
        class (A(kB_1)), allocatable :: a1(:) ! tcx: (kB_1)
    end type
end module

program ffinal533ak
use m1
    type(B(8)), pointer :: b01(:) ! tcx: (8)

    allocate (b01(2))
    allocate (b01(1)%a1(1), b01(2)%a1(2))

    allocate (b01(1)%a1(1)%b1, b01(2)%a1(1)%b1, b01(2)%a1(2)%b1)
    allocate (b01(1)%a1(1)%b1%data(10), b01(2)%a1(2)%b1%data(2))

    deallocate (b01)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 2 changes
! type: A - added parameters (kA_1) to invoke with (8) / declare with (8) - 1 changes
! type: B - added parameters (kB_1) to invoke with (8) / declare with (8) - 1 changes
