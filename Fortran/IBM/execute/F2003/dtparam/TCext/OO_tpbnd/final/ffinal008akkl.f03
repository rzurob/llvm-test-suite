! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-31 (original: 04/11/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (step3 during the finalization
!                               process kick off the finalizations again for the
!                               parent components)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (kA_1) ! kA_1=8
       integer, kind :: kA_1
        real(kA_1), pointer :: data(:) => null()

        contains

        final :: finalizeAarray1, finalizeA
    end type

    type base (kbase_1,lbase_1) ! kbase_1,lbase_1=8,2
       integer, kind :: kbase_1
       integer, len :: lbase_1
        type (A(kbase_1)) a1(lbase_1) ! tcx: (kbase_1)
    end type

    type, extends(base) :: child
        character(10*lbase_1), pointer :: name(:) => null()

        contains

        final :: finalizeChild, finalizeChildArray1
    end type

    contains

    subroutine finalizeA (a1)
        type (A(8)), intent(inout) :: a1 ! tcx: (8)

        if (associated (a1%data)) then
            print *, 'deallocating data'

            deallocate (a1%data)
        end if
    end subroutine

    subroutine finalizeAarray1 (a1)
        type (A(8)), intent(inout) :: a1(:) ! tcx: (8)

        print *, 'finalizeAarray1'

        do i = 1, size(a1)
            call finalizeA (a1(i))
        end do
    end subroutine

    subroutine finalizeChild(c)
        type (child(8,*)), intent(inout) :: c ! tcx: (8,*)

        if (associated (c%name)) then
            print *, 'deallocating name'

            deallocate (c%name)
        end if
    end subroutine

    subroutine finalizeChildArray1 (c)
        type (child(8,*)), intent(inout) :: c(:) ! tcx: (8,*)

        print *, 'finalizeChildArray1'

        do i = 1, size(c)
            call finalizeChild (c(i))
        end do
    end subroutine
end module

program ffinal008akkl
use m
    class (base(8,:)), pointer :: b1, b2(:) ! tcx: (8,:)

    allocate (child(8,2) :: b1, b2(2)) ! tcx: (8,2)

    !! allocate the data components
    allocate (b1%a1(1)%data(2), b1%a1(2)%data(2))

    allocate (b2(1)%a1(1)%data(1:0), b2(2)%a1(1)%data(2), b2(2)%a1(2)%data(1))

    deallocate (b1)
    deallocate (b2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA_1) to invoke with (8) / declare with (8) - 3 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (8,2) / declare with (8,*) - 1 changes
! type: child - added parameters () to invoke with (8,2) / declare with (8,*) - 3 changes