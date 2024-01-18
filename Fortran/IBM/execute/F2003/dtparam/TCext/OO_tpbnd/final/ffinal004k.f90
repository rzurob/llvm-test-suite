! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal004k
!*
!*  DATE                       : 2007-10-31 (original: 02/06/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: id

        contains

        FINAL :: finalizeBase
        final :: finalizeBaseArray1
    end type

    contains

    elemental subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        if (associated (b%id)) then
            deallocate (b%id, stat=iFail)

            !! if the target is not eligible to be deallocated then nullify it
            if (iFail == 2) nullify (b%id)
        end if
    end subroutine

    subroutine finalizeBaseArray1 (b)
        type (base(4)), intent(inout) :: b(:) ! tcx: (4)

        print *, 'finalizeBaseArray1'
    end subroutine

    subroutine test1 (b)
        class (base(4)), intent(out) :: b(:,:) ! tcx: (4)

        do j = 1, size(b,2)
            do i = 1, size(b,1)
                if  (associated (b(i,j)%id)) then
                    call zzrc(int(10*i+j, 4))
                end if
            end do
        end do
    end subroutine
end module

program ffinal004k
use m
    type (base(4)) b1(2,2) ! tcx: (4)
    type (base(4)), allocatable :: b2(:,:) ! tcx: (4)

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


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
