!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*

!*  TEST CASE NAME             : ffinal004a2k
!*  PROGRAMMER                 : David Forster(derived from ffinal004a2)
!*  DATE                       : 2007-10-31
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*  DRIVER STANZA              : xlf2003
module m
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        real(kbase_1), pointer :: data => null()

        contains

        final :: finalizeArray1, finalizeArray2
    end type

    contains

    subroutine finalizeArray1 (b)
        type (base(8)), intent(inout) :: b(:) ! tcx: (8)

        do i = 1, size(b)
            if (associated (b(i)%data)) then
                write (*, '(a, f10.2)') 'before deallocate, data= ', b(i)%data
                deallocate (b(i)%data)
            end if
        end do
    end subroutine

    subroutine finalizeArray2 (b)
        type (base(8)), intent(inout) :: b(:,:) ! tcx: (8)

        do j = 1, size (b, 2)
            do i = 1, size (b, 1)
                if (associated (b(i,j)%data)) then
                    write (*, '(a, f10.2)') 'before deallocate, data= ', &
                            b(i,j)%data

                    deallocate (b(i,j)%data)
                end if
            end do
        end do
    end subroutine
end module

program ffinal004a2k
use m
    class (base(8)), pointer :: b1(:), b2(:,:) ! tcx: (8)

    allocate (b1(3), b2(2,2))

    do i = 0, 2, 2
        allocate (b1(i+1)%data, source=i*1.5_8)
    end do

    do j = 1, 2
        do i = 1, 2
            allocate (b2(i,j)%data, source=10._8*i + j)
        end do
    end do

    deallocate (b1, b2)

    print *, 'end'
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 3 changes
