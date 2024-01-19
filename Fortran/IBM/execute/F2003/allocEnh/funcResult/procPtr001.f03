! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/17/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               A test using proc-ptr call as the expr for the
!                               intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    abstract interface
        function phony (p)
            real p(:)

            real, allocatable :: phony(:)
        end function
    end interface

    contains

    function genPtr (p)
        real p(:)

        real, allocatable :: genPtr(:)

        genPtr = p
    end function

    function genProcPtr (p)
        procedure(phony), pointer:: p
        procedure(phony), pointer :: genProcPtr

        if (associated(p)) then
            genProcPtr => p
        else
            genProcPtr => genPtr
        end if
    end function
end module

use m
    procedure(phony), pointer :: p
    procedure(phony) :: reverseOrder

    real, allocatable :: rp(:)

    logical(4), external :: precision_r4

    !! associate p with genPtr
    p => genProcPtr (null())

    if (.not. associated(p, genPtr)) error stop 1_4

    rp = [(i, i=1,10)]

    rp =  p(rp*1.e1)

    if (.not. allocated(rp)) error stop 2_4

    if (size(rp) /= 10) error stop 3_4

    do i = 1, 10
        if (.not. precision_r4 (rp(i), i*1.0e1)) error stop 4_4
    end do

    p => reverseOrder
    p => genProcPtr (p)

    if (associated(p, genPtr) .or. (.not. associated(p))) error stop 5_4

    if (.not. associated(p, reverseOrder)) error stop 6_4

    rp = p([rp, rp])

    if (size(rp) /= 20) error stop 7_4

    do i = 1, 10
        if (.not. precision_r4 (rp(i), (11-i)*1.0e1)) error stop 8_4

        if (.not. precision_r4 (rp(10+i), (11-i)*1.0e1)) error stop 9_4
    end do

end


function reverseOrder (p)
    real p(:)

    real, allocatable :: reverseOrder(:)

    reverseOrder = p(size(p):1:-1)
end function
