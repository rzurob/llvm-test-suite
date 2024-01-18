! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/OO_poly/point_assgn/fpAssgn027a.f
! opt variations: -qnok -qnol -qdeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2005
!*
!*  DESCRIPTION                : data pointer assignment (type-bound is a
!                               function that returns a pinter)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn027a
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), pointer :: data(:)

        contains

        procedure, nopass :: makeDataPtr
    end type

    interface
        class(*) function makeDataPtr (x)
            class (*), intent(in) :: x(:)
            pointer makeDataPtr(:)
        end function
    end interface

    class (base(4,20)), allocatable :: b1

    allocate (b1)

    b1%data => b1%makeDataPtr((/1,2,3,4,5,6,7,8/))

    if (.not. associated (b1%data)) error stop 1_4

    if ((lbound(b1%data,1) /= 0) .or. (ubound(b1%data,1) /= 7)) error stop 2_4

    select type (x => b1%data)
        type is (integer)
            if (any(x(0::2) /= (/1,3,5,7/))) error stop 3_4
        class default
            error stop 5_4
    end select
end


class (*) function makeDataPtr (x)
    class (*), intent(in) :: x(:)
    pointer makeDataPtr(:)

    allocate (makeDataPtr(0:size(x)-1), source=x)
end function
