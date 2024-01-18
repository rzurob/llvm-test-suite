!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/28/2005
!*
!*  DESCRIPTION                : specific type bound (singleton pattern, sort
!                               of)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer(1), parameter :: SUCCESS = 0
    integer(1), parameter :: FAILURE = 1

    type single
        private
        integer(4), pointer :: data

        contains

        procedure, nopass :: initialize => assgnData
        procedure, nopass :: getInstance => getSingleObj
        procedure :: getVal => getSingleVal

        final :: finalizeSingle
    end type

    type (single), pointer, protected :: globalData => null()

    contains

    type (single) function getSingleObj ()
        pointer getSingleObj

        getSingleObj => globalData
    end function

    integer*1 function assgnData (val)
        integer(4), intent(in) :: val

        if (associated (globalData)) then
            assgnData = FAILURE
        else
            allocate (globalData)

            allocate (globalData%data, source=val)

            assgnData = SUCCESS
        end if
    end function

    integer(4) function getSingleVal (s)
        class (single), intent(in) :: s

        if (associated (globalData)) then
            getSingleVal = globalData%data
        else
            getSingleVal = 0
        end if
    end function

    elemental subroutine finalizeSingle (s)
        type (single), intent(inout) :: s

        if (associated (s%data)) then
            deallocate (s%data)
        end if
    end subroutine
end module

program ftpbnd511a
use m
    class (single), pointer :: s1_ptr, s2_ptr

    nullify (s1_ptr, s2_ptr)

    if (s1_ptr%initialize (100) /= SUCCESS) error stop 1_4

    s1_ptr => s1_ptr%getInstance()

    s2_ptr => s2_ptr%getInstance()

    if (s2_ptr%initialize (200) /= FAILURE) error stop 2_4

    if (.not. associated (s1_ptr, s2_ptr)) error stop 3_4

    if (s1_ptr%getVal() /= 100) error stop 4_4

    deallocate (s1_ptr)
end
