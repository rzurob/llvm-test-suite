! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/06/2005
!*
!*  DESCRIPTION                : allocate (allocation objects are of zero-length
!                               character type)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc513a3
    class (*), allocatable :: x

    class (*), pointer :: x1(:)

    allocate (character(0) :: x)
    allocate (x1(-2), source='')

    if (.not. allocated (x) .or. (.not. associated (x1))) error stop 3_4

    select type (x)
        type is (character(*))
            if (len(x) /= 0) error stop 1_4
        class default
            error stop 2_4
    end select

    select type (x1)
        type is (character(*))
            if (len(x1) /= 0) error stop 5_4
            if (size(x1) /= 0) error stop 6_4
        class default
            error stop 8_4
    end select

    deallocate (x, x1)
end
