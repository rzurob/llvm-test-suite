! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/20/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (class(*) as the structure
!*                               component; more tests on associated())
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
    type base
    end type

    type, extends(base) :: child
        integer*4 :: id = 1
    end type
end module

program fpAssgn003a2
use m
    type dataType
        class(*), pointer :: data
    end type

    integer*4, target :: i (200)

    class (child), pointer :: c_ptr(:)
    class (base), pointer :: b_ptr(:)

    type (dataType), allocatable :: d1(:)

    allocate (d1(100))
    allocate (c_ptr(200))

    d1 = (/(dataType (data = c_ptr(j)), j=1,200,2)/)

    do j = 199, 1, -2
        if (.not. associated (d1((j+1)/2)%data, c_ptr(j))) error stop 1_4
        if (associated (d1((j+1)/2)%data, c_ptr(j)%base)) error stop 2_4
    end do

    b_ptr => c_ptr(::2)

    d1 = (/(dataType (data = b_ptr(j)), j=1,100)/)

    do j = 1, 100
        if (.not. associated (d1(j)%data, b_ptr(j))) error stop 3_4
        if (.not. associated (d1(j)%data, c_ptr(2*j-1))) error stop 4_4
    end do

    d1 = (/(dataType (data = i(j)), j=1,200,2)/)

    do j = 1, 200, 2
        if (.not. associated (d1((j+1)/2)%data, i(j))) error stop 5_4
    end do

    deallocate (c_ptr)

end