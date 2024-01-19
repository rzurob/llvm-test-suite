! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/22/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test use of type-bound defined operator used in
!                               a defined operator for arrays.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id

        contains

        procedure :: addB1B2
        procedure :: idGreaterThan
        generic :: operator (+) => addB1B2
        generic :: operator (>) => idGreaterThan
    end type

    interface operator (+)
        procedure addB1B2Array
    end interface

    contains

    type(base) function addB1B2 (b1, b2)
        class(base), intent(in) :: b1, b2

        allocatable :: addB1B2

        addB1B2 = base(null())

        addB1B2%id = b1%id + b2%id
    end function

    type(base) function addB1B2Array (b1, b2)
        type (base), intent(in) :: b1(:,:,:), b2(:,:,:)

        allocatable :: addB1B2Array(:,:,:)

        if (any(shape(b1) /= shape(b2))) error stop 10

        addB1B2Array = reshape((/(((b1(i,j,k)+b2(i,j,k), i = 1, size(b1,1)), &
            j=1,size(b1,2)), k=1,size(b1,3))/), (/size(b1,1), size(b1,2), &
            size(b1,3)/))
    end function

    logical elemental function idGreaterThan (b1, id)
        class(base), intent(in) :: b1
        integer, intent(in) :: id

        idGreaterThan = b1%id > id
    end function
end module

program definedOp005
use m
    type(base), pointer :: b1(:,:,:)
    type(base), allocatable :: b2(:)

    allocate(b1(0:5, -1:6, 0:4))

    b1 = reshape((/(base(i), i=-110, 129)/), (/6,8,5/))

    b2 = pack (b1, b1+b1 > 0)

    if (.not. allocated(b2)) error stop 1_4

    if ((lbound(b2, 1) /= 1) .or. (ubound(b2, 1) /= 129)) error stop 2_4

    do i = 1, 129
        if (.not. allocated(b2(i)%id)) error stop 3_4

        if (b2(i)%id /= i) error stop 4_4
    end do
end
