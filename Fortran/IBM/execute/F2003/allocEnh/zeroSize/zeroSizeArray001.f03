! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/28/3006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Zero size array allocated as the result of the
!                               intrinsic assignment; test integer type, real
!                               type and logical type; Use array section of
!                               zero-sized array, or remapped pointer array of
!                               zero-size for the RHS.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program zeroSizeArray001
    integer(8), allocatable :: i1(:,:)
    real(16), allocatable :: d1(:)
    logical(8), allocatable :: l1(:,:,:)

    integer, allocatable, target :: u(:), v(:)

    integer, pointer :: i2(:)
    logical, target :: l2(100)

    logical, pointer :: l3(:,:,:)

    i1 = reshape(2_8**33*(/(i, i=0,6)/), (/3,2/))

    allocate (d1(100))

    u = (/1, 2/)

    v = (/integer :: /)

    i1 = i1(u,v)

    i2(0:) => v

    d1 = i2

    l3(0:-1,1:1,2:2) => l2(::3)

    l1 = l3

    if ((.not. allocated(i1)) .or. (.not. allocated(d1)) .or. &
        (.not. allocated(l1))) error stop 1_4

    if (any(shape(i1) /= (/2,0/)) .or. (size(d1) /= 0) .or. &
        any(shape(l1) /= (/0,1,1/))) error stop 2_4

end
