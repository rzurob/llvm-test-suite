!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C490: data target used for the data
!                               pointer component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        integer(k), pointer :: id(:)
    end type
end module

program dtparamConstr020
use m
    abstract interface
        integer(8) function genPtr8 (i)
            integer(8), intent(in) :: i(:)
            pointer genPtr8(:)
        end function

        integer(4) function genPtr4 (i)
            integer(4), intent(in) :: i(:)
            pointer genPtr4(:)
        end function
    end interface

    procedure(genPtr8), pointer :: genPtrArray8Ptr

    procedure(genPtr4), pointer :: genPtrArray4Ptr

    procedure(genPtr8) :: genPtrArray8

    procedure(genPtr4) :: genPtrArray4

    type (base(4,35)) :: b1

    type(base(8, :)), allocatable :: c1

    logical(4), external :: precision_r4, precision_r8

    genPtrArray8Ptr => genPtrArray8
    genPtrArray4Ptr => genPtrArray4

    allocate (base(8,99) :: c1)

    b1 = base(4,35)(1.0, genPtrArray4Ptr((/(i, i=1,35)/)))

    c1 = base(8,99)((/(i*1.0d0, i=1,99)/), &
            genPtrArray8Ptr((/(i*2_8**0, i=1,99)/)))

    !! verify
    do i = 1, 35
        if (.not. precision_r4(b1%data(i), 1.0)) error stop 1_4

        if (.not. associated(b1%id)) error stop 2_4

        if (b1%id(i) /= int(sqrt(i*1.0d0))) error stop 3_4
    end do

    do i = 1, 99
        if (.not. precision_r8(c1%data(i), i*1.0d0)) error stop 4_4

        if (.not. associated(c1%id)) error stop 5_4

        if (c1%id(i) /= i / int(sqrt(i*1.0d0), 8)) error stop 6_4
    end do
end

integer(8) function genPtrArray8(i)
    integer(8), intent(in) :: i(:)

    pointer genPtrArray8(:)

    allocate (genPtrArray8(size(i)))

    genPtrArray8 = i / int(sqrt(i*1.0d0), 8)
end function


integer(4)  function genPtrArray4 (i)
    integer(4), intent(in) :: i(:)

    pointer genPtrArray4(:)

    allocate (genPtrArray4(size(i)), source=int(sqrt(i*1.0d0)))
end function
