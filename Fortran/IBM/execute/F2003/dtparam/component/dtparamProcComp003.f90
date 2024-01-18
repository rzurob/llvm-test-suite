! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test the NOPASS attribute; involve length
!                               type parameters; function result is
!                               parameterized type that has specification
!                               expression for the length type parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        complex :: cx(n)

        procedure(genBase), pointer, nopass :: create => null()
    end type

    contains

    function genBase (cx, n)
        integer, intent(in) :: n
        complex, intent(in) :: cx(n)

        type(base(n)) :: genBase

        genBase%cx = cx
    end function
end module

program dtparamProcComp003
use m
    type(base(10)), allocatable :: b1(:)

    complex cx1(100)

    logical(4), external :: precision_x8

    cx1 = (/((i*1.0, i+1.0), i=1, 100)/)

    allocate (b1(10))

    b1(1)%create => genBase

    do i = 2, 10
        b1(i) = b1(1)%create(cx1(i), 10)
    end do

    b1(1) = b1(1)%create(cx1, 10)

    !! verdify
    do i = 1, 10
        do j = 1, 10
            if (associated(b1(i)%create)) error stop 1_4

            if (.not. precision_x8(b1(i)%cx(j), ((i+j-1)*1.0, (i+j)*1.0))) &
                    error stop 2_4
        end do
    end do
end
