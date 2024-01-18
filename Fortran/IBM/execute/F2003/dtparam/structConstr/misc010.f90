!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/16/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 317114)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base8_225! (k, n)
        integer :: n = 225

        real(8) :: data(225)
        procedure(produceBasePtr), pointer :: gen8
    end type

    abstract interface
        function produceBasePtr (b1, d1)
        import
            class(base8_225), intent(in) :: b1
            real(8), intent(in) :: d1(:)

            type(base8_225), allocatable :: produceBasePtr
        end function
    end interface
end module

program dtparamConstr034a4
use m
    interface
        function genBase (b1, d1)
        use m
            class(base8_225), intent(in) :: b1
            real(8), intent(in) :: d1(:)

            type(base8_225), allocatable :: genBase
        end function
    end interface

    type (base8_225) :: b1

    real(8) :: d1(1000)

    logical(4), external :: precision_r8

    d1 = sqrt(1.0d0*(/(i, i=1,1000)/))

    b1 = base8_225(225,d1(300:524), genBase)

    b1 = b1%gen8(d1)

    !! verify
    do i = 1, 225
        if (.not. precision_r8 (b1%data(i), &
            sqrt(1.0d0*i) + sqrt(1.0d0*(i+299)))) error stop 1_4
    end do

    b1 = b1%gen8(d1(801:))

    do i = 1, 200
        if (.not. precision_r8 (b1%data(i), &
            sqrt(1.d0*i)+sqrt(1.d0*(i+299))+sqrt(1.d0*(i+800)))) error stop 2_4
    end do

    do i = 201, 225
        if (.not. precision_r8 (b1%data(i), &
            sqrt(1.0d0*i) + sqrt(1.0d0*(i+299)))) error stop 3_4
    end do
end


function genBase (b1, d1)
use m
    class(base8_225), intent(in) :: b1
    real(8), intent(in) :: d1(:)

    type(base8_225), allocatable :: genBase

    allocate (base8_225 :: genBase)

    if (b1%n > size(d1)) then
        genBase%data(:size(d1)) = b1%data(:size(d1)) + d1
        genBase%data(size(d1)+1:) = b1%data(size(d1)+1:)
    else
        genBase%data = b1%data + d1(:b1%n)
    end if

    genBase%gen8 => b1%gen8
end function
