! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/17/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Data target is a function reference whose
!                               result is a pointer of paramterized derived
!                               type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(genBase8), pointer :: gen
    end type

    abstract interface
        function genBase8 (b)
        import
            class(base(8,*)), intent(in) :: b

            type(base(8, :)), pointer :: genBase8
        end function
    end interface

    type container (k)
        integer, kind :: k

        type(base(k,:)), pointer :: data
        procedure(genBase8), nopass, pointer :: gen
    end type
end module

program dtparamConstr037a
use m
    procedure(genBase8) genBase
    type(base(8,:)), allocatable :: b1(:)

    type (container(8)) :: co1(10)

    logical(4), external :: precision_r8

    !! 1st test
    allocate (base(8, 100) :: b1(10))

    do i = 1, 10
        b1(i) = base(8,100)(2.0d-5*(/(100*i+j, j=0,99)/), genBase)
    end do

    do i = 1, 10
        co1(i) = container(8)(b1(i)%gen(), b1(i)%gen)
    end do

    deallocate (b1)

    allocate (base(8,25) :: b1(10))

    !! 1st verify
    do i = 1, 4
        if (associated(co1(i)%data) .or. &
            (.not. associated(co1(i)%gen, genBase))) error stop 1_4
    end do

    do i = 5, 10
        if ((.not. associated(co1(i)%gen, genBase)) .or. &
            (.not. associated(co1(i)%data))) error stop 2_4

        do j = 1, 100
            if (.not. precision_r8(co1(i)%data%data(j), 2.0d-5*(100*i+j-1))) &
                error stop 3_4
        end do
    end do


    !! 2nd test
    do i = 1, 10
        b1(i) = base(8,25)(log(1.0d0*(/(j, j=1,25)/)), null())
    end do

    co1 = container(8)(null(), genBase)

    do i = 1, 10
        co1(i) = container(8)(co1(i)%gen(b1(i)), genBase)
    end do

    !! 2nd verify
    if (b1%n /= 25) error stop 5_4

    do i = 1, 10
        if (.not. associated(co1(i)%data)) error stop 4_4

        do j = 1, 25
            if (.not. precision_r8 (co1(i)%data%data(j), log(1.0d0*j))) &
                error stop 6_4
        end do
    end do
end


function genBase (b)
use m
    class(base(8,*)), intent(in) :: b

    type(base(8, :)), pointer :: genBase

    if (sum(b%data) > 1.0) then
        allocate (genBase, source=b)
    else
        nullify(genBase)
    end if
end function
