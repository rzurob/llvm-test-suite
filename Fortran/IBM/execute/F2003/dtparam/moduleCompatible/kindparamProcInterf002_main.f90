!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/09/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type-parameter in
!                               declaration-type-spec: procedure interface in
!                               proc-component-def-stmt.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program kindparamProcInterf002
use m
    type(base(k=selected_real_kind(prec1, range1))), external :: genBase4
    type(base(selected_real_kind(prec2, range2))), external :: genBase8

    type (baseGenerator) bg1

    type(base(4)) b1
    type(base(8)), allocatable :: b2(:)
    real(8) d1(100)

    logical(4) precision_r4, precision_r8

    allocate(b2(20))

    call random_number(d1)

    bg1%gen1 => genBase4
    bg1%gen2 => genBase8

    b1 = bg1%gen1(1.5, 20, 'test b1')

    do i = 1, 20
        b2(i) = bg1%gen2(d1(i:), i, 'test b2')
    end do

    !! verify the results
    if (b1%name /= 'test b1') error stop 1_4

    do i = 1, 20
        if (.not. precision_r4 (1.5e0+(i-1)*1.0e0, b1%data(i))) error stop 2_4

        if (b2(i)%name /= 'test b2') error stop 3_4

        do j = 1, i
            if (.not. precision_r8 (b2(i)%data(j), d1(i+j-1))) error stop 4_4
        end do
    end do
end

function genBase4 (start, n, name)
use m, only: base
    type(base(4)) genBase4

    real(4), intent(in) :: start
    integer, intent(in) :: n
    character(*), intent(in) :: name

    genBase4%name = name

    allocate(genBase4%data(n))

    genBase4%data = (/(start+i*1.0e0, i=0, n-1)/)
end function

function genBase8 (data, n, name)
use m, only: base
    type (base(8)) genBase8

    real(8), intent(in) :: data(n)
    integer, intent(in) :: n
    character(*), intent(in) :: name

    allocate (genBase8%data(n), source=data)

    genBase8%name = name
end function
