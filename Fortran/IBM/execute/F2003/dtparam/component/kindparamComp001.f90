!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/16/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Kind type parameter and component: in
!                               declaration-type-spec: allocatable, pointer
!                               attributes.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        real(k), allocatable :: data(:)

        contains

        procedure :: sum4 => addBaseData4
        procedure :: sum8 => addBaseData8
        procedure :: sum16 => addBaseData16
    end type

    type container
        type(base), pointer :: p1 => null()
        class (base(8)), allocatable :: data(:,:)

        contains

        procedure :: sum4 => addCo4
        procedure :: sum8 => addCo8
        procedure :: sumAll => addCoAll
    end type

    contains

    real(4) function addBaseData4 (b)
        class (base), intent(in) :: b

        addBaseData4 = 0

        if (.not. allocated(b%data)) return

        addBaseData4 = sum (b%data)
    end function

    real(8) function addBaseData8 (b)
        class (base(8)), intent(in) :: b

        addBaseData8 = 0

        if (.not. allocated(b%data)) return

        addBaseData8 = sum (b%data)
    end function

    real(16) function addBaseData16 (b)
        class (base(16)), intent(in) :: b

        addBaseData16 = 0

        if (.not. allocated(b%data)) return

        addBaseData16 = sum (b%data)
    end function

    real(4) function addCo4 (co)
        class (container), intent(in) :: co

        if (associated(co%p1)) then
            addCo4 = co%p1%sum4()
        else
            addCo4 = 0
        end if
    end function

    real(8) function addCo8 (co)
        class (container), intent(in) :: co

        addCo8 = 0

        if (allocated(co%data)) then
            do i = lbound(co%data, 1), ubound(co%data, 1)
                do j = lbound(co%data, 2), ubound(co%data, 2)
                    addCo8 = addCo8 + co%data(i, j)%sum8()
                end do
            end do
        end if
    end function

    real(8) function addCoAll (co)
        class (container), intent(in) :: co

        addCoAll = co%sum4() + co%sum8()
    end function
end module

program kindparamComp001
use m
    logical(4), external :: precision_r4, precision_r8, precision_r6

    class(base(16)), allocatable :: b1

    type (container) co1

    if (.not. precision_r8(co1%sumAll(), 0.0d0)) error stop 1_4

    !! test co1
    allocate(co1%p1)
    allocate (co1%p1%data(5), source=(/(i*1.2e1, i=10,14)/))

    if (.not. precision_r4(co1%sum4(), 7.2e2)) error stop 2_4
    if (.not. precision_r8(co1%sumAll(), 7.2d2)) error stop 3_4

    allocate (co1%data(0:1, 0:1))

    allocate (co1%data(0,0)%data(2), source=(/1.0d0, 5.0d0/))
    allocate (co1%data(1,0)%data(3), source=(/7.0d0, 3.0d0, 9.0d0/))
    allocate (co1%data(1,1)%data(0:5), source=(/(i*2.0d0, i=6,1,-1)/))
    allocate (co1%data(0,1)%data(7), source=(/(2.0d0+i, i = 1, 7)/))

    if (.not. precision_r8(co1%sum8(), 1.09d2)) error stop 4_4

    if (.not. precision_r8(co1%sumAll(), 8.29d2)) error stop 5_4

    !! test b1
    allocate (b1)

    allocate (b1%data(6), source=(/(i*1.q0, i=1,6)/))

    if (.not. precision_r6(b1%sum16(), 2.1q1)) error stop 6_4
end
