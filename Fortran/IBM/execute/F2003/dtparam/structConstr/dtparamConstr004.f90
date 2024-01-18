!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Still the procedure target in the
!                               structure constructor; pass attribute.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type container (k)
        integer, kind :: k

        class(base(k,:)), pointer :: data => null()
        procedure(genBase), pointer :: genData => null()
    end type

    abstract interface
        function genBase (co, proc)
        import
            class(container(8)), intent(in) :: co
            procedure(real(8)) proc

            class(base(8,:)), allocatable :: genBase
        end function
    end interface
end module

program dtparamConstr004
use m
    procedure(real(8)) getSqrt
    procedure(genBase) genBase8

    class(base(8,:)), pointer :: b1, b2

    type (container(8)) :: co1

    logical(4), external :: precision_r8

    allocate (base(8, 31) :: b1)

    b1%data = (/(i*1.0d0, i=1, 31)/)

    co1 = container(8)()

    if (associated(co1%data) .or. associated(co1%genData)) error stop 1_4

    co1 = container(8)(b1, genBase8)

    if ((.not. associated(co1%data, b1)) .or. &
        (.not. associated(co1%genData, genBase8))) error stop 2_4

    allocate (b2, source=co1%genData(getSqrt))

    !! verify b2
    if (b2%n /= 31) error stop 3_4

    do i = 1, 31
        if (.not. precision_r8(b2%data(i), sqrt(i*1.0d0))) error stop 4_4
    end do
end

function genBase8 (co, proc)
use m
    class(container(8)), intent(in) :: co
    procedure(real(8)) proc

    class(base(8,:)), allocatable :: genBase8

    if (associated(co%data)) then
        allocate (base(8,co%data%n) :: genBase8)

        do i = 1, co%data%n
            genBase8%data(i) = proc(co%data%data(i))
        end do
    end if
end function

real(8) function getSqrt (d1)
    real(8), intent(in) :: d1

    getSqrt = sqrt(d1)
end function
