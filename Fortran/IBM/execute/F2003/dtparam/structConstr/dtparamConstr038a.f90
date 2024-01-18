! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/20/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: A correct use of procedure pointer
!                               component for the case in dtparamConstr038d.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type procPtr (k)
        integer, kind :: k

        procedure(real(k)), pointer, nopass :: proc
    end type

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        type(procPtr(k)) :: x(n)
        procedure(select), pointer :: select
    end type

    abstract interface
        function select (b1, n)
        import
            class(base(4,*)), intent(in) :: b1
            integer, intent(in) :: n

            procedure(real(4)), pointer :: select
        end function
    end interface

    contains

    function doSelect (b1, n)
        class(base(4,*)), intent(in) :: b1
        integer, intent(in) :: n

        procedure(real(4)), pointer ::doSelect

        if ((n < 1) .or. (n > b1%n)) error stop 10

        doSelect => b1%x(n)%proc
    end function
end module

program dtparamConstr038d
use m
    procedure(real(4)), pointer :: r1, r2

    real(4), external :: getReal4

    logical(4), external :: precision_r4

    type (base(4, 100)) :: b1

    nullify(r1)

    b1 = base(4,100)((/(i*1.0, i=-31,167,2)/), procPtr(4)(r1), doSelect)

    r2 => b1%select(25)

    if (associated(r2)) error stop 5_4

    r1 => getReal4

    b1 = base(4,100)((/(i*1.0, i=-31,167,2)/), procPtr(4)(r1), doSelect)

    r2 => b1%select (10)

    if ((.not. associated(r1, r2)) .or. (.not. associated(r2, getReal4))) &
            error stop 1_4

    do i = 1, 16
        if (.not. precision_r4(r2(b1%data(i)), -sqrt(-b1%data(i)))) &
            error stop 2_4
    end do

    do i = 17, 100
        if (.not. precision_r4(r2(b1%data(i)), sqrt(b1%data(i)))) &
            error stop 3_4
    end do
end


real(4) function getReal4 (r)
    getReal4 = sign(sqrt(abs(r)), r)
end function
