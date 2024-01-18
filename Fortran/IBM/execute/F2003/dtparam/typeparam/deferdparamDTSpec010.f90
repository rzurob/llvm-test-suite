!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2006
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: function prefix and
!                               procedure declaration stmt.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real(8) :: data(n+1) = 0.0d0
    end type

    interface
        type (base(:)) function genBase (n, data)
        import
            allocatable genBase
            integer, intent(in) :: n
            real(8), intent(in) :: data(n)
        end function
    end interface
end module

program deferdparamDTSpec010
use m
    procedure(genBase) :: genBaseExternal

    interface
        subroutine transferAlloc (b1, proc, n, data)
        use m
            type(base(:)), allocatable, intent(out) :: b1
            procedure(genBase), optional :: proc
            integer, intent(in) :: n
            real(8), intent(in) :: data(n)
        end subroutine
    end interface

    type(base(:)), allocatable :: b1

    real(8) d1(20)

    d1 = (/(i*1.0d0, i=1,20)/)

    call transferAlloc (b1, genBaseExternal, 20, d1)

    write (*, 100) b1%data

    call transferAlloc (b1, n=20, data=d1)

    write (*, 100) b1%data

100 format (7f10.2)
end


subroutine transferAlloc (b1, proc, n, data)
use m
    type(base(:)), allocatable, intent(out) :: b1
    procedure(genBase), optional :: proc
    integer, intent(in) :: n
    real(8), intent(in) :: data(n)

    if (present(proc)) then
        allocate (b1, source=proc(n, data))
    else
        allocate (base(n-1) :: b1)

        b1%data = data
    end if
end subroutine


type (base(:)) function genBaseExternal (n, data)
use m, only : base
    allocatable genBaseExternal
    integer, intent(in) :: n
    real(8), intent(in) :: data(n)

    allocate (base(n) :: genBaseExternal)

    genBaseExternal%data(:n) = data
end function
