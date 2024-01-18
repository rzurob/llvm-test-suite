! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/11/2006
!*
!*  DESCRIPTION                : miscellaneous (compilation hangs in xlfhot)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base20! (n)
        real(8) :: data(20+1) = 0.0d0
    end type

    type base19! (n)
        real(8) :: data(19+1) = 0.0d0
    end type

    interface
        type (base20) function genBase (n, data)
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

    interface transferAlloc
        subroutine transferAlloc (b1, proc, n, data)
        use m
            type(base20), allocatable, intent(out) :: b1
            procedure(genBase) :: proc
            integer, intent(in) :: n
            real(8), intent(in) :: data(n)
        end subroutine

        subroutine transferAlloc19 (b1, n, data)
        use m
            type(base19), allocatable, intent(out) :: b1
            integer, intent(in) :: n
            real(8), intent(in) :: data(n)
        end subroutine
    end interface

    type(base20), allocatable :: b1
    type(base19), allocatable :: b2

    real(8) d1(20)

    d1 = (/(i*1.0d0, i=1,20)/)

    call transferAlloc (b1, genBaseExternal, 20, d1)

    write (*, 100) b1%data

    call transferAlloc (b2, n=20, data=d1)

    write (*, 100) b2%data

100 format (7f10.2)
end


subroutine transferAlloc (b1, proc, n, data)
use m
    type(base20), allocatable, intent(out) :: b1
    procedure(genBase) :: proc
    integer, intent(in) :: n
    real(8), intent(in) :: data(n)

    if (n /=20) stop 20
    allocate (b1, source=proc(n, data))
end subroutine


subroutine transferAlloc19 (b1, n, data)
use m
    type(base19), allocatable, intent(out) :: b1
    integer, intent(in) :: n
    real(8), intent(in) :: data(n)

    if (n /=20) stop 19
    allocate (base19:: b1)

    b1%data = data
end subroutine

type (base20) function genBaseExternal (n, data)
use m, only : base20
    allocatable genBaseExternal
    integer, intent(in) :: n
    real(8), intent(in) :: data(n)

    allocate (base20 :: genBaseExternal)

    genBaseExternal%data(:n) = data
end function
