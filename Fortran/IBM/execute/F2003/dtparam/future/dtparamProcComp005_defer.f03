! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/23/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: componnets)
!                               Case: Test the NOPASS attribute; function return
!                               is allocatable array.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        procedure(genBaseAlloc), pointer, nopass :: makeArray => null()
    end type

    abstract interface
        function genBaseAlloc (x, n)
        import
            class(base(:)), allocatable, dimension(:) :: genBaseAlloc
            class(*), intent(in) :: x(:)
            integer, intent(in) :: n
        end function
    end interface

    type, extends(base) :: realArray
        real, dimension(n) :: data
    end type

    contains

    subroutine printRealArray (ra)
        class(base(:)), allocatable, intent(in) :: ra(:)

        if (.not. allocated(ra)) then
            print *, 'unallocated'
        else
            select type (ra)
                type is (base(*))
                    error stop 1_4

                type is (realArray(*))
                    do i = 1, size(ra)
                        write (*, '(5f10.2)') ra(i)%data
                    end do

                class default
                    error stop 2_4
            end select
        end if
    end subroutine
end module

program dtparamProcComp005
use m
    procedure(genBaseAlloc) genRealArray

    class(base(:)), pointer :: b1

    allocate (b1, source=realArray(10)(genRealArray, 1))

    call printRealArray (b1%makeArray((/1,2,3,4,5/), 10))

    call printRealArray (b1%makeArray((/(i*1.0, i=1,8)/), 3))

    call printRealArray (b1%makeArray((/cmplx(1.0)/), 10))

    call printRealArray (b1%makeArray((/(i*1.2, i=8,6,-1)/), 4))
end

function genRealArray (x, n)
use m, only: base, realArray
    class(base(:)), allocatable, dimension(:) :: genRealArray
    class(*), intent(in) :: x(:)
    integer, intent(in) :: n

    select type (x)
        type is (real)
            allocate (genRealArray(n), source=realArray(size(x))(null(), x))
    end select
end function
