! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Dtv-type-spec is of derived type with
!                               default type parameter values; test type-bound
!                               generic dtio
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 8

        real(k), allocatable :: data(:)

        contains

        generic :: write(unformatted) => write8U
        procedure, private :: write8U => writeBase8U
    end type


    contains

    subroutine writeBase8U (b, unit, iostat, iomsg)
        class(base), intent(in) :: b
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (allocated(b%data)) then
            write(unit, iostat=iostat, iomsg=iomsg) b%data
        end if
    end subroutine
end module

program dtparamDefVal014
use m
    class(base), allocatable :: b1(:)

    logical(4), external :: precision_r8

    type A (n)
        integer, len :: n

        real(8) :: data(0:n)
    end type

    type (A(1)) :: a1
    type (A(2)) :: a2
    type (A(3)) :: a3
    type (A(4)) :: a4
    type (A(5)) :: a5
    type (A(6)) :: a6
    type (A(7)) :: a7
    type (A(8)) :: a8
    type (A(9)) :: a9
    type (A(10)) :: a10


    allocate (b1(10), source=(/(base((/(i*1.0d0, i=0,j)/)), j=1, 10)/))

    write(1) b1

    rewind 1

    read (1) a1, a2, a3, a4, a5, a6, a7, a8, a9, a10

    write (*, 100) a1
    write (*, 100) a2
    write (*, 100) a3
    write (*, 100) a4
    write (*, 100) a5
    write (*, 100) a6
    write (*, 100) a7
    write (*, 100) a8
    write (*, 100) a9
    write (*, 100) a10

100 format (6f10.2)
end
