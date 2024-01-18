!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/15/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Dtv-type-spec is of derived type with
!                               default type parameter values; test type-bound
!                               generic dtio; similar test case to
!                               dtparamDefVal014.f except the read part is
!                               handled by a recursive subroutine call.
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

program dtparamDefVal015
use m
    class(base), allocatable :: b1(:)

    logical(4), external :: precision_r8

    allocate (b1(10), source=(/(base((/(i*1.0d0, i=0,j)/)), j=1, 10)/))

    do i = 1, size(b1)
        write(1) b1(i)
    end do

    rewind 1

    call readNwriteData (1, 10)
end

recursive subroutine readNwriteData (i, total)
    integer, intent(in) :: i, total

!    type A (n)
!        integer, len :: n
!
!        real(8) :: data(0:n)
!    end type
!
!    type(A(i)) a1

    real(8) local_array(0:i)

    if ((i <= 0) .or. (i > total)) return

!    read (1) a1
    read (1) local_array

!    write (*, 100) a1
    write (*, 100) local_array

    call readNwriteData (i+1, total)

100 format (6f10.2)
end subroutine
