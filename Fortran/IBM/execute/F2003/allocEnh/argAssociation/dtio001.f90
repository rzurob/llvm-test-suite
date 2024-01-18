!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/05/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the intrinsic assignment for the allocatable
!                               components of derived type in the dtio routine
!                               (read for integer and real(8) types).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id
        real(8), allocatable :: data(:)

        contains

        procedure :: print => printBase
        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine printBase(b, unit)
        class(base), intent(in) :: b
        integer, intent(in) :: unit

        if (.not. allocated(b%id)) then
            write (unit, '(a)', advance='no') 'id is not allocated; '

        else
            write (unit, '(a, i10)', advance='no') 'id = ', b%id
        end if

        if (allocated(b%data)) then
            write (unit, '(a,2(1x, i7), 1x, a)', advance='no') '; bounds of data:',&
                    lbound(b%data,1), ubound(b%data,1), ', values are :'

            write (unit, '(1x, 5g20.12)') b%data
        else
            write (unit, *) 'data is not allocated'
        end if
    end subroutine

    subroutine readBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        integer id, lbound, ubound

        read (unit, *, iostat=iostat, iomsg=iomsg) id

        if (iostat /= 0) return

        dtv%id = id

        read (unit, *, iostat=iostat, iomsg=iomsg) lbound, ubound

        if (iostat /= 0) return

        call readDataArray (lbound, ubound)

        contains

        subroutine readDataArray (lb, ub)
            integer, intent(in) :: lb, ub

            double precision localData (lb:ub)

            read (unit, *, iostat=iostat, iomsg=iomsg) localData

            if (iostat /= 0) return

            dtv%data = localData
        end subroutine
    end subroutine
end module

program dtio001
use m
use iso_fortran_env
    type(base), allocatable :: b1(:)

    b1 = (/(base(null(), null()), base(i, (/(j, j=1,i)/)), i=1, 4)/)

    do i =1, 8
        write (1,*) i*100, 0, i, (j*1.1d0, j=1,i+1)
    end do

    rewind(1)

    read(1,*) b1

    do i = 1, 8
        call b1(i)%print(OUTPUT_UNIT)
    end do
end
