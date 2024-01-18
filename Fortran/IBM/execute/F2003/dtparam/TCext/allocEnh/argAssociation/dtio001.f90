! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/allocEnh/argAssociation/dtio001.f
! opt variations: -ql

! SCCS ID Information
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
    type base(k1,k2)    ! (4,8)
        integer, kind            :: k1,k2
        integer(k1), allocatable :: id
        real(k2), allocatable    :: data(:)

        contains

        procedure :: print => printBase
        procedure :: readBaseFmtd
        generic :: read(formatted) => readBaseFmtd
    end type

    contains

    subroutine printBase(b, unit)
        class(base(4,8)), intent(in) :: b
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
        class(base(4,8)), intent(inout) :: dtv
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
    type(base(4,8)), allocatable :: b1(:)

    b1 = (/(base(4,8)(null(), null()), base(4,8)(i, (/(j, j=1,i)/)), i=1, 4)/)

    do i =1, 8
        write (1,*) i*100, 0, i, (j*1.1d0, j=1,i+1)
    end do

    rewind(1)

    read(1,*) b1

    do i = 1, 8
        call b1(i)%print(OUTPUT_UNIT)
    end do
end
