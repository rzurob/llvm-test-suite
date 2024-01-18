! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/decimaledit/dtio/dcmlChildWrite012.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/26/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the dtio write is used in a namelist
!                               write using decimal mode of comma.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(n1,k1)    ! (20,8)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: data(:)

        contains

        procedure :: writeDataTypeFmtd
        generic :: write(formatted) => writeDataTypeFmtd
    end type

    type base(n2,k2,k3)    ! (20,4,8)
        integer, kind                      :: k2,k3
        integer, len                       :: n2
        complex(k2), pointer               :: cx => null()
        class(dataType(:,k3)), allocatable :: data(:)

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    !! controls the precision in output if parent is list-directed
    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,4,8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'NAMELIST') error stop 10_4

        if (associated(dtv%cx)) then
            write (unit, 999, iostat=iostat, iomsg=iomsg) dtv%cx

        else
            iostat = 100
            iomsg = 'dtv%cx not allocated in base type'
            return
        end if

        if (iostat /= 0) return

        if (allocated(dtv%data)) then
            do i = lbound(dtv%data, 1), ubound(dtv%data, 1)
                write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data(i)

                if (iostat /= 0) return
            end do
        end if


999     format (" (", e20.10, " ; ", e20.10, ")")
    end subroutine

    !! controls the precision in output if parent is list-directed
    subroutine writeDataTypeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(dataType(*,8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') error stop 20_4

        if (allocated(dtv%data)) then
            do i = lbound(dtv%data,1), ubound(dtv%data,1)
                write (unit, '(e20.10)', iostat=iostat, iomsg=iomsg) dtv%data(i)

                if (iostat /= 0) return
            end do
        end if
    end subroutine
end module

program dcmlChildWrite012
use m
    real, allocatable :: r1(:)
    complex, pointer :: cx1(:)
    class(base(:,4,8)), allocatable :: b1(:)

    namelist /nml1/ r1, b1, cx1

    allocate (r1(10), cx1(10))
    allocate (base(20,4,8) :: b1(10))

    r1 = (/(i, i=0,9)/)

    cx1 = (/(i, i= -10,-1)/)

    do i = 1, 10
        allocate (b1(i)%data(i), source=(/(dataType(20,8)((/(k*1.0d0, k=1,j)/)), &
                j=1,i)/))


        allocate(b1(i)%cx, source=cmplx(i, 2*i))
    end do

    open (1, file='dcmlChildWrite012.data')

    write (1, nml1, decimal='comma')

    close (1)
end
