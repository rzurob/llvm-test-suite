! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/05/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the decimal edit mode is also set
!                               during DTIO if the file is opened for.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id
        real(8), pointer :: data
        logical flag

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    type, extends(base) :: child
        class(*), allocatable :: x

        contains

        procedure :: writeBaseFmtd => writeChildFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. associated(dtv%data)) then
            iostat = 1000
            return
        end if

        write (unit, '(i5,1x, d20.12, 1x, l5)', iostat=iostat, iomsg=iomsg) &
            dtv%id, dtv%data, dtv%flag
    end subroutine

    subroutine writeChildFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(child), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (unit, '(DT)', decimal='Point', iostat=iostat, iomsg=iomsg) &
            dtv%base

        if (iostat /= 0) return

        select type (x => dtv%x)
            type is (real(8))
                write (unit, '(e20.12)') x

            class default
                iostat = 2000
                return
        end select
    end subroutine
end module

program dcmlChildWrite004a
use m
    class (base), allocatable :: b1, b2(:)

    double precision, target :: d1(10)

    character(:), allocatable :: c(:,:)

    allocate (b1, source=base(1, null(), .true.))

    allocate (b1%data, source= -1.2d0)

    allocate (b2(size(d1)), source=(/(child(i*10, d1(i), mod(i,3) == 0, i*1.0d1),&
            i = 1, 10)/))


    d1 = (/(i*11, i=1,10)/)

    allocate(character(100) :: c(2,12))

    c(:,:) = ''

    write (c(1,:), '(DT)', decimal='comma') b1, b2

    write (c(2,1), '(i5,1x, d20.12, 1x, l5)', decimal='comma') 1, -1.2d0, .true.

    do i = 1, 10
        write (c(2, i+1), '(dp, i5,1x, d20.12, 1x, l5, dc, e20.12)') i*10, &
                d1(i), mod(i,3) == 0, i*1.0d1
    end do

    do i = 1, 10
        if (c(1,i) /= c(2,i)) call zzrc(int(i, 4))
    end do
end
