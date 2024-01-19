! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/decimaledit/dtio/modeReset002.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that if multiple child write statements
!                               ocurr during one parent write, each statement
!                               will NOT affect the subsequent child write
!                               statements; test on internal file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type dataType(k1)    ! (4)
        integer, kind         :: k1
        real(k1), allocatable :: data

        contains

        procedure :: writeDatatypeFmtd
        generic :: write(formatted) => writeDatatypeFmtd
    end type

    type base(k2)    ! (4)
        integer, kind :: k2
        class(*), allocatable :: data
        complex(k2)      cx

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    recursive subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated(dtv%data)) error stop 10_4

        select type (x => dtv%data)
            class is (dataType(4))
                write (unit, '(DT)', iostat=iostat, iomsg=iomsg, &
                    decimal='Comma') x

            class is (base(4))
                write (unit, '(DT)', iostat=iostat, iomsg=iomsg) x

        end select

        if (iostat /= 0) return

        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%cx

    end subroutine

    subroutine writeDatatypeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(dataType(4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (.not. allocated(dtv%data)) error stop 20_4

        write (unit, '(g12.5)', iostat=iostat, iomsg=iomsg) &
            dtv%data
    end subroutine
end module

use m
    character(:), pointer :: c

    allocate (character(100) :: c)

    write (c, '(DT"DP")') base(4)(base(4)(dataType(4)(1.2e0), 2.0), 1.0)

    write (*,'(a)') c

    write (c, '("dc", DT"DP")') base(4)(base(4)(dataType(4)(1.2e0), 2.0), 1.0)

    write (*,'(a)') c
end
