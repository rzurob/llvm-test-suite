!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio054a_1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (advance= specifier in DTIO
!                               has no effects on DTIO behavior)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class (*), allocatable :: data(:)
    end type

    character(100) globalBuffer

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio054a_1
use m
    class (base), allocatable :: b1

    integer stat
    character(200) err

    character(13), parameter :: testString = 'abc xyz 3 4 5'

    open (1, file='fdtio054a.data')

    allocate (b1)

    write (1, '(a)')  testString

    rewind 1

    write (globalBuffer, '(a)') 'char 11'

    !! read will fail as the advance='no' specified in DTIO
    read (1, *, iostat=stat, iomsg=err) b1  !<-- DTIO still non-advancing

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    !! verify data read in
    select type (x => b1%data)
        type is (character(*))
            if (len(x) /= 1) error stop 2_4

            if (size(x) /= 11) error stop 3_4

            do i = 1, 11
                if (x(i) /= testString(i:i)) error stop 4_4
            end do
        class default
            error stop 5_4
    end select

    close (1, status='delete')
end


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, globalBuffer
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(4) dataType
    integer(4) dataSize

    if (allocated (dtv%data)) deallocate (dtv%data)

    read (globalBuffer, *, iostat=iostat, iomsg=iomsg) dataType, dataSize

    if (iostat /= 0) return

    if (dataType == 'real') then
        allocate (real :: dtv%data(dataSize))
    else if (dataType == 'int') then
        allocate (integer :: dtv%data(dataSize))
    else
        allocate (character :: dtv%data(dataSize))  !<-- by default treat as string
    end if

    select type (x => dtv%data)
        type is (integer)
            read (unit, *, iostat=iostat, iomsg=iomsg) x
        type is (real)
            read (unit, *, iostat=iostat, iomsg=iomsg) x
        type is (character(*))
            do i = 1, size(x)

                !! specifying advance='no' will have no effect on DTIO
                read (unit, '(a)', iostat=iostat, iomsg=iomsg, advance='no') x(i)
                if (iostat /= 0) return
            end do
        class default
            error stop 10_4
    end select
end subroutine

