!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio515a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/06/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (effects of null values in the
!                               input record)
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
    type base1
        integer(8), allocatable :: data
    end type

    type base2
        real(8), allocatable :: data
    end type


    interface read(formatted)
        subroutine formattedRead1 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base1
            class (base1), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedRead2 (dtv, unit, iotype, v_list, iostat, iomsg)
        import base2
            class (base2), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedRead1 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base1
    class (base1), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=-1_8)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


subroutine formattedRead2 (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base2
    class (base2), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) deallocate (dtv%data)

    allocate (dtv%data, source=-1.e0_8)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio515a
use m
    class (base1), allocatable :: b1(:)
    class (base2), allocatable :: b2 (:)

    logical precision_r8

    integer stat1
    character(200) err

    allocate (b1(2), b2(2))

    open (3, file='fdtio515a.data')

    write (3, *) '10,, 1*,10.35, 1.24'   !<-- this has 5 records

    rewind (3)

    read (3, *, iostat=stat1, iomsg=err) b1, b2

    !! verify data

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    if ((.not. allocated (b1(1)%data)) .or. (.not. allocated (b1(2)%data))) &
                error stop 2_4

    if ((.not. allocated (b2(1)%data)) .or. (.not. allocated (b2(2)%data))) &
                error stop 3_4

    if ((b1(1)%data /= 10) .or. (b1(2)%data /= -1)) error stop 4_4


    if ((.not. precision_r8 (b2(1)%data, -1.e0_8)) .or. (.not. precision_r8 &
        (b2(2)%data, 10.35e0_8))) &
                error stop 5_4


    close (3, status = 'delete')
end
