! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/18/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (formatted read for an array of
!                               unknown size; record in multiple lines)
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
        real(8), pointer :: data (:)
    end type

    integer, parameter :: maxDataSize = 10000

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


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, maxDataSize
use iso_fortran_env
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(15) c1
    real(8), save :: staticArray (maxDataSize)

    !! only run the list-directed read
    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    iTotal = 0

    !! '/' will be the marker to terminate the read
    do while (.true.)
        !! peek next character 1st to see if eor marker there
        read (unit, '(a1, TL1)', iostat=iostat, iomsg=iomsg) c1(1:1)

        if (iostat == IOSTAT_EOR)  then !! end of line encountered
            read (unit, '(/)') !! move to next line
            cycle
        end if

        if (iostat /= 0) return

        if (c1(1:1) == '/') exit   !! get out the loop if hit the eor marker

        read (unit, '(a15)', iostat=iostat, iomsg=iomsg) c1

        if (iostat == IOSTAT_EOR)  then !! end of line encountered
            read (unit, '(/)') !! move to next line
            cycle
        end if

        if (iostat /= 0) return

        iTotal = iTotal + 1

        if (iTotal >= maxDataSize) exit

        !! assign this string to a data element
        read (c1, *, iostat=iostat, iomsg=iomsg) staticArray (iTotal)

        if (iostat /= 0) return
    end do

    if (iTotal == 0) return     !! nothing to do

    allocate (dtv%data(iTotal), source=staticArray (1:iTotal))

end subroutine


program fdtio059a2
use m
    type (base) b1
    class (base), allocatable :: b2

    integer stat1
    character(200) err

    logical precision_r8

    allocate (b2)

    write (1, '(5g15.2, 8x, " ")') (i*1.5, i=1,5)
    write (1, '(3g15.2)') (i*1.5, i=-3,-1)
    write (1, '(2g15.2)') (i*0.5, i=9, 10)
    write (1,'(a1)') '/'


    write (1, '(3g15.2, 8x, " ")') (i*2.5, i=-3,-1)
    write (1, '(2g15.2)') (i*0.5, i=9, 10)
    write (1, '(5g15.2)') (i*1.5, i=1,5)
    write (1,'(g15.2,a1)') 1.4, '/'


    rewind 1

    read (1, *, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    if (.not. associated (b1%data)) error stop 2_4


    read (1, *, iostat=stat1, iomsg=err) b2

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 3_4
    end if

    write (*, '(10f10.2)') b1%data
    write (*, '(11f10.2)') b2%data

    close (1, status='delete')
end