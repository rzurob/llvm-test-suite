!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio059a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/17/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (TL1 used in formatted read for
!                               peeking operation; read in an array of unknown
!                               size)
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

    !! '|' will be the marker to terminate the read
    do while (.true.)
        !! peek next string to see if eor marker there
        read (unit, *, iostat=iostat, iomsg=iomsg) c1

        if (iostat /= 0) return

        if (c1(1:1) == '|') exit   !! get out the loop if hit the eor marker


        iTotal = iTotal + 1

        if (iTotal >= maxDataSize) exit

        !! assign this string to a data element
        read (c1, *, iostat=iostat, iomsg=iomsg) staticArray (iTotal)
    end do

    if (iTotal == 0) return     !! nothing to do

    allocate (dtv%data(iTotal), source=staticArray (1:iTotal))

end subroutine


program fdtio059a1
use m
    type (base) b1
    integer stat1
    character(200) err

    logical precision_r8

    write (1, '(5g15.2,1x, a1)') (i*1.5, i=1,5), '|'

    rewind 1

    read (1, *, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    if (.not. associated (b1%data)) error stop 2_4

    if (size (b1%data) /= 5) error stop 3_4

    do i = 1, 5
        if (.not. precision_r8 (b1%data(i), real(i*1.5, 8))) error stop 4_4
    end do
end
