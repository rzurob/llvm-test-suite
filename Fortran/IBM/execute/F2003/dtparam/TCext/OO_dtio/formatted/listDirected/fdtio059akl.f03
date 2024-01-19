! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-20 (original: 01/13/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (effect of TR and TL during DTIO)
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
    type base (kb)
       integer, kind :: kb
        integer(kb), allocatable :: data(:)
    end type


    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8)), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


!! read in 3 integer data
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(8)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

!    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        if (size (dtv%data) /= 3) deallocate (dtv%data)
    end if

    if (.not. allocated (dtv%data)) allocate (dtv%data(3))

    !! read the data in reverse order
    read (unit, '(TR20,i10,TL20,i10,TL30,i10,TR20)', iostat=iostat, iomsg=iomsg) &
                        dtv%data
end subroutine


program fdtio059akl
use m
    class (base(8)), pointer :: b1(:)

    integer stat1
    character(200) err

    allocate (b1(2))

    allocate (b1(2)%data(3))

    write (1, '(6i10)') (i, i=1,12,2)

    rewind (1)

    !! test read a scalar
    read (1, *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if

    if (any (b1(1)%data /= (/5_8,3_8,1_8/))) error stop 4_4

    !! test read for an array
    rewind 1
    read (1, '(2DT)', iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    if (any (b1(1)%data /= (/5_8,3_8,1_8/))) error stop 5_4

    if (any (b1(2)%data /= (/11_8,9_8,7_8/))) error stop 6_4
end
