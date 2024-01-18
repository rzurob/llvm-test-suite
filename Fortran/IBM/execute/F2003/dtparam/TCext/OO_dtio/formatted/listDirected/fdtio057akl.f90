! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio057akl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio057a by Jim Xia)
!*  DATE                       : 2007-06-20 (original: 01/27/2005)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (FLUSH has no effect on file
!                               position; test during DTIO)
!*  KEYWORD(S)                 :
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kb)
       integer, kind :: kb
        integer(kb), allocatable :: data(:)
    end type

    integer pos(2)

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8)), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, pos
    class (base(8)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    inquire (unit, pos=pos(1), iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) return

    flush (unit, iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) return

    inquire (unit, pos=pos(2), iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) return

    !! verify that flush stmt does not cause file reposition
    if (pos(1) /= pos(2)) error stop 20_4


    if (allocated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return
    end if

    inquire (unit, pos=pos(1), iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) return

    !! flush statement does not position file
    flush (unit, iostat=iostat, iomsg=iomsg)

    inquire (unit, pos=pos(2), iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) return

    if (pos(1) /= pos(2)) error stop 21_4
end subroutine

program fdtio057akl
use m
    class (base(8)), allocatable :: b1, b2(:)

    integer(8) i1(8)

    integer stat
    character(200) err

    allocate (b1, source=base(8)((/-1, -2/)))
    allocate (b2(0:1))

    allocate (b2(0)%data(1), source=100_8)
    allocate (b2(1)%data(0:1), source=(/10_8, 20_8/))

    open (10, file='fdtio057.data', access='stream', form='formatted', &
                status='new')

    write (10, *, pos=15, iostat=stat, iomsg=err) base(8) ((/2,3,1/))

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    if (any (pos < 21)) error stop 2_4

    write (10, *, pos=25) b1

    write (10, *, pos=1) b2

    read (10, *, pos=1) i1(1:3)    !<-- 100, 10, 20
    read (10, *, pos=15) i1(4:6)   !<-- 2, 3, 1
    read (10, *, pos=25) i1(7:8)   !<-- -1, -2

    if (any (i1 /= (/100, 10, 20, 2, 3, 1, -1, -2/))) error stop 8_4

    close (10, status='delete')
end
