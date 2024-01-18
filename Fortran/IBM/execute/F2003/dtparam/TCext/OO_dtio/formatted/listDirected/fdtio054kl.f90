! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio054kl
!*
!*  DATE                       : 2007-06-19 (original: 11/30/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (a basic test that IO on
!                               internal file is allowed during DTIO)
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
        integer(kb), allocatable :: i1
    end type

    character (200) :: globalBuffer

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, globalBuffer
    class (base(4)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (.not. allocated (dtv%i1)) allocate (dtv%i1)

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%i1

    if (iostat /= 0) return

    write (globalBuffer, '(a,i5)', iostat=iostat, iomsg=iomsg) 'base type', dtv%i1
end subroutine

program fdtio054
use m
    type (base(4)) :: b1

    integer stat
    character(200) :: err

    write (1, *) 100

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    !! verify that globalBuffer is filled with values
    if (globalBuffer /= 'base type  100') error stop 2_4

    if (.not. allocated (b1%i1)) error stop 3_4

    if (b1%i1 /= 100) error stop 4_4
end
