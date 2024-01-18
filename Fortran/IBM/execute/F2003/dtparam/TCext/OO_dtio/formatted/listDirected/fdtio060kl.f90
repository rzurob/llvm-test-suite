! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio060kl
!*
!*  DATE                       : 2007-06-20 (original: unknown)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kb)
       integer, kind :: kb
        integer(kb), pointer :: data(:)
    end type


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
use m, only: base
    class (base(8)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    if (associated (dtv%data)) &
            write (unit, '(3i10, TR5)', iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


program fdtio060kl
use m
    class (base(8)), allocatable :: b1(:)

    integer stat1
    character(200) err

    err = 'no err'
    allocate (b1(2))

    allocate (b1(1)%data(4), source=(/10_8, 11_8, 12_8, 13_8/))

    write (1, *, iostat=stat1, iomsg=err) b1(1)

    if (stat1 /= 0) error stop 1_4
end
