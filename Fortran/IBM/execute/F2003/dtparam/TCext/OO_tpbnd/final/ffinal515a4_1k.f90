! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal515a4_1k
!*
!*  DATE                       : 2007-11-11 (original: 02/14/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.tbnd)
!*
!*  DESCRIPTION                : final sub (finalization of temps created by
!                               structure constructor in write statement)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        real(kbase_1), allocatable :: data(:)

        contains

        final :: finalizeBase
    end type

    interface write (formatted)
        subroutine writeBaseFmt (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            integer, intent(in) :: unit
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            character(*),intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(inout) :: b ! tcx: (4)

        if (allocated (b%data)) then
            print *, 'deallocate data'

            deallocate (b%data)
        end if
    end subroutine
end module

program ffinal515a4_1k
use m
    integer stat1
    character(200) err

    write (*,*) base(4) (null()) ! tcx: (4)

    write (*, *) base(4) ((/2.2, 4.2, 3.1/)) ! tcx: (4)

    print *, 'end'
end


!! we only handle the list-directed write
subroutine writeBaseFmt (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    integer, intent(in) :: unit
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    character(*),intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        write (unit, '(5f10.2)', iostat=iostat, iomsg=iomsg) dtv%data
    else
        write (unit, *, iostat=iostat, iomsg=iomsg) 'data not allocated'
    end if

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
