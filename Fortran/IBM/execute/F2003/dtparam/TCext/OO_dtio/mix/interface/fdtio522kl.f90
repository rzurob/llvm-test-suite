! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio522kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio522 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 02/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (list-directed write on array
!                               constructor)
!                               adaptation: exposed kind
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        real(kbase_1), allocatable :: data(:)
    end type



    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8)), intent(in) :: dtv ! tcx: (8)
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
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! we only do the list-directed
    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        write (unit, '(5(1x, f15.3))', iostat=iostat, iomsg=iomsg) dtv%data
    else
        write (unit, '(a)', iostat=iostat, iomsg=iomsg) 'unallocated'
    end if
end subroutine

program fdtio522kl
use m
    type (base(8)), allocatable :: b1(:) ! tcx: (8)
    class (base(8)), allocatable :: b2, b3(:) ! tcx: (8)

    allocate (b1(2), source=(/base(8)((/1.7_8, 3.3_8/)), base(8)((/3.6_8/))/)) ! tcx: (8) ! tcx: (8)
    allocate (b2, source = base(8) (null())) ! tcx: (8)
    allocate (b3(5), source= (/(base(8)((/i*1.1_8, i*2.2_8/)), i = 1, 5)/)) ! tcx: (8)

    print *, (/b1, base(8)((/10.3_8/))/) ! tcx: (8)

    print *, (/b2, b3/)

    print *, (/b2, base(8)(null())/) ! tcx: (8)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 10 changes
