! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio524kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio524 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 05/05/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generic (DTIO generic declared inside the
!                               module becomes available to other module
!                               procedures)
!                               adaptation: exposed kind
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        real(kbase_1), allocatable :: data(:)

        contains

        procedure :: print => printBase
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

    contains

    subroutine printBase (b)
        class(base(8)), intent(in) :: b ! tcx: (8)

        print *, b
    end subroutine
end module

program fdtio524kl
use m
    class (base(8)), allocatable :: b1 ! tcx: (8)

    allocate (b1)

    allocate (b1%data(0:2), source=(/1.1_8, 2.2_8, 3.3_8/))

    call b1%print
end

!! this routine only deals with list-directed output
subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size(v_list) /= 0) error stop 10_4

    if (allocated (dtv%data)) then
        do i = lbound(dtv%data,1), ubound(dtv%data,1)
            write (unit, '(f12.3,1x)', iostat=iostat, iomsg=iomsg) dtv%data(i)
        end do
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 4 changes
