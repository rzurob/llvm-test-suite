! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio523kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio523 by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 03/18/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (DTIO and associate construct)
!*                               adaptation: exposed kind
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        real(kbase_1), private, allocatable :: r1(:)
    end type

    interface write(formatted)
        module procedure formattedWrite
    end interface

    interface base
        module procedure createBaseObj
    end interface

    contains

    !! only deal with list-directed write
    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base(4)), intent(in) :: dtv ! tcx: (4)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        if (allocated (dtv%r1)) then
            write (unit, '(/,8f10.2)', iostat=iostat, iomsg=iomsg) dtv%r1
        end if
    end subroutine

    type (base(4)) function createBaseObj (r1) ! tcx: (4)
        real, intent(in) :: r1(:)

        allocate (createBaseObj%r1(size(r1)), source = r1)
    end function
end module

program fdtio523kl
use m
    associate (x => (/base((/1.3, 4.2, 3.1/)), base((/3.3, 4.4, 6.1, 12.3/))/)) ! these are really invocations of createBaseObj, not the structure constructor
        print *, x
    end associate
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
