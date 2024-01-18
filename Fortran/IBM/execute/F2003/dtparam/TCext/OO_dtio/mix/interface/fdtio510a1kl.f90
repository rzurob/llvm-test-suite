! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio510a1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio510a1 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 11/17/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO on generics (a DTIO for a derived type
!                               calls another DTIO for another derived type)
!                               adaptation: exposed kind
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: i
    end type

    type dataType (kdataType_1) ! kdataType_1=4
       integer, kind :: kdataType_1
        class (base(kdataType_1)), allocatable :: data ! tcx: (kdataType_1)
    end type

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedWriteData (dtv, unit, iostat, iomsg)
        import dataType
            class (dataType(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m, only: base
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%i)) write (unit, iostat=iostat, iomsg=iomsg) dtv%i, 'a'
end subroutine

subroutine unformattedWriteData (dtv, unit, iostat, iomsg)
use m, only: dataType, write(unformatted)
    class (dataType(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) write (unit, iostat=iostat, iomsg=iomsg) dtv%data, 'A'
end subroutine

program fdtio510a1kl
use m
    class (dataType(4)), allocatable :: d1 ! tcx: (4)

    integer stat, i1, i2
    character (200) err
    character(2) c1
    character c2

    allocate (d1)
    allocate (d1%data, source=base(4)(100_4)) ! tcx: (4)

    open (1, form='unformatted', file='fdtio510a1kl.data')

    write (1, iostat=stat, iomsg=err) d1, base(4) (200) ! tcx: (4)

    if (stat /= 0) then
        print *, stat, err
        error stop 101_4
    end if

    rewind 1

    read (1) i1, c1, i2, c2

    if ((i1 /= 100) .or. (i2 /= 200)) error stop 2_4

    if ((c1 /= 'aA') .or. (c2 /= 'a')) error stop 3_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: dataType - added parameters (kdataType_1) to invoke with (4) / declare with (4) - 3 changes
