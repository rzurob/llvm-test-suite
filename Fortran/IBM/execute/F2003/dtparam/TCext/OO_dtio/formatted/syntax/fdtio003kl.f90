! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio003kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio003 by Jim Xia)
!*  DATE                       : 2007-07-23 (original: 11/19/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (test the dtv; functional test
!                               that DTIO defined for base type will be used for
!                               the data declared of child type)
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
    type base (lb) ! lb=5
       integer, len :: lb
        character(lb), allocatable :: data
    end type

    type, extends(base) :: child (kc) ! kc=4
       integer, kind :: kc
        integer(kc) :: i1
    end type

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(*)), intent(in) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio003kl
use m
    class (child(:,4)), allocatable :: c1 ! tcx: (:,4)
    class (base(:)), allocatable :: b1 ! tcx: (:)
    type (child(5,4)) c2 ! tcx: (5,4)

    allocate (c1, source=child(5,4)('abcde', 100)) ! tcx: (5,4)
    allocate (b1, source=child(5,4)('ABCDE', 200)) ! tcx: (5,4)

    allocate (c2%data, source='XYZ  ')
    c2%i1 = 300

    print *, c1

    write (*,*) b1

    print *, c2
end

subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) &
            write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (5) / declare with (*) - 3 changes
! type: child - added parameters (kb) to invoke with (4) / declare with (4) - 4 changes
! type: child - added parameters (kc) to invoke with (5,4) / declare with (*,4) - 4 changes
