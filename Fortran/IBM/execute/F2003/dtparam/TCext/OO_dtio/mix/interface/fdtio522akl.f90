! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio522akl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio522a by Jim Xia)
!*  DATE                       : 2007-08-16 (original: 02/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DTIO generics (list-directed write on array
!                               constructor containing poly-entities)
!                               adaptation: exposed kind, len
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

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name
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
use m, only: base, child
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    !! we only do the list-directed
    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    select type (dtv)
        type is (base(8)) ! tcx: (8)
            if (allocated (dtv%data)) then
                write (unit, '(5(1x, f15.3))', iostat=iostat, iomsg=iomsg) dtv%data
            else
                write (unit, '(a)', iostat=iostat, iomsg=iomsg) 'unallocated'
            end if
        class is (child(8,*)) ! tcx: (8,*)
            if (allocated (dtv%data)) then
                write (unit, '(5(1x, f15.3))', iostat=iostat, iomsg=iomsg)&
                    dtv%data
                if (iostat /= 0) return

                write (unit, '(2a)', iostat=iostat, iomsg=iomsg) &
                    '; name=', dtv%name
            else
                write (unit, '(2a)', iostat=iostat, iomsg=iomsg) &
                    'unallocated; name=', dtv%name
            end if
    end select
end subroutine

program fdtio522akl
use m
    class (base(8)), allocatable :: b1(:), b2, b3(:) ! tcx: (8)

    allocate (b1(2), source=(/child(8,20)((/1.7_8, 3.3_8/), 'abc'), & ! tcx: (8,20)
                child(8,20)((/3.6_8/), 'xyz')/)) ! tcx: (8,20)
    allocate (b2, source = child(8,20) (null(),'')) ! tcx: (8,20)
    allocate (b3(5), source= (/(child(8,20)((/i*1.1_8, i*2.2_8/), 'b3'), i = 1, 5)/)) ! tcx: (8,20)

    print *, (/b1/)
    print *, (/b2, b3/)
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 4 changes
! type: child - added parameters (lchild_1) to invoke with (8,20) / declare with (8,*) - 5 changes
