! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio511kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio511 by Jim Xia)
!*  DATE                       : 2007-08-15 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : DTIO on generics (for components of a derived
!                               type DTIO is invoked based on the component
!                               order)
!                               adaptation: exposed kind, length
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
    type A (kA_1) ! kA_1=4
       integer, kind :: kA_1
        integer(kA_1) id
    end type

    type B (lB_1) ! lB_1=20
       integer, len :: lB_1
        character(lB_1) :: name
    end type

    type base (kbase_1,lbase_1) ! kbase_1,lbase_1=4,20
       integer, kind :: kbase_1
       integer, len :: lbase_1
        type (A(kbase_1)) a1 ! tcx: (kbase_1)
        type (B(lbase_1)) b1 ! tcx: (lbase_1)
    end type
end module


program fdtio511kl
use m
    interface read(formatted)
        subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedReadB (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (B(*)), intent(inout) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(formatted)
        subroutine formattedWriteA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteB (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (B(*)), intent(in) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base(4,20)) b1 ! tcx: (4,20)

    print *,  base(4,20)(A(4)(100), B(20)('xlftest')) ! tcx: (4) ! tcx: (20) ! tcx: (4,20)

    write (1, *) base(4,20) (b1=B(20)('xlftest 101'), a1 = A(4)(200)) ! tcx: (4) ! tcx: (20) ! tcx: (4,20)

    close (1)

    read (1, *) b1

    print *, b1

    close(1, status='delete')
end


subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id
end subroutine


subroutine formattedReadB (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (B(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(a20)', iostat=iostat, iomsg=iomsg) dtv%name
end subroutine

subroutine formattedWriteA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(i5)', iostat=iostat, iomsg=iomsg) dtv%id
end subroutine


subroutine formattedWriteB (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (B(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(a20)', iostat=iostat, iomsg=iomsg)  dtv%name
end subroutine


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA_1) to invoke with (4) / declare with (4) - 7 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (4,20) / declare with (4,*) - 3 changes
