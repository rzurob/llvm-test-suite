! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (zero-size arrays on effective
!                               items)
!                               adaptation: exposed kind, lengths
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
    type A (kA_1,lA_1) ! kA_1,lA_1=4,2
       integer, kind :: kA_1
       integer, len :: lA_1
        integer(kA_1) id(lA_1)
    end type

    type B (lB_1) ! lB_1=20
       integer, len :: lB_1
        character(lB_1) :: name
    end type

    type base (kbase_1,lbase_1,lbase_2) ! kbase_1,lbase_1,lbase_2=4,2,20
       integer, kind :: kbase_1
       integer, len :: lbase_1,lbase_2
        type (A(kbase_1,lbase_1)) a1 ! tcx: (kbase_1,lbase_1)
        type (B(lbase_2)) b1 ! tcx: (lbase_2)
    end type
end module


program fdtio511akl
use m
    interface read(formatted)
        subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A(4,*)), intent(inout) :: dtv ! tcx: (4,*)
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
            class (A(4,*)), intent(in) :: dtv ! tcx: (4,*)
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

    type (base(4,2,20)) b1 ! tcx: (4,2,20)

    b1%a1 = A(4,2)((/10, 20/)) ! tcx: (4,2)

    print *, base(4,2,20)(a(4,2)((/1,2/)), b(20)('xlftest')) ! tcx: (4,2) ! tcx: (20) ! tcx: (4,2,20)

    write (1, *) base(4,2,20)(a(4,2)((/1,2/)), b(20)('xlftest team')) ! tcx: (4,2) ! tcx: (20) ! tcx: (4,2,20)

    rewind(1)

    read (1, *) b1

    if (any (b1%a1%id /= (/10, 20/))) error stop 101_4

    if (b1%b1%name /= 'xlftest team') error stop 2_4

    close (1, status='delete')
end


subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A(4,*)), intent(inout) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(i6)', iostat=iostat, iomsg=iomsg) dtv%id(2:1)
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
    class (A(4,*)), intent(in) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(i5)', iostat=iostat, iomsg=iomsg) dtv%id(2:1)
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
! type: A - added parameters (kA_1,lA_1) to invoke with (4,2) / declare with (4,*) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: base - added parameters (lbase_1) to invoke with (20) / declare with (*) - 3 changes
! type: A - added parameters (kA_1,kA_2) to invoke with (2,4) / declare with (2,4) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: A - added parameters (kA_1,kA_2) to invoke with (2,4) / declare with (2,4) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: A - added parameters (kA_1,kA_2) to invoke with (2,4) / declare with (2,4) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: A - added parameters (kA_1,kA_2) to invoke with (2,4) / declare with (2,4) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: A - added parameters (kA_1) to invoke with (4) / declare with (4) - 7 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: A - added parameters (kA_1,kA_2) to invoke with (2,4) / declare with (2,4) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: base - added parameters (kbase_1,kbase_2,lbase_1) to invoke with (4,2,20) / declare with (4,2,*) - 3 changes
! type: A - added parameters (kA_1,kA_2) to invoke with (2,4) / declare with (2,4) - 8 changes
! type: A - added parameters (kA_1,lA_2) to invoke with (4,2) / declare with (4,*) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 8 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,2,20) / declare with (4,*,*) - 3 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,2,20) / declare with (4,*,*) - 3 changes
! type: zort - added parameters (kzort_1,lzort_1,lzort_2,lzort_3,lzort_4,lzort_5,lzort_6) to invoke with (4,1,2,7,20,5,3) / declare with (4,*,*,*,*,*,*,*) - 0 changes
! type: A - added parameters (kA_1,lA_1) to invoke with (4,2) / declare with (4,*) - 8 changes
! type: B - added parameters (lB_1) to invoke with (20) / declare with (*) - 7 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,2,20) / declare with (4,*,*) - 3 changes