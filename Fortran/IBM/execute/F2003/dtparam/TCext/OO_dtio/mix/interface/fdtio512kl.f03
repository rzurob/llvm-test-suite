! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 12/03/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (function return result as the
!                               list item; polymorphic return)
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
    type base (kbase_1) ! kbase_1=8
       integer, kind :: kbase_1
        integer(kbase_1), allocatable :: id
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

    contains

    class (base(8)) function produceBasePtr (id, name) ! tcx: (8)
        pointer produceBasePtr
        integer(8), intent(in) :: id
        character(*), optional, intent(in) :: name

        if (present (name)) then
            allocate (produceBasePtr, source=child(8,20)(id, name)) ! tcx: (8,20)
        else
            allocate (produceBasePtr, source=base(8)(id)) ! tcx: (8)
        end if
    end function
end module

program fdtio512kl
use m

    print *, produceBasePtr (100_8)
    print *, produceBasePtr (200_8, 'xlftest team')
end


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%id)) write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

    if (iostat /= 0) return

    select type (dtv)
        type is (base(8)) ! tcx: (8)

        type is (child(8,*)) ! tcx: (8,*)
            write (unit, *, iostat=iostat, iomsg=iomsg) ', ', dtv%name
        class default
            error stop 10_4
    end select
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 5 changes
! type: child - added parameters (lchild_1) to invoke with (8,20) / declare with (8,*) - 2 changes
