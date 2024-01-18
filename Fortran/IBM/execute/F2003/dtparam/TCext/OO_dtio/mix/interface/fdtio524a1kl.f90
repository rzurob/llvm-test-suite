! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio524a1kl
!*
!*  DATE                       : 2007-08-16 (original: 05/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (DTIO generics in local scope)
!                               adaptation: exposed kinds, len
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1,kbase_2) ! kbase_1,kbase_2=4,8
       integer, kind :: kbase_1,kbase_2
        private
        complex(kbase_2) :: data
        integer(kbase_1) id
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        private
        character(lchild_1) :: name
    end type

    private formattedWrite

    contains

    subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base(4,8)), intent(in) :: dtv ! tcx: (4,8)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        select type (dtv)
            type is (base(4,8)) ! tcx: (4,8)
                write (unit, *, iostat=iostat, iomsg=iomsg) dtv
            type is (child(4,8,*)) ! tcx: (4,8,*)
                write (unit, *, iostat=iostat, iomsg=iomsg) dtv
            class default
                error stop 11_4
        end select
    end subroutine

    subroutine setVal (b, data, id, name)
        class (base(4,8)), intent(out) :: b ! tcx: (4,8)
        complex(8), intent(in) :: data
        integer, intent(in) :: id
        character(*), intent(in) :: name

        select type (b)
            type is (base(4,8)) ! tcx: (4,8)
                b = base(4,8) (data, id) ! tcx: (4,8)
            type is (child(4,8,*)) ! tcx: (4,8,*)
                b = child(4,8,20) (data, id, name) ! tcx: (4,8,20)
            class default
                error stop 15_4
        end select
    end subroutine

    subroutine printBase (b)
        class (base(4,8)), intent(in) :: b ! tcx: (4,8)

        interface write(formatted)
            module procedure formattedWrite
        end interface

        print *, b
    end subroutine
end module

program fdtio524a1kl
use m
    class (base(4,8)), pointer :: b1(:) ! tcx: (4,8)

    allocate (child(4,8,20)::b1(2)) ! tcx: (4,8,20)

    call setVal (b1(1), (1.0_8, 2.0_8), 10, 'test xlf')

    call printBase (b1(1))
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2) to invoke with (4,8) / declare with (4,8) - 7 changes
! type: child - added parameters (lchild_1) to invoke with (4,8,20) / declare with (4,8,*) - 4 changes
