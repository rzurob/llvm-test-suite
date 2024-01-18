! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 05/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (test the runtime error that
!                               raised as user tries to do DTIO on types that
!                               requires DT edit descriptor which is NOT
!                               supplied)
!                               adaptation: exposed kind, len
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) id
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1), private :: name
    end type


    interface read(formatted)
        module procedure formattedRead
    end interface

    contains

    !! does NOT take read other than list-directed
    subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        class (base(4)), intent(inout) :: dtv ! tcx: (4)
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') stop 11
    end subroutine

    subroutine writeB (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        select type (b)
            type is (base(4)) ! tcx: (4)
                print *, b%id
            type is (child(4,*)) ! tcx: (4,*)
                print *, b%id, b%name
        end select
    end subroutine
end module

program fdtio519b1kl
use m
    class (child(4,:)), allocatable :: b1(:,:) ! tcx: (4,:)

    allocate (child(4,20):: b1(2,2)) ! tcx: (4,20)

    open (1, file='fdtio519b1kl.in')

    write (1, '(i5,a20)') 100, 'xlftest 01'

    rewind 1

    read (1, '(i5,a20)') b1(1,1)

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 3 changes
