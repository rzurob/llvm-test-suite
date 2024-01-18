! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 11/10/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (extended types in the DTIO
!                               routines; use select type construct during the
!                               child data transfer; test arrays)
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child (lchild_1) ! lchild_1=20
       integer, len :: lchild_1
        character(lchild_1) :: name
        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b ! tcx: (4)

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*)), intent(in) :: b ! tcx: (4,*)

        print *, b%id, b%name
    end subroutine
end module

program fdtio508akl
use m
    class (base(4)), pointer :: b1(:), b2(:) ! tcx: (4)

    integer stat
    character(200) error

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    call openFile (1, 'fdtio508akl.data', 'unformatted')

    call writeData

    allocate (b1(2))
    allocate (child(4,20):: b2(2)) ! tcx: (4,20)

    rewind(1)

    error = 'no error'

    read (1, iostat=stat, iomsg=error) b1, b2

    if (stat /= 0) error stop 2_4

    call b1(1)%print
    call b1(2)%print

    call b2(1)%print
    call b2(2)%print

    close(1, status='delete')
end


subroutine openFile (i, name, form)
    integer(4), intent(in) :: i
    character(*), intent(in) :: name, form

    open (i, file=name, form=form)
end subroutine

subroutine writeData
use m
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer stat
    character(200) error

    class (base(4)), allocatable :: b1(:), b2(:) ! tcx: (4)

    allocate (b1(0:1), source=(/base(4)(10), base(4)(20)/)) ! tcx: (4) ! tcx: (4)
    allocate (b2(-1:0), source=(/child(4,20)(100, 'xlftest A'), child(4,20)(200, 'xlftest B') & ! tcx: (4,20) ! tcx: (4,20)
                /))


    write (1, iostat=stat, iomsg=error) b1, b2

    if (stat /= 0) error stop 101_4
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
        type is (base(4)) ! tcx: (4)
            write (unit, iostat=iostat, iomsg=iomsg) dtv
        type is (child(4,*)) ! tcx: (4,*)
            write (unit, iostat=iostat, iomsg=iomsg) dtv
        class default
            error stop 10_4
    end select
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select type (dtv)
        type is (base(4)) ! tcx: (4)
            read (unit, iostat=iostat, iomsg=iomsg) dtv

        type is (child(4,*)) ! tcx: (4,*)
            read (unit, iostat=iostat, iomsg=iomsg) dtv
        class default
            error stop 15_4
    end select
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 11 changes
! type: child - added parameters (lchild_1) to invoke with (4,20) / declare with (4,*) - 6 changes
