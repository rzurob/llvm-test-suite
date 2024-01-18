! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio512a1kl
!*
!*  DATE                       : 2007-08-16 (original: 05/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (DTIO on function results; use
!                               rank-two non-poly function results)
!                               adaptation: exposed kind, length
!*
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


    interface makeData
        type (base(8)) function produceBaseArray (id, bShape) ! tcx: (8)
            import base
                integer(8), intent(in) :: id
                class (base(8)), intent(in) :: bShape(:,:) ! tcx: (8)
                dimension produceBaseArray(size(bShape,1),size(bShape,2))
        end function

        type (child(8,20)) function produceChildArray (id, name, bShape) ! tcx: (8,20)
        import child
            integer(8), intent(in) :: id
            character(*), intent(in) :: name
            class (child(8,*)), intent(in) :: bShape(:,:) ! tcx: (8,*)
            dimension produceChildArray (size(bShape,1),size(bShape,2))
        end function
    end interface

end module

program fdtio512a1kl
use m
    print *, makeData (10_8, reshape ((/(base(8)(i), i=1,4)/), (/2,2/))) ! tcx: (8)

    print *, makeData (20_8, 'test',  reshape ((/(child(8,20)(null(), ''), i=1,4)/), & ! tcx: (8,20)
                    (/2,2/)))
end


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child
    class (base(8)), intent(in) :: dtv ! tcx: (8)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%id)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id

        if (iostat /= 0) return
    end if

    select type (dtv)
        type is (base(8)) ! tcx: (8)
        type is (child(8,*)) ! tcx: (8,*)
            write (unit, *, iostat=iostat, iomsg=iomsg) ', ', dtv%name
        class default
            error stop 10_4
    end select
end subroutine


type (base(8)) function produceBaseArray (id, bShape) ! tcx: (8)
use m, only: base
    integer(8), intent(in) :: id
    class (base(8)), intent(in) :: bShape(:,:) ! tcx: (8)
    dimension produceBaseArray(size(bShape,1),size(bShape,2))

    k = 0

    do i = 1, size (produceBaseArray, 2)
        do j = 1, size (produceBaseArray, 1)
            allocate (produceBaseArray (j, i)%id, source = id + k)
            k = k + 1
        end do
    end do
end function


type (child(8,20)) function produceChildArray (id, name, bShape) ! tcx: (8,20)
use m, only: child
    integer(8), intent(in) :: id
    character(*), intent(in) :: name
    class (child(8,*)), intent(in) :: bShape(:,:) ! tcx: (8,*)
    dimension produceChildArray (size(bShape,1),size(bShape,2))

    k = 0

    do i = 1, size (produceChildArray, 2)
        do j = 1, size (produceChildArray, 1)
            produceChildArray (j, i) = child(8,20)(id+k, name) ! tcx: (8,20)

            k = k + 1
        end do
    end do
end function


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (8) / declare with (8) - 8 changes
! type: child - added parameters (lchild_1) to invoke with (8,20) / declare with (8,*) - 7 changes
