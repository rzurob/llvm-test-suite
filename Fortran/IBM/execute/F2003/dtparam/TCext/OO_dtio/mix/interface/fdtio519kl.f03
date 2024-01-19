! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-16 (original: 01/14/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (DTIO for derived types with
!                               private component)
!                               adaptation: exposed kind, len
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
    type base (kbase_1,lbase_1) ! kbase_1,lbase_1=8,2
       integer, kind :: kbase_1
       integer, len :: lbase_1
        real(kbase_1), private :: data(lbase_1)

        contains

        procedure :: print => printBase
        procedure :: setVal => updateBase
        procedure :: getVal => getBaseVal
    end type


    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(8,*)), intent(in) :: dtv ! tcx: (8,*)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface


    contains

    subroutine updateBase (b, val)
        class (base(8,*)), intent(inout) :: b ! tcx: (8,*)
        real(8), intent(in) :: val(2)

        b%data = val
    end subroutine

    real(8) function getBaseVal (b)
        class (base(8,*)), intent(in) :: b ! tcx: (8,*)
        dimension getBaseVal(2)

        getBaseVal = b%data
    end function


    subroutine printBase (b, unit, iostat, iomsg)
        class (base(8,*)), intent(in) :: b ! tcx: (8,*)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write (unit, '(10x, 2g10.2,/)', iostat=iostat, iomsg=iomsg) b%getVal()
    end subroutine
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
    class (base(8,*)), intent(in) :: dtv ! tcx: (8,*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (iotype /= 'LISTDIRECTED') return

    if (size (v_list) /= 0) error stop 10_4

    call dtv%print(unit, iostat, iomsg)
end subroutine
program fdtio519kl
use m
    class (base(8,:)), allocatable :: b1(:) ! tcx: (8,:)

    integer stat1
    character(200) err

    allocate (base(8,2):: b1(2)) ! tcx: base(8,2)


    call b1(1)%setVal ((/1.3_8, 2.1_8/))
    call b1(2)%setVal ((/-2.4_8, -3.2_8/))

    !! test scalars
    print *, b1(1)

    !! test arrays
    print *, b1
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,kbase_2) to invoke with (2,8) / declare with (2,8) - 6 changes
! type: base - added parameters (kbase_1,lbase_1) to invoke with (8,2) / declare with (8,*) - 6 changes
