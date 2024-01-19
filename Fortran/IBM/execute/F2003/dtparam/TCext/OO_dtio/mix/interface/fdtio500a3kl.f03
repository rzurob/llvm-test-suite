! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 10/28/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (two derived types with DTIO
!                               defined for both of them used in the same I/O
!                               data transfer statement)
!                               adaptation: exposed kind
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
        integer(kbase_1), pointer :: i
    end type

    integer(4), parameter :: ISNULL = -999999

    type base1 (lbase1_1) ! lbase1_1=10
       integer, len :: lbase1_1
        character(lbase1_1), allocatable :: c
    end type

    character(*), parameter :: UNALLOCATED = 'UNALLOCATE'
end module


program fdtio500a3kl
use m
    interface read(unformatted)
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedReadBase1 (dtv, unit, iostat, iomsg)
        import base1
            class (base1(*)), intent(inout) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedWriteBase1 (dtv, unit, iostat, iomsg)
        import base1
            class (base1(*)), intent(in) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type(base(4)) :: b1 ! tcx: (4)
    type (base1(10)) :: b2 ! tcx: (10)
    class (base(4)), allocatable :: b3(:) ! tcx: (4)
    class (base1(:)), allocatable :: b4(:) ! tcx: (:)

    integer stat
    character(200) :: errormsg

    integer(4), target :: i1 = 1000

    nullify (b1%i)
    allocate (b2%c, source='xlftest   ')

    open (1, file='fdtio500a3kl.data', form='unformatted')

    stat = -10
    errormsg = 'no error'

    write (1, iostat=stat, iomsg=errormsg) b1, b2

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 101_4

    write (1, iostat=stat, iomsg=errormsg)  base1(10)(null()), base(4)(i1) ! tcx: (4) ! tcx: (10)

    rewind 1

    allocate (b3(2))
    allocate (base1(10)::b4(2)) ! tcx: base(10)

    read (1, iostat=stat, iomsg=errormsg) b3(1), b4(1)

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 2_4

    read (1, iostat=stat, iomsg=errormsg) b4(2), b3(2)

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 3_4

    if (associated (b3(1)%i)) error stop 4_4

    if (allocated (b4(2)%c)) error stop 5_4

    if ((b3(2)%i /= 1000) .or. (b4(1)%c /= 'xlftest')) error stop 6_4
end


subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    !! check for NULL values
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine


subroutine unformattedReadBase1 (dtv, unit, iostat, iomsg)
use m
    class (base1(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer error
    character(10) temp

    !! we don't care if deallocation fails
    if (allocated (dtv%c)) deallocate (dtv%c, stat=error)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    if (temp /= UNALLOCATED) allocate (dtv%c, source=temp)
end subroutine


subroutine unformattedWriteBase1 (dtv, unit, iostat, iomsg)
use m
    class (base1(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated(dtv%c)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%c
    else
        write (unit, iostat=iostat, iomsg=iomsg) UNALLOCATED
    end if
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
! type: base1 - added parameters (lbase1_1) to invoke with (10) / declare with (*) - 7 changes
