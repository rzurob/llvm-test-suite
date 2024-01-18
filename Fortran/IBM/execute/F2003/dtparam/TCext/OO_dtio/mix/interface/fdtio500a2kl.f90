! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio500a2kl
!*
!*  DATE                       : 2007-08-13 (original: 10/28/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (IO list items are of mixed
!                               data types: some with DTIO)
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
end module


program fdtio500a2kl
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type(base(4)) :: b1, b2 ! tcx: (4)
    class (base(4)), allocatable :: b3, b4 ! tcx: (4)

    integer stat
    character(20) :: errormsg

    integer(4), target :: i1 = 200
    integer(4) i2
    real(4) r1

    type A
        integer i, j
        character(10) c
    end type

    type (A) a1
    logical precision_r4

    nullify (b1%i)
    allocate (b2%i, source=-100)

    open (1, file='fdtio500a2kl.data', form='unformatted')

    stat = -10
    errormsg = 'no error'

    write (1, iostat=stat, iomsg=errormsg) 100.0e0_4, b1, i1, A(1, 2, 'xlftest'), b2

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 101_4

    rewind 1

    allocate (b3, b4)

    read (1, iostat=stat, iomsg=errormsg) r1, b3, i2, a1, b4

    if ((stat /= 0) .or. (errormsg /= 'no error')) error stop 2_4

    if (associated (b3%i) .or. (.not. associated(b4%i))) error stop 3_4

    if (b4%i /= -100) error stop 4_4

    if (i2 /= i1) error stop 5_4

    if (.not. precision_r4(r1, 1.0e2_4)) error stop 6_4

    if ((a1%i /= 1) .or. (a1%j /= 2) .or. (a1%c /= 'xlftest')) error stop 7_4
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    !! check for NULL values
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
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



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
