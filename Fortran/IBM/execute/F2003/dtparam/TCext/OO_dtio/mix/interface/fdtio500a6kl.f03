! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 11/10/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (test I/O for rank-2 arrays)
!*                               adaptation: exposed kind
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
        integer(kbase_1), pointer :: i => null()
    end type

    integer(4), parameter :: ISNULL = -999999
end module


program fdtio500a
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

    class(base(4)), allocatable :: b1(:,:), b2(:,:) ! tcx: (4)
    integer stat
    character(20) :: errormsg

    integer(4), target :: i1 = 200

    allocate (b1(0:1,2), b2(2,0:1))

    nullify (b1(1,1)%i, b1(0,2)%i)

    allocate (b1(0,1)%i, source=100)
    allocate (b1(1,2)%i, source=-10)

    !! initialize b2, they'll be changed via read
    allocate (b2(1,0)%i, source=-1)
    allocate (b2(2,0)%i, source=-2)
    allocate (b2(1,1)%i, source=-3)

    open (1, file='fdtio500a6kl.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg) b1

    if (stat /= 0) error stop 101_4

    rewind 1

    read (1, iostat=stat, iomsg=errormsg) b2

    if (stat /= 0) error stop 2_4


    !! verify the results of b2
    if (associated (b2(2,0)%i) .or. associated (b2(1,1)%i)) error stop 3_4

    if ((b2(1,0)%i /= b1(0,1)%i) .or. (b2(1,0)%i /= 100)) error stop 4_4

    if ((b2(2,1)%i /= b1(1,2)%i) .or. (b2(2,1)%i /= -10)) error stop 5_4

    close (1, status='delete')
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

    if (iostat /= 0) return

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
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes