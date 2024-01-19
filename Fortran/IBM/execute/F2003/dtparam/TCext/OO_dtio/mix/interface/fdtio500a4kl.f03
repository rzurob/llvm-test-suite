! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 11/1/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (use of io-implied-do loop for
!                               the item list)
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


program fdtio500a4kl
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
    character(20) :: errmsg

    integer(4), target :: i1(4), i2

    allocate (b3, b4)

    open (1, file='fdtio500a4kl.data', form='unformatted')

    stat = -10
    errmsg = 'no error'

    i1 = (/(j, j=1,4)/)

    !! write out using io-implied do loop
    write (1, iostat=stat, iomsg=errmsg) (base(4)(i1(j)), j=1,4), 100 ! tcx: (4)

    if ((stat /= 0) .or. (errmsg /= 'no error')) error stop 101_4

    rewind (1)

    !! read in the data
    read (1, iostat=stat, iomsg=errmsg) b1, b2, b3, b4, i2

    if ((stat /= 0) .or. (errmsg /= 'no error')) error stop 2_4

    if (i2 /= 100) error stop 3_4

    if ((b1%i /= 1) .or. (b2%i /= 2) .or. (b3%i /= 3) .or. (b4%i /= 4)) &
                    error stop 4_4
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
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
