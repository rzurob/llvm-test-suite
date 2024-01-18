! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio500a4_1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio500a4_1 by Jim Xia)
!*  DATE                       : 2007-08-13 (original: 11/1/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO on generics (io-implied-do loop in output
!                               and arrays in read)
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
        integer(kbase_1), pointer :: i => null()
    end type

    integer(4), parameter :: ISNULL = -999999
end module


program fdtio500a4_1kl
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

    type (base(4)) :: b(5) ! tcx: (4)
    class (base(4)), allocatable :: b1(:) ! tcx: (4)

    integer stat
    character(20) :: errmsg

    integer(4), target :: i1(4), i2

    allocate (b1(0:4))

    open (1, file='fdtio500a4_1kl.data', form='unformatted')

    stat = -10
    errmsg = 'no error'

    i1 = (/(j, j=1,4)/)

    do i = 1, 4
        b(i)%i => i1(i)
    end do

    !! write out using io-implied do loop
    write (1, iostat=stat, iomsg=errmsg) (b(j), j=1,5), 100

    if ((stat /= 0) .or. (errmsg /= 'no error')) error stop 101_4

    rewind (1)

    !! read in the data
    read (1, iostat=stat, iomsg=errmsg) b1, i2

    if ((stat /= 0) .or. (errmsg /= 'no error')) error stop 2_4

    if (i2 /= 100) error stop 3_4

    if ((b1(0)%i /= 1) .or. (b1(1)%i /= 2) .or. (b1(2)%i /= 3) .or. &
        (b1(3)%i /= 4)) error stop 4_4

    if (associated (b1(4)%i)) error stop 5_4

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
