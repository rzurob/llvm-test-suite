! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio500_2kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio500_2 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 11/10/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (test the use only of the DTIO generics)
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


module m1
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
end module

program fdtio500_2kl
use m
use m1, only: read(unformatted)

    type (base(4)) :: b1, b2 ! tcx: (4)

    integer(4) stat
    character(200) err

    open (1, form='unformatted', file='fdtio500_2kl.data')

    call writeData

    rewind(1)

    read (1, iostat = stat, iomsg=err) b1

    if (stat /= 0) error stop 3_4

    read (1, iostat = stat, iomsg=err) b2

    if (stat /= 0) error stop 4_4

    !! verify the results

    if (associated(b1%i)) error stop 5_4

    if (b2%i /= 20) error stop 6_4

    close(1, status='delete')
end


subroutine writeData
use m
use m1, only: write (unformatted)
    integer stat
    character(200) :: err

    type (base(4)) :: b1 ! tcx: (4)

    allocate (b1%i, source=20)

    write(1, iostat=stat, iomsg=err) base(4)(null()) ! tcx: (4)

    if (stat /= 0) error stop 101_4

    write (1, iostat=stat, iomsg=err) b1

    if (stat /= 0) error stop 2_4
end subroutine

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

    !! check for NULL pointer
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
