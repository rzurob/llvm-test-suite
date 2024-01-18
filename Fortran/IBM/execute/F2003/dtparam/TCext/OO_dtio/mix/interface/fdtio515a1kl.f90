! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio515a1kl
!*
!*  DATE                       : 2007-08-16 (original: 05/31/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (null values in the input record)
!*                               adaptation: exposed kind
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (kA_1) ! kA_1=4
       integer, kind :: kA_1
        integer(kA_1) :: i = -1
    end type

    interface read (formatted)
        subroutine formattedRead (dtv, unit, iotype, vlist, iostat, iomsg)
        import A
            class (A(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character (*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

subroutine formattedRead (dtv, unit, iotype, vlist, iostat, iomsg)
use m, only:A
    class (A(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    character (*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    dtv%i = -1
    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%i
end subroutine


program fdtio515a1kl
use m
    type(a(4))  a1(3) ! tcx: (4)

    write (1, *) '1, ,3, 4'
    write (1, *) ', 20,30,40'

    rewind (1)

    read (1, *) a1

    if (any (a1%i /= (/1, -1, 3/))) error stop 101_4

    read (1, *) a1

    if (any (a1%i /= (/-1, 20, 30/))) error stop 2_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA_1) to invoke with (4) / declare with (4) - 3 changes
