! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 03/03/2005)
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (test inquire statement during
!                               DTIO)
!                               adaptation: exposed kind
!*
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
        subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
        use m
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio509a3kl
use m
use m1, only: read (unformatted)
    type (base(4)) :: b1 ! tcx: (4)

    character(8) :: pos

    integer stat1
    character(200) err

    write(1) 100

    inquire(1, position=pos)

    print *, pos

    rewind 1

    inquire(1, position=pos)

    print *, pos

    read (1, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 101_4
    end if

    inquire(1, position=pos)

    print *, pos

    close(1, status='delete')
end


subroutine unformattedReadBase (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(20) :: pos

    read (unit, iostat=iostat, iomsg=iomsg) i1

    if (iostat /= 0) return

    inquire (unit, position=pos)

    if (pos /= 'ASIS') then
        iostat = 1000
        iomsg = 'inquire failed'

        return
    end if

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
