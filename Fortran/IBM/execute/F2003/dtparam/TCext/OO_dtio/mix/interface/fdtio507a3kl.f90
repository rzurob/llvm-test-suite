! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 11/3/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (test the END= in the parent
!                               data transfer statement; when EOF condition
!                               occurs then the execution will branch to the
!                               labeled statement)
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
    type base (lbase_1) ! lbase_1=150
       integer, len :: lbase_1
        character(lbase_1), allocatable :: name
    end type
end module

program fdtio507a2
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base(*)), intent(inout) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base(:)), allocatable :: b1(:) ! tcx: (:)
    integer stat
    character(200) err

    err = 'no error'
    allocate (base(150)::b1(2)) ! tcx: base(150)

    open (1, file='fdtio507a3kl.data', form='formatted')
    write (1, '(a10)') 'U'

    close(1)

    open (1, file='fdtio507a3kl.data', form='unformatted')

    read (1, iostat=stat, iomsg=err, end=100) b1(1), b1(2)

    print *, stat, err
    print *, b1(1)%name, b1(2)%name
    stop 1

100 if (stat == 0) error stop 101_4

    close (1, status='delete')
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(150) name

    integer stat1

    read (unit, iostat=iostat, iomsg=iomsg) name

    if (iostat /= 0) return

    deallocate (dtv%name, stat=stat1)
    allocate (dtv%name, source=name)

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (150) / declare with (*) - 3 changes
