!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio507a4.f
! %VERIFY: fdtio507a4.out:fdtio507a4.vf
! %STDIN:
! %STDOUT: fdtio507a4.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/3/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO on generics (REC= shall not appear in a
!                               child data transfer statement)
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
    type base
        real(4), allocatable :: d1(:)
    end type

    character(12), parameter :: UNALLOCATED =  'UNALLOCATED '
    character(12), parameter :: ALLOCED = 'ALLOCATED   '
end module

program fdtio507a4
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base) b1
    integer stat
    character(8) :: err = 'no error'

    call writeData

    read (1, iostat=stat, iomsg=err, rec=2) b1

    if ((stat == 0) .or. (err == 'no error')) error stop 2_4

    print *, stat, err
end


subroutine writeData
use m
    interface write (unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base), pointer :: b1
    integer stat
    character(8) errmsg

    errmsg = 'no error'

    allocate (b1, source=base((/0.0e0_4/)))

    open (1, file='fdtio507d.data', access='direct', form='unformatted',  &
                        recl=1000)

    write (1, iostat=stat, iomsg=errmsg, rec=2) b1

    !! this write should fail
    if ((stat == 0) .or. (errmsg == 'no error')) error stop 1_4

    print *, stat, errmsg
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(12) allocStat
    integer lb, ub

    read (unit, iostat=iostat, iomsg=iomsg, rec=1)  allocStat

    if (iostat /= 0) return

    !! we only change the value of dtv if the data source from file is allocated
    if (allocStat == ALLOCED) then
        read (unit, iostat=iostat, iomsg=iomsg, rec=1)  lb, ub

        if (iostat /= 0) return

        if (allocated (dtv%d1))  deallocate (dtv%d1)

        allocate (dtv%d1(lb:ub))

        read (unit, iostat=iostat, iomsg=iomsg, rec=1) dtv%d1
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%d1)) then
        write (unit, iostat=iostat, iomsg=iomsg, rec=1) ALLOCED, lbound(dtv%d1,1), &
                ubound(dtv%d1,1), dtv%d1
    else
        write (unit, iostat=iostat, iomsg=iomsg, rec=1) UNALLOCATED
    end if
end subroutine
