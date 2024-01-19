! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-19 (original: 11/17/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (test on the case that unit=*
!                               used in parent; the unit in child must be
!                               INPUT_UNIT for READ statement)
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
    type base (kb)
       integer, kind :: kb
        real(kb), allocatable :: r1
        double precision :: d1
    end type

    character(*), parameter :: inputUnitErr = &
        'input unit should be INPUT_UNIT defined in ISO_FORTRAN_ENV'

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio052a_1
use m
    class (base(4)), allocatable :: b1(:)

    integer stat
    character(200) err

    allocate (b1(0:1))

    write (1, *) 1.0, 2.0d0

    rewind 1

    read (1, *, iostat=stat, iomsg=err) b1

    close (1, status = 'delete')

    !! verify that the read statement failed
    if ((stat /= 1) .or. (err /= inputUnitErr)) error stop 1_4

    if (allocated (b1(0)%r1)) error stop 2_4
end


!! this routine should be called only using unit=* by the parent
subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, inputUnitErr
use iso_fortran_env
    class (base(4)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg


    if (unit /= INPUT_UNIT) then
        iostat = 1
        iomsg = inputUnitErr
        return
    end if

    if (allocated (dtv%r1)) deallocate (dtv%r1)

    allocate (dtv%r1)

    read *, dtv%r1, dtv%d1
end subroutine
