! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio052akl
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

program fdtio052a
use m
    class (base(4)), allocatable :: b1(:)

    logical precision_r4, precision_r8

    integer stat
    character(200) err

    allocate (b1(0:1))

    read *, b1

    if ((.not. precision_r4 (b1(0)%r1, 1.0)) .or. (.not. precision_r4 &
                (b1(1)%r1, 2.0))) error stop 1_4


    if ((.not. precision_r8 (b1(0)%d1, 1.0d0)) .or. (.not. precision_r8 &
                (b1(1)%d1, 3.0d0))) error stop 2_4

    read (*, *, iostat=stat, iomsg=err) b1(1)

    if (stat /= 0) then
        print *, stat, err
        error stop 5_4
    end if

    if (.not. precision_r4 (b1(1)%r1, 1.02e1)) error stop 3_4

    if (.not. precision_r8 (b1(1)%d1, 76.0d0)) error stop 4_4
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
