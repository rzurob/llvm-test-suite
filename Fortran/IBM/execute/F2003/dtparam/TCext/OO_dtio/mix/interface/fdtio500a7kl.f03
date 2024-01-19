! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 12/22/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (use module procedure as the DTIO
!                               subroutines for unformatted read)
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
        real(kbase_1), pointer :: r1 => null()
    end type

    real(4), parameter :: isNull = -9.99e-9

    interface read(unformatted)
        module procedure unformattedRead
    end interface

    contains

    subroutine unformattedRead (dtv,unit, iostat, iomsg)
        class (base(4)), intent(inout) :: dtv ! tcx: (4)
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        logical precision_r4

        real(4) :: temp = 1.0

        read (unit, iostat=iostat, iomsg=iomsg) temp

        if (iostat /= 0) return

        !! test if the data is indicating a null value
        if (precision_r4 (temp, isNull)) return

        if (.not. associated (dtv%r1)) then
            allocate (dtv%r1)
        end if

        dtv%r1 = temp

    end subroutine
end module

program fdtio500a7kl
use m
    type (base(4)) b1(2) ! tcx: (4)
    class (base(4)), allocatable :: b2 ! tcx: (4)

    integer stat1
    character(200) error

    logical precision_r4

    open (1, file='fdtio500a7kl.data', form='unformatted')

    write (1) 1.03, isNull
    write (1) isNull

    rewind (1)

    read (1, iostat=stat1, iomsg=error) b1

    if (stat1 /= 0) then
        print *, stat1, error
        error stop 101_4
    end if

    allocate (b2)

    read (1, iostat=stat1, iomsg=error) b2

    if (stat1 /= 0) then
        print *, stat1, error
        error stop 2_4
    end if

    !! verify the results

    if (.not. associated (b1(1)%r1)) error stop 3_4

    if (associated (b1(2)%r1) .or. associated (b2%r1)) error stop 4_4

    if (.not. precision_r4 (b1(1)%r1, 1.03_4)) error stop 5_4

    close (1, status='delete')
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
