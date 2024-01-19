! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/4/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (use of part of the
!                               assumed-size array in defined read statement)
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
    type base (kb) ! kb=4
       integer, kind :: kb
        integer(kb), pointer :: i => null()
    end type

    type base1 (kb1) ! kb1=4
       integer, kind :: kb1
        integer(kb1) :: i
    end type
end module

program fdtio001kl
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base(4)) b1(11) ! tcx: (4)

    write (1) base1(4)(1), (base1(4)(j), j=2,10,2) ! tcx: (4) ! tcx: (4)

    rewind(1)

    call xyz (b1(1), b1(2:10))

    if (b1(1)%i /= 1) error stop 1_4

    do i = 2, 10, 2
        if (b1(i)%i /= i) error stop 2_4
        if (associated (b1(i+1)%i)) error stop 3_4
    end do

    contains

    subroutine xyz(a, b)
        class (base(4)), intent(inout) :: b(*) ! tcx: (4)
        class (base(4)), intent(inout) :: a ! tcx: (4)

        read (1) a, b (1:9:2)
    end subroutine
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, stat1

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (associated (dtv%i)) deallocate (dtv%i, stat=stat1)

    allocate (dtv%i, source=temp)
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 5 changes
! type: base1 - added parameters (kb1) to invoke with (4) / declare with (4) - 2 changes
