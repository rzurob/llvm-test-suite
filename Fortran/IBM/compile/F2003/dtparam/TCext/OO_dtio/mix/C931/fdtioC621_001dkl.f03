! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/4/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (C931 and C621)
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
end module

program fdtioC621_001dkl
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

    type (base(4)) b1(10) ! tcx: (4)

    call xyz (b1(1), b1(2:10))

    contains

    subroutine xyz(a, b)
        class (base(4)), intent(inout) :: b(*) ! tcx: (4)
        class (base(4)), intent(inout) :: a ! tcx: (4)

        read (1) a, b(1::2)     !<-- illegal
        read (1) b(5)           !<-- this is legal
    end subroutine
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 4 changes
