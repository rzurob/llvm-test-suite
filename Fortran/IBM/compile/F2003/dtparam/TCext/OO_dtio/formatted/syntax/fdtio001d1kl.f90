! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (v_list must be of rank-1 array)
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
        real(kb), allocatable :: d1
    end type
end module

program fdtio001d1kl
use m
    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:,:)  !<-- must be rank 1
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 1 changes
