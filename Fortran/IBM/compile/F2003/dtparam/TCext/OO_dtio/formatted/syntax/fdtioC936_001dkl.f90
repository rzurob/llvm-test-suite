! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/4/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (syntax check for FORMATTED
!                               READ; dtv must be class keyword for extensible
!                               type)
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
    type point (kp) ! kp=4
       integer, kind :: kp
        real(kp) :: x, y
    end type
end module

program fdtioC936_001dkl
use m
    interface WRITE(FORMATTED)
        subroutine writePointFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            type (point(4)), intent(in) :: dtv  !<-- must use class keyword ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

end


! Extensions to introduce derived type parameters:
! type: point - added parameters (kp) to invoke with (4) / declare with (4) - 1 changes
