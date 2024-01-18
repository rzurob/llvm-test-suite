! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio002d2kl
!*
!*  DATE                       : 2007-07-23 (original: 11/16/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (test that unit must be scalar)
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
    type base (lb) ! lb=5
       integer, len :: lb
        character(lb), allocatable :: data
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(*)), intent(inout) :: dtv ! tcx: (*)
            integer, intent(in), dimension(*) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio002d
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (lb) to invoke with (5) / declare with (*) - 1 changes
