! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio004dkl
!*
!*  DATE                       : 2007-07-23 (original: 11/24/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO generics (syntax check for iotype for
!                               formatted read; can't be array)
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
        complex(kb), allocatable :: data
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype(2)
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

program fdtio004dkl
end


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 1 changes
