! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-07-23 (original: 11/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (C936, CLASS keyword for
!                               bind(C) type)
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
    use ISO_C_BINDING

    type, bind(c) :: bType (kbs,kbi) ! kbs,kbi=C_SHORT,C_INT
       integer, kind :: kbs,kbi
        integer(kbs) :: i
        integer(kbi) :: j
    end type
end module

program fdtioC936_001d2kl
use m
use ISO_C_BINDING
    interface read(formatted)
        subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
        import C_SHORT,C_INT
            class (bType(C_SHORT,C_INT)), intent(inout) :: dtv     !<-- illegal ! tcx: (C_SHORT,C_INT)
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end


! Extensions to introduce derived type parameters:
! type: bType - added parameters (kbs,kbi) to invoke with (C_SHORT,C_INT) / declare with (C_SHORT,C_INT) - 1 changes
