! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/17/2006
!*
!*  DESCRIPTION                : miscellaneous(defect 318624)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        generic :: write (formatted) => write
        procedure, private :: write => writeb
    end type

    abstract interface
        subroutine absWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import
            class(base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in)  :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    procedure(absWrite) writeb, writebaseext

    interface write(formatted)
        procedure writebaseext
    end interface
end module

end
