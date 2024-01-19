! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/misc/d318624.f
! opt variations: -ql

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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        generic :: write (formatted) => write
        procedure, private :: write => writeb
    end type

    abstract interface
        subroutine absWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import
            class(base(4)), intent(in) :: dtv
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
