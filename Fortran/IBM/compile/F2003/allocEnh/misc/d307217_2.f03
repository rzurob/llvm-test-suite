! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/17/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 307217)
!                               diagnostic case
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: id
    end type

    interface write (formatted)
        subroutine writeFormatted (dtv, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    private write(formatted)
end module

use m
    type(base), allocatable :: b1(:,:)

    b1 = reshape([(base(null()), i = 1, 20)], [4,5])

    print *, b1     !<-- illegal
end
