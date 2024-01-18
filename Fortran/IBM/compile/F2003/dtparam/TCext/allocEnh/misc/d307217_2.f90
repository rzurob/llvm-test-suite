! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/misc/d307217_2.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
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
    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: id
    end type

    interface write (formatted)
        subroutine writeFormatted (dtv, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base(*,4)), intent(in) :: dtv
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
    type(base(:,4)), allocatable :: b1(:,:)

    b1 = reshape([(base(20,4)(null()), i = 1, 20)], [4,5])

    print *, b1     !<-- illegal
end
