! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/argAssociation/dtio002.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/12/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use the intrinsic assignment in a DTIO routine
!                               (write statement).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind         :: k1
        integer, len          :: n1
        real(k1), allocatable :: r1(:)
        real(k1), allocatable :: r2(:,:)

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,4)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        real, allocatable :: localVar(:)

        if (allocated(dtv%r1)) then
            if (allocated(dtv%r2)) then
                localVar = (/dtv%r1, dtv%r2/)
            else
                localVar = (/dtv%r1/)
            end if
        else
            if (allocated(dtv%r2)) then
                localVar = (/dtv%r2/)
            end if
        end if

        if (allocated(localVar)) then
            write (unit, '(5g12.4)', iostat=iostat, iomsg=iomsg) localVar
        else
            write (unit, *, iostat=iostat, iomsg=iomsg) 'no data to write'
        end if

        if (iostat /= 0) return

        write (unit, *) new_line('a')
    end subroutine
end module

program dtio002
use m
    type(base(:,4)), allocatable :: b1(:), b2(:)

    b1 = (/(base(20,4)(null(), null()), i=1,10)/)

    print *, b1

    b2 = (/(base(20,4)((/(j, j=1,i)/),reshape((/(j, j=1,i*i)/), (/i,i/))), &
        base(20,4)((/(j, j=1,i)/), null()), base(20,4)(null(), reshape((/(j, j=1,i*i)/), &
        (/i,i/))), i=1,3)/)

    write (*, '(9DT)') b2
end
