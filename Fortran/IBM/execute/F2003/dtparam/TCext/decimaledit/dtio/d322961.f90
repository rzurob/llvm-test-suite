! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self /tstdev/F2003/decimaledit/dtio/d322961.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/17/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 322961)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type node(k1)    ! (4)
        integer, kind           :: k1
        real(k1), allocatable   :: data(:)

        type(node(k1)), pointer :: next => null()

        contains

        procedure :: readNodeFmtd
        generic :: read(formatted) => readNodeFmtd
    end type

    integer :: isum = 0

    contains

    recursive subroutine readNodeFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(node(4)), intent(inout) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        read(unit, *, iostat=iostat, iomsg=iomsg) i1

        if (iostat /= 0) return

        isum = isum + i1

        allocate (dtv%next)

        read (unit, *, iostat=iostat, iomsg=iomsg) dtv%next

        if (iostat /= 0) then   !<-- assume end of file is encountered
            iostat = 0

            deallocate (dtv%next)
        end if
    end subroutine
end module

use m
    class (node(4)), pointer :: n1, iterator

    integer :: count = 0

    write (1, *) (i, i=1,10)

    write (1, '(i10)') (i, i=11, 20)

    rewind 1

    allocate (n1)

    read (1, *) n1

    if (isum /= 210) stop 1

    iterator => n1

    do while (associated(iterator))
        iterator => iterator%next

        count = count + 1
    end do

    if (count /= 20) stop 2
end
