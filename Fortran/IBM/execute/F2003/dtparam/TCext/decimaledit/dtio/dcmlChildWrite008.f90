! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/decimaledit/dtio/dcmlChildWrite008.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/10/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Derived type with DTIO procedure defined as a
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        complex(k1)      cx(2)

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    !! subroutine controls precision in writing complex; but in a format
    !similar to listed-directed write
    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base(*,8)), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5), allocatable :: mode

        character, parameter :: separator(2) = (/',', ';'/)

        character currSeparator

        allocate (mode)

        inquire (unit, decimal=mode)

        if (mode == 'COMMA') then
            currSeparator = separator(2)
        else if (mode == 'POINT') then
            currSeparator = separator(1)
        else
            error stop 10_4
        end if

        write (unit, '(2(" (", d18.10, a, d18.10, ") "))', &
            iostat=iostat, iomsg=iomsg) (real(dtv%cx(i)), currSeparator, &
                aimag(dtv%cx(i)), i=1,2,1)
    end subroutine
end module

program dcmlChildWrite008
use m
    type A(n2,k2)    ! (20,8)
        integer, kind     :: k2
        integer, len      :: n2
        real(k2)          :: val
        type(base(n2,k2)) :: data
    end type

    type(A(:,8)), allocatable :: a1(:)

    open (1, file='dcmlChildWrite008.out')

    write (1, '(dp, d18.10, DT)', decimal='COMMA') &
            A(20,8)(1.25d0, base(20,8)((/2.3d0, 1.2d0/)))


    allocate(a1(2), source=(/A(20,8)(1.1d1, base(20,8)((1.2d0, 2.4d0))),&
        A(20,8)(2.2d1, data=base(20,8)((/(3.6d0, 4.8d0), (6.0d0, 7.2d0)/)))/))

    open (1, decimal='Comma')

    write (1, *) a1

    write (1, '(d18.10, dp, DT, dc)') a1
end
