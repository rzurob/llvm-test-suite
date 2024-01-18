!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/30/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: selected_real_kind() used in the
!                               declaration of extended type components' kind
!                               type parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type arrayTemplate (size)
        integer, len :: size
    end type arrayTemplate
end module

module m1
use m
    type, extends(arrayTemplate) :: realArray (p, r)
        integer, kind :: p, r

        real (selected_real_kind(p, r)) data(size)
    end type realArray
end module

module param
    integer, parameter :: pd = 8
    integer, parameter :: rd = 70

    integer, parameter :: ps = 6
    integer, parameter :: rs = 30
end module

program dtparamInitexpr001
use m1
use param

    type(realArray(p=pd, r = rd, size=100)) da1     !<-- double precision array
    type(realArray(p=ps, r = rs, size=200)) ra1     !<-- single precision array

    logical(4) precision_r4, precision_r8

    !! initialize arrays
    call initializeReal8 (da1%data, da1%size)
    call initializeReal4 (ra1%data, ra1%size)

    !! verify the data
    do i = 1, 100
        if (.not. precision_r8(da1%data(i), i*1.1d0))  error stop 1_4
    end do

    do i = 1, 200
        if (.not. precision_r4(ra1%data(i), (201-i)*1.2e0)) error stop 2_4
    end do
end

subroutine initializeReal8 (d1, n)
use param
    integer, intent(in) :: n
    real(selected_real_kind(pd, rd)), intent(inout):: d1(n)

    d1 = (/(i*1.1d0, i=1, n)/)
end subroutine

subroutine initializeReal4 (r1, n)
use param
    integer, intent(in) :: n
    real(selected_real_kind(ps, rs)), intent(inout) :: r1(n)

    r1 = (/(i*1.2e0, i=n, 1, -1)/)
end subroutine
