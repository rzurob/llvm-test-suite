! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qreuse=self /tstdev/F2003/decimaledit/defaultIO/commaEdit006.f
! opt variations: -qnol -qdefaultpv -qreuse=none

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 06/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the COMMA mode for the derived type object
!                               in list-dirceted output and input.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        complex(k1)   :: cx(3)
    end type

    type B(n2,k2,k3)    ! (20,4,8)
        integer, kind  :: k2,k3
        integer, len   :: n2
        integer(k2)       i
        real(k2)          r1
        logical(k2)       j(2)
        type(A(n2,k3)) :: a1 = A(20,k3) ((/1.0, 2.0, 3.0/))
    end type
end module

program commaEdit006
use m
    type (A(20,8)) a1(10)
    type (B(20,4,8)) b1(10)

    logical(4) precision_x6, precision_r4

    character(10) :: stream='stream'

    open (9, file='commaEdit006.out', decimal='decimal point is COMMA'(18:22),&
            access=stream, form='formatted')

    write (9, *) (A(20,8)(cmplx(3.0*i, 3.0+i, kind=8)), i=1, 10)

    write (9, *, decimal = 'POINT') (B(20,4,8)(i, i*1.0, mod(i,2) == 1), i=1,10)

    read (9, *, pos=1, decimal='COMMA') a1
    read (9, *, decimal='POINT') b1

    do i = 1, 10
        do j = 1, 3
            if (.not. precision_x6(a1(i)%cx(j), cmplx(3.0*i, 3.0+i, kind=8))) &
                error stop 1_4

            if (.not. precision_x6(b1(i)%a1%cx(j), cmplx(j*1.0, 0.0, 8))) &
                error stop 2_4
        end do

        if (b1(i)%i /= i) error stop 3_4

        if (.not. precision_r4 (b1(i)%r1, i*1.0)) error stop 4_4

        if (any(b1(i)%j .neqv. (mod(i,2) == 1))) error stop 5_4
    end do
end

