! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/misc/fmisc046.f
! opt variations: -qnok -qnol

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
!*  DATE                       : 11/29/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 312602)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        procedure(p), pass(b), pointer :: p1
    end type

    contains

    subroutine p (a, b, c)
        class(base(4,*)), intent(in) :: b

        write (*, '(2f12.2)') a, c
    end subroutine

end module

program fmisc046
use m
    type (base(4,20)) b1

    b1%p1 => p

    call b1%p1(1.0, 3.0)
end

