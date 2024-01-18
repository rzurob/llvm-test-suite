! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/misc/d318782.f
! opt variations: -qnok -ql -qdefaultpv

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
!*  DATE                       : 04/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 318782)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type base(k1)    ! (8)
        integer, kind :: k1
        real(k1)      :: d
    end type

    type container(k2,k3)    ! (4,8)
        integer, kind               :: k2,k3
        type(base(k3)), allocatable :: b
    end type

    contains

    type(container(4,8)) function genCo (d1)
        pointer :: genCo

        real(8), intent(in) :: d1

        allocate(genCo, source=container(4,8)(base(8)(d1)))

    end function

    subroutine printCo (co)
        type(container(4,8)), value :: co

        if (allocated(co%b)) then
            write(*, '(f12.5)') co%b%d
        end if
    end subroutine
end module

use m
    call printCo (genCo(1.2d0))
end
