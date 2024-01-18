! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc010b.f
! opt variations: -qnol -qreuse=self

!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fmisc010b.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 08/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : miscellaneous items (defect 277245; problem #3:
!                               structure constructor in an implied-do in array
!                               constructor for structure with allocatable
!                               components)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1,k2)    ! (20,4,4)
        integer, kind            :: k1,k2
        integer, len             :: n1
        integer(k1)              :: id
        integer(k2), allocatable :: data
    end type

    type base1(n2,k3)    ! (20,4)
        integer, kind            :: k3
        integer, len             :: n2
        integer(k3), allocatable :: data(:)
    end type
end module

program fmisc010b
use m
    type (base(20,4,4)) :: b1(2:11) 

    type (base1(20,4)) :: b11 (3)

    b1 = (/(base(20,4,4)(i, i), i = 1, 10)/)

    if (any (b1%id /= (/1,2,3,4,5,6,7,8,9,10/))) error stop 1_4

    do i = 1, 10
        if (b1(i+1)%data /= i) error stop 2_4
    end do

    b11 = (/(base1(20,4)((/(i, i=1,j)/)), j=1,3)/)

    if (any (b11(1)%data /= (/1/))) error stop 3_4
    if (any (b11(2)%data /= (/1, 2/))) error stop 4_4
    if (any (b11(3)%data /= (/1, 2, 3/))) error stop 5_4
end 

