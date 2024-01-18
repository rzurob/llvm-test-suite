! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_poly/misc/fmisc041.f
! opt variations: -qnock

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
!*  DATE                       : 07/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous items (defect 290055)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fmisc041
    type base(k1,n1)    ! (1,10)
        integer, kind                          :: k1
        integer, len                           :: n1
        character(kind=k1,len=n1), allocatable :: data(:)
    end type

    type (base(1,10)) :: b1

    allocate (b1%data(2))

    b1%data = 'test 101'

    print *, (/(b1%data(i)(:5), i=1,2)/)
    print *, (/(b1%data(i)(:5)//char(ichar('0')+i), i=1,2)/)
    print *, (/(b1%data(i)(i:i+5), i=1,2)/)
end
