! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd505.f
! opt variations: -qnok -qnol -qreuse=none

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
! %GROUP: ftpbnd505.f
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
!*  DATE                       : 03/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (elemental nopass binding;
!*                               dummy-arg could be array or scalar)
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
    type dataType(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: typeID => dataTypeID
    end type

    type, extends(dataType) :: moduleData    ! (4,20)
        integer(k1) :: id

        contains
        procedure, nopass :: typeID => moduleDataID
        procedure, nopass :: count => dataCounter
    end type

    contains

    integer*4 function dataTypeID ()
        dataTypeID = 1
    end function

    integer*4 function moduleDataID ()
        moduleDataID = 2
    end function

    elemental integer*4 function dataCounter (b)
        class (moduleData(4,*)), intent(in) :: b

        dataCounter = 0
        if (b%id > 0) dataCounter = 1
    end function
end module

program ftpbnd505
use m
    type (moduleData(4,20)) :: d1(10)

    d1 = (/(moduleData(4,20)(i), i=-5,4)/)

    if (d1%typeID() /= 2) error stop 1_4

    if (d1(10)%typeID() /= 2) error stop 2_4

    if (sum (d1%count(d1)) /= 4) error stop 3_4

    if (sum (d1(2)%count(d1)) /= 4) error stop 4_4

    if (d1%count(d1(1)) /= 0) error stop 5_4

    if (d1(5)%count(d1(9)) /= 1) error stop 6_4
end
