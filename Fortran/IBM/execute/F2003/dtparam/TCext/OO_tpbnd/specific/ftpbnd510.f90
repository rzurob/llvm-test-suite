! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specific/ftpbnd510.f
! opt variations: -qnol

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
! %GROUP: ftpbnd510.f
! %VERIFY: ftpbnd510.out:ftpbnd510.vf
! %STDIN:
! %STDOUT: ftpbnd510.out
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
!*  DATE                       : 03/18/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : specific type bound (binding calls can be
!*                               invoked for parameter constant as long as the
!*                               dummy-arg is intent(in))
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure, nopass :: typeID => baseID

        procedure :: getID => getBaseID

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base(*,4)), intent(in) :: b

        print *, b%id
    end subroutine

    integer*4 function baseID ()
        baseID = 1
    end function

    integer*4 function getBaseID (b)
        class (base(*,4)), intent(in) :: b

        getBaseID = b%id
    end function
end module

program ftpbnd510
use m
    type(base(20,4)), parameter :: b = base(20,4)(10)

    call b%print

    if (b%typeID() /= 1) error stop 1_4

    if (b%getID() /= 10) error stop 2_4
end
