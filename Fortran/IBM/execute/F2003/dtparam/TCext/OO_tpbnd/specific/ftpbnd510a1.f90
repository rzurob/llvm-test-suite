! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd510a1.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
! %GROUP: ftpbnd510a1.f
! %VERIFY: ftpbnd510a1.out:ftpbnd510a1.vf
! %STDIN:
! %STDOUT: ftpbnd510a1.out
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
!*  DATE                       : 05/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type-bound (type-bound shall be
!*                               available to protected data if it does not
!*                               redefine the variable -- INTENT(IN) attribute)
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

        procedure, non_overridable :: getID => getBaseID

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

module m1
use m, only : base
    type, extends(base) :: child    ! (20,4)
        class (base(:,k1)), pointer :: data => null()

        contains

        procedure, nopass :: typeID => childID
    end type

    type (child(20,4)), protected :: c = child(20,4) (12)

    contains

    subroutine increaseBase (b, i)
        class (base(*,4)), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    integer*4 function childID ()
        childID = 2
    end function
end module

program ftpbnd510a
use m1

    call c%base%print

    if (c%typeID() /= 2) error stop 1_4

    if (c%getID() /= 12) error stop 2_4

    if (c%base%typeID() /= 1) error stop 3_4

    if (c%base%getID () /= 12) error stop 4_4
end
