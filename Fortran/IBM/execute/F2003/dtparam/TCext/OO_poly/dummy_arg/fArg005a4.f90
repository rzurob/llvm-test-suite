! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a4.f
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
! %GROUP: fArg005a4.f
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
!*  DATE                       : 05/05/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg's
!*                               association; base type is empty type; use both
!*                               scalars and arrays)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        contains

        procedure, nopass :: typeID => baseID
    end type

    type, extends(base) :: child    ! (4,20)
        integer(k1) :: id = -1

        contains

        procedure, nopass :: typeID => childID
    end type

    class (base(4,:)), pointer :: b1_m, b2_m(:)

    contains

    integer*4 function baseID ()
        baseID = 1
    end function

    integer*4 function childID ()
        childID = 2
    end function
end module

program fArg005a4
use m

    call createBase (b1_m, child(4,20)(1))

    call createBaseArray (b2_m, (/child(4,20)(2), child(4,20)(3)/))

    if (b1_m%typeID() /= 2) error stop 1_4

    if (size (b2_m) /= 2) error stop 2_4

    if (b2_m%typeID() /= 2) error stop 3_4

    contains

    subroutine createBase (b1, b2)
        class (base(4,:)), pointer, intent(out) :: b1
        class (base(4,*)), intent(in) :: b2

        allocate (b1, source=b2)
    end subroutine

    subroutine createBaseArray (b1, b2)
        class (base(4,:)), pointer, intent(out) :: b1 (:)
        class (base(4,*)), intent(in) :: b2(:)

        allocate (b1 (size(b2)), source=b2)
    end subroutine
end
