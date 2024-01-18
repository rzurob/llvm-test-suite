! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_tpbnd/specific/ftpbnd515a2.f
! opt variations: -qck -qnok -qnol -qreuse=base

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
! %GROUP: ftpbnd515a2.f
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
!*  DATE                       : 05/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type bound (elemental binding calls;
!                               use array constructors as the actual argument)
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
    end type

    type, extends(base) :: child1(k2,n2)    ! (20,4,4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: name

        contains

        procedure, pass (c1) :: compare => compareC1C2
    end type

    type, extends(base) :: child2(k3,n3)    ! (20,4,4,15)
        integer, kind :: k3
        integer, len  :: n3
        character(n3) :: name

        contains

        procedure, pass (c2) :: compare => compareC1C2
    end type

    contains

    elemental logical function compareC1C2 (c1, c2)
        class (child1(*,4,4,*)), intent(in) :: c1
        class (child2(*,4,4,*)), intent(in) :: c2

        compareC1C2 = ((c1%name == c2%name) .and. (c1%id == c2%id))
    end function
end module

program ftpbnd515a2
use m
    type (child1(20,4,4,20)) :: c1(10)
    type (child2(20,4,4,15)) :: c2(2:11)

    c1 = (/(child1(20,4,4,20) (i, 'test'),i=1,10)/)

    c2 = (/(child2(20,4,4,15) (i, 'test'),i=1,10)/)

    !! check for the array constructor
    if (.not. all (c1%compare ((/(child2(20,4,4,15)(i, 'test'),i=1,10)/)))) error stop 1_4

    if (.not. all (c2%compare ((/(child1(20,4,4,20)(i, 'test'), i=1,10)/)))) error stop 2_4
end
