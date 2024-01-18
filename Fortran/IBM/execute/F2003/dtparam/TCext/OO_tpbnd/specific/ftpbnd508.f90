! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_tpbnd/specific/ftpbnd508.f
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
! %GROUP: ftpbnd508.f
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
!*  DATE                       : 02/27/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type-bound specific (one procedure bound to
!*                               multiple bindings; use pass(arg))
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
    type base1(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: value

        contains

        procedure, pass (b1) :: add => addB1B2
    end type

    type base2(n2,k2)    ! (20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)   :: value

        contains

        procedure, pass (b2) :: add => addB1B2
    end type

    type, extends(base1) :: child1(k3,n3)    ! (20,4,4,20)
        integer, kind :: k3
        integer, len  :: n3
        character(n3) :: name
    end type

    contains

    integer*4 function addB1B2 (b1, b2)
        class (base1(*,4)), intent(in) :: b1
        class (base2(*,4)), intent(in) :: b2

        addB1B2 = b1%value + b2%value
    end function
end module

use m
    type, extends (base2) :: child2(k4,n4)    ! (20,4,4,20)
        integer, kind :: k4
        integer, len  :: n4
        character(n4) :: name
    end type

    type (base1(20,4)) :: b1 = base1(20,4) (10)

    type (base2(20,4)) :: b2

    type (child1(20,4,4,20)) :: c1
    type (child2(20,4,4,20)) :: c2

    c1 = child1(20,4,4,20) (-10, 'c1')
    c2 = child2(20,4,4,20) (100, 'c2')

    b2 = base2(20,4) (20)

    if (b1%add (b2) /= 30) error stop 1_4

    if (b2%add (b1) /= 30) error stop 2_4

    if (c1%add(b2) /= 10) error stop 3_4

    if (b2%add(c1) /= 10) error stop 4_4

    if (c1%base1%add(b2) /= 10) error stop 5_4

    if (c1%add(c2) /= 90) error stop 6_4

    if (c2%add(c1) /= 90) error stop 7_4
end
