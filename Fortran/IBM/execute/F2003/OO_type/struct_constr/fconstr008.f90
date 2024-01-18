!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr008.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (no more component in
!*                               extending type)
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
    type, extends(base) :: child
    end type

    type base
        integer*4 ::id
        character*20 :: name
    end type

    type(base) :: b1_m = base(10, 'module data b1_m')
    type(child) :: c1_m = child(20, 'module data c1_m')
end module

program fconstr008
use m

    type (base) :: b1 = base(0, 'test data b1')
    type (child) :: c1 = child(1, 'test data c1')

    if (b1_m%id /= 10) error stop 1_4
    if (b1_m%name /= 'module data b1_m') error stop 2_4

    if (c1_m%id /= 20) error stop 3_4
    if (c1_m%name /= 'module data c1_m') error stop 4_4

    if (b1%id /= 0) error stop 5_4
    if (b1%name /= 'test data b1') error stop 6_4

    if (c1%id /= 1) error stop 7_4
    if (c1%name /= 'test data c1') error stop 8_4
end
