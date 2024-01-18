!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr007.f
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
!*  DESCRIPTION                : structure constructor (empty base type)
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
    type base
    end type

    type, extends(base) :: child
        integer*4 ::id
        character*20 :: name
    end type

    type(base) :: b1_m = base()
    type(child) :: c1_m = child (0, 'module data c1_m')
end module

program fconstr007
use m

    type (base) :: b1 = base()
    type (child) :: c1 = child(1, 'test data c1')

    print *, b1, b1_m

    if (c1_m%id /= 0) error stop 1_4
    if (c1_m%name /= 'module data c1_m') error stop 2_4

    if (c1%id /= 1) error stop 3_4
    if (c1%name /= 'test data c1') error stop 4_4
end
