!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr011.f
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
!*  DESCRIPTION                : structure constructor (base type fully
!*                               initialized by default; only supply data for
!*                               extended type object)
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
        integer*4 ::id = 1
        real*4 :: value = 0.0
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    type (child), save :: c1_m = child (name = 'c1_m')
end module

program fconstr011
use m

    type (base) :: b1
    type (child) :: c1

    b1 = base(2)

    c1 = child(name = 'c1')

    if ((b1%id /= 2) .or. (b1%value /= 0.0)) error stop 1_4

    if ((c1%id /= 1) .or. (c1%value /= 0.0) .or. (c1%name /= 'c1')) error stop 2_4

    if ((c1_m%id /= 1) .or. (c1_m%value /= 0.0) .or. &
        (c1_m%name /= 'c1_m')) error stop 3_4
end
