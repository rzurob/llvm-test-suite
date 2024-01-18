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
! %GROUP: fconstr016a.f
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
!*  DATE                       :
!*  ORIGIN                     :
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (parent type renamed
!*                               out of the module)
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
        integer*4 ::id
        real*4 :: value = 1.0
    end type

    type(base), save :: b1_m = base (2, 10.0)
end module

module m1
use m, newBase => base

    type, extends (newBase) :: child
        character(20) :: name
    end type

    type (newBase), save :: n1_m = newBase (1)
    type (child), save :: c1_m = child (2, 2.0, 'c1_m')
    type (child), save :: c2_m = child (newbase = newBase (3, 3.0), name = 'c2_m')

    contains

    subroutine updateC1_m
        c1_m%newbase = b1_m
    end subroutine
end module


program fconstr016a
use m, anotherBase => base
use m1, only: n1_m, c1_m, updateC1_m, c2_m, child

    type (anotherBase) :: a1

    type (child) :: c1 = child (newbase = anotherBase (10, 0.0), name = 'c1')
    type (child) :: c2 = child (newbase = anotherBase (value = 1.0, id = 20), &
                    name = 'c2')

    type (child) :: c3

    c3 = child (newbase = c2%newbase, name = 'c3')

    a1 = b1_m

    if ((a1%id /= 2) .or. (a1%value /= 10.0) ) error stop 1_4

    a1 = n1_m   ! two objs with different renamed base type
    if ((a1%id /= 1) .or. (a1%value /= 1.0)) error stop 2_4

    a1 = c1_m%newbase
    if ((a1%id /= 2) .or. (a1%value /= 2.0)) error stop 3_4

    b1_m = n1_m

    if ((b1_m%id /= 1) .or. (b1_m%value /= 1.0)) error stop 4_4

    call updateC1_m

    if ((c1_m%id /= 1) .or. (c1_m%value /= 1.0)) error stop 5_4
    if (c1_m%name /= 'c1_m') error stop 6_4

    a1 = c2_m%newbase
    if ((a1%id /= 3) .or. (a1%value /= 3.0)) error stop 7_4
    if (c2_m%name /= 'c2_m') error stop 8_4

    a1 = c1%newbase
    if ((a1%id /= 10) .or. (a1%value /= 0.0)) error stop 9_4
    if (c1%name /= 'c1') error stop 10_4


    a1 = c2%newbase
    if ((a1%id /= 20) .or. (a1%value /= 1.0)) error stop 11_4
    if (c2%name /= 'c2') error stop 12_4

    a1 = c3%newbase
    if ((a1%id /= 20) .or. (a1%value /= 1.0)) error stop 13_4
    if (c3%name /= 'c3') error stop 14_4

end
