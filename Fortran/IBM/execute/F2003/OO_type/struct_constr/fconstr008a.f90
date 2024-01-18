!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr008a.f
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
!*  DESCRIPTION                : structure constructor (three generations and
!*                               the previous two generations are both empty
!*                               types)
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
    end type

    type, extends (child) :: thirdGeneration
        integer*4 ::id
        character*20 :: name
    end type

    type (thirdGeneration) :: t1_m = thirdGeneration (1, 't1_m')
    type (thirdGeneration) :: t2_m = thirdGeneration (name = 't2_m', id = 2)

    type (thirdGeneration) :: t3_m = thirdGeneration (base = base(), id = 10, &
            name = 't3_m')

    type (thirdGeneration) :: t4_m = thirdGeneration (child = child(), id = 20, &
            name = 't4_m')

end module

program fconstr008a
use m

    type (base) :: b1 = base()
    type (child) :: c1 = child()
    type (thirdGeneration) :: t1 = thirdGeneration (3, name = 't1')
    type (thirdGeneration) :: t2 = thirdGeneration (name = 't2', id = 4)

    type (thirdGeneration) :: t3 = thirdGeneration (base = base(), name = &
            't3', id = 30)

    type (thirdGeneration) :: t4 = thirdGeneration (child = child(), id = &
            40, name = 't4')


    ! validate all the data entities
    if (t1_m%id /= 1) error stop 1_4
    if (t1_m%name /= 't1_m') error stop 2_4

    if (t2_m%id /= 2) error stop 3_4
    if (t2_m%name /= 't2_m') error stop 4_4

    if (t1%id /= 3) error stop 5_4
    if (t1%name /= 't1') error stop 6_4

    if (t2%id /= 4) error stop 7_4
    if (t2%name /= 't2') error stop 8_4

    if (t3_m%id /= 10) error stop 9_4
    if (t3_m%name /= 't3_m') error stop 10_4

    if (t4_m%id /= 20) error stop 11_4
    if (t4_m%name /= 't4_m') error stop 12_4

    if (t3%id /= 30) error stop 13_4
    if (t3%name /= 't3') error stop 14_4

    if (t4%id /= 40) error stop 15_4
    if (t4%name /= 't4') error stop 16_4

    print *, b1, c1     ! nothing prints out
end
