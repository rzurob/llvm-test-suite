! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (all components default
!*                               initialized, omitting them)
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
        character*20 :: name = 'test'
    end type

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet = .false.
    end type

    type (base), save :: b1_m = base()
    type (child), save :: c1_m = child()
    type (thirdGeneration), save :: t1_m = thirdGeneration()


end module

program fconstr009
use m

    type (base) :: b1
    type (child) :: c1
    type (thirdGeneration) :: t1

    type (base) :: b2 = base()
    type (child) :: c2 = child()
    type (thirdGeneration) :: t2 = thirdGeneration()

    b1 = base()
    c1 = child()
    t1 = thirdGeneration()

    if ( (b1%id /= 1) .or. (b1%value /= 0.0)) error stop 1_4

    if ( (c1%id /= 1) .or. (c1%value /= 0.0) .or. &
         (c1%name /= 'test')) error stop 2_4

    if ( (t1%id /= 1) .or. (t1%value /= 0.0) .or. &
         (t1%name /= 'test') .or. t1%isSet) error stop 3_4

    if ( (b2%id /= 1) .or. (b2%value /= 0.0)) error stop 4_4

    if ( (c2%id /= 1) .or. (c2%value /= 0.0) .or. &
         (c2%name /= 'test')) error stop 5_4

    if ( (t2%id /= 1) .or. (t2%value /= 0.0) .or. &
         (t2%name /= 'test') .or. t2%isSet) error stop 6_4

    if ( (b1_m%id /= 1) .or. (b1_m%value /= 0.0)) error stop 7_4

    if ( (c1_m%id /= 1) .or. (c1_m%value /= 0.0) .or. &
         (c1_m%name /= 'test')) error stop 8_4

    if ( (t1_m%id /= 1) .or. (t1_m%value /= 0.0) .or. &
         (t1_m%name /= 'test') .or. t1_m%isSet) error stop 9_4

end
