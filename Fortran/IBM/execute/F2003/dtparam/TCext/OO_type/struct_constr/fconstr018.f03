! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr018.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (keyword usage in the
!*                               component-spec; parent component used)
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
    type base(k1,k2)    ! (4,4)
        integer, kind     :: k1,k2
        integer(k1)       :: id
        real(k2), private :: value = 1.0
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,2)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (base(4,4)), save :: b1_m = base(4,4) (10, 11.0)

    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (name = 'c1_m', base = base(4,4)(20, 5.0))

    type (thirdGeneration(4,4,1,20,2)), save :: t1_m = thirdGeneration(4,4,1,20,2) (&
          isSet = .true., base = base(4,4)(10, 12.0), name = 't1_m')


    type (thirdGeneration(4,4,1,20,2)), save :: t2_m = thirdGeneration(4,4,1,20,2) (&
            child = child(4,4,1,20) (name = 't2_m', base = base(4,4) (value = 0.0, &
            id = 40)), isSet = ('xlf'//'test' == 'xlftest'))

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base(4,4)), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ((b%id == intVal) .and. (b%value == realVal))
    end function

    subroutine updateT1_m
        t1_m = thirdGeneration(4,4,1,20,2) (child = c1_m, isSet = .true.)
    end subroutine
end module

program fconstr018
use m

    type (thirdGeneration(4,4,1,20,2)) :: t1 = thirdGeneration(4,4,1,20,2) (name = 't1', &
                    base = base(4,4) (2), isSet = .true.)

    type (thirdGeneration(4,4,1,20,2)) :: t2, t3, t4

    t2 = thirdGeneration(4,4,1,20,2) (isSet = .true., base = b1_m, name = 't2')
    t3 = thirdGeneration(4,4,1,20,2) (isSet = .true., child = c1_m)

    t4 = thirdGeneration(4,4,1,20,2) (child = child(4,4,1,20) (base = base(4,4)(3), name = 't4'), &
                          isSet = .true.)

    ! validate all data
    if (.not. isBaseCorrect (t1%base, 2, 1.0)) error stop 1_4
    if ((.not. t1%isSet) .or. (t1%name /= 't1')) error stop 2_4

    if (.not. isBaseCorrect (t2%base, 10, 11.0)) error stop 3_4
    if ((.not. t2%isSet) .or. (t2%name /= 't2')) error stop 4_4

    if (.not. isBaseCorrect (t3%base, 20, 5.0)) error stop 5_4
    if ((.not. t3%isSet) .or. (t3%name /= 'c1_m')) error stop 6_4

    if (.not. isBaseCorrect (t4%base, 3, 1.0)) error stop 7_4
    if ((.not. t4%isSet) .or. (t4%name /= 't4')) error stop 8_4

    if (.not. isBaseCorrect (t1_m%base, 10, 12.0)) error stop 9_4
    if ((.not. t1_m%isSet) .or. (t1_m%name /= 't1_m')) error stop 10_4

    call updateT1_m

    if (.not. isBaseCorrect (t1_m%base, 20, 5.0)) error stop 11_4
    if ((.not. t1_m%isSet) .or. (t1_m%name /= 'c1_m')) error stop 12_4

    if (.not. isBaseCorrect (t2_m%base, 40, 0.0)) error stop 13_4
    if ((.not. t2_m%isSet) .or. (t2_m%name /= 't2_m')) error stop 14_4

end
