! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr012a.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (parent component
!*                               initialization via an object)
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
        integer, kind :: k1,k2
        integer(k1)   :: id = 1
        real(k2)      :: value = 0.0
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name = ''
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet = .false.
    end type

    type (base(4,4)), save :: b1_m = base(4,4) (10)
    type (child(4,4,1,20)), save :: c1_m
    type (thirdGeneration(4,4,1,20,1)), save :: t1_m

    contains

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type (child(4,4,1,*)), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id == intVal) .and. (c%value == realVal) &
                    .and. (c%name == charVal))
    end function

    subroutine initializeModuleData
        c1_m = child(4,4,1,20) (base = b1_m, name = 'c1_m')
        t1_m = thirdGeneration(4,4,1,20,1) (child = child(4,4,1,20) (name = 't1_m', base=b1_m))
    end subroutine

end module

program fconstr012a
use m

    type (base(4,4)) :: b1 = base(4,4) (1, 1.0)
    type (child(4,4,1,20)) :: c1, c2, c3
    type (thirdGeneration(4,4,1,20,1)) :: t1, t2

    c1 = child(4,4,1,20)(base = b1)
    c2 = child(4,4,1,20) (base = c1%base, name = 'test')

    c3 = child(4,4,1,20) (b1%id, b1%value, c2%name)

    t1 = thirdGeneration(4,4,1,20,1) (child = c2, isSet = .true.)
    t2 = thirdGeneration(4,4,1,20,1) (base = c1%base, name ='t2')

    if (.not. isChildCorrect (c1, 1, 1.0, '')) error stop 1_4

    if (.not. isChildCorrect (c2, 1, 1.0, 'test')) error stop 2_4

    if (.not. isChildCorrect (c3, 1, 1.0, 'test')) error stop 3_4

    if (.not. isChildCorrect (t1%child, 1, 1.0, 'test')) error stop 4_4
    if (.not. t1%isSet) error stop 5_4

    if (.not. isChildCorrect (t2%child, 1, 1.0, 't2')) error stop 6_4
    if (t2%isSet) error stop 7_4


    call initializeModuleData

    if (.not. isChildCorrect (c1_m, 10, 0.0, 'c1_m')) error stop 8_4

    if (.not. isChildCorrect (t1_m%child, 10, 0.0, 't1_m')) error stop 9_4

    if (t1_m%isSet) error stop 10_4
end
