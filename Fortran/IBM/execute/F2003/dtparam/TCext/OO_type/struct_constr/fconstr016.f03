! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr016.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 14, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1,k2)    ! (4,4)
        integer, kind     :: k1,k2
        integer(k1)       :: id
        real(k2), private :: value = 1.0
    end type

    contains

    logical function isBaseCorrect (b, intVal, realVal)
        type (base(4,4)), intent(in) :: b
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal

        isBaseCorrect = ((b%id == intVal) .and. (b%value == realVal))
    end function
end module

module m1
use m, newBase => base
    type, extends(newBase) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (newBase(4,4)), save :: b1_m = newBase(4,4) (1)
    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (2, name = 'c1_m')
    type (child(4,4,1,20)), save :: c2_m

    contains

    subroutine initializeC2_m
        c2_m = child(4,4,1,20) (newbase = b1_m, name = 'c2_m')
    end subroutine
end module


program fconstr016
use m, anotherBase => base

    type, extends(anotherBase) :: secondChild(k4)    ! (4,4,2)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (anotherBase(4,4)) :: a1 = anotherBase(4,4)(4)
    type (secondChild(4,4,2)) :: s1, s3
    type (secondChild(4,4,2)) :: s2 = secondChild(4,4,2) (id = 5, isSet = .true.)

    s1 = secondChild(4,4,2) (anotherbase = a1, isSet = .true.)
    s3 = secondChild(4,4,2) (anotherbase = anotherBase(4,4) (6), isSet = .true.)

    ! validate all the data
    if (.not. isBaseCorrect (a1, 4, 1.0)) error stop 1_4

    if (.not. isBaseCorrect (s1%anotherbase, 4, 1.0)) error stop 2_4
    if (.not. s1%isSet) error stop 3_4

    if (.not. isBaseCorrect (s2%anotherbase, 5, 1.0)) error stop 4_4
    if (.not. s2%isSet) error stop 5_4

    if (.not. isBaseCorrect (s3%anotherbase, 6, 1.0)) error stop 6_4
    if (.not. s3%isSet) error stop 7_4

    call validateM1Data()
end

subroutine validateM1Data()
use m1

    call initializeC2_m()

    if (.not. isBaseCorrect (b1_m, 1, 1.0)) error stop 20_4

    if (.not. isBaseCorrect (c1_m%newbase, 2, 1.0)) error stop 21_4
    if (c1_m%name /= 'c1_m') error stop 22_4

    if (.not. isBaseCorrect (c2_m%newbase, 1, 1.0)) error stop 23_4
    if (c2_m%name /= 'c2_m') error stop 24_4

end subroutine
