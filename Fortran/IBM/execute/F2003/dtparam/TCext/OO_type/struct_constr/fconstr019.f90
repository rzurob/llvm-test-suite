! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr019.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr019.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/19/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (components fully default
!*                                  initialized)
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
        integer(k1)       :: id = 0
        real(k2), private :: value = 1.0
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

    type (base(4,4)), save :: b1_m = base(4,4) ()
    type (child(4,4,1,20)), save :: c1_m = child(4,4,1,20) (1)
    type (child(4,4,1,20)), save :: c2_m = child(4,4,1,20) (name = 'c2_m', id = 2)
    type (child(4,4,1,20)), save :: c3_m = child(4,4,1,20) (base = base(4,4)(), name = 'c3_m')

    type (thirdGeneration(4,4,1,20,1)), save :: t1_m = thirdGeneration(4,4,1,20,1)()
    type (thirdGeneration(4,4,1,20,1)), save :: t2_m = thirdGeneration(4,4,1,20,1)(id = 3, isSet=.true.)
    type (thirdGeneration(4,4,1,20,1)), save :: t3_m = thirdGeneration(4,4,1,20,1)(child = child(4,4,1,20) ())

    contains

    logical function validateChildData (c, intVal, charVal)
        type (child(4,4,1,*)), intent(in) :: c
        integer*4, intent(in) :: intVal
        character(*), intent(in) :: charVal

        validateChildData = ((c%id == intVal) .and. (c%name == charVal))
    end function
end module


program fconstr019
use m
    type (thirdGeneration(4,4,1,20,1)) :: t1 = thirdGeneration(4,4,1,20,1)(name = 't1')
    type (thirdGeneration(4,4,1,20,1)) :: t2 = thirdGeneration(4,4,1,20,1)(id = 1, name = 't2')
    type (thirdGeneration(4,4,1,20,1)) :: t3 = thirdGeneration(4,4,1,20,1)(id = 2, name = 't3', isSet = .true.)

    type (child(4,4,1,20)) :: c1 = child(4,4,1,20)()
    type (child(4,4,1,20)) :: c2 = child(4,4,1,20) (name = 'c2')
    type (child(4,4,1,20)) :: c3 = child(4,4,1,20) (base = base(4,4)(10))


    ! validate all data
    if (.not. validateChildData(c1, 0, '')) error stop 1_4

    if (.not. validateChildData(c2, 0, 'c2')) error stop 2_4

    if (.not. validateChildData(c3, 10, '')) error stop 3_4

    if ((.not. validateChildData(t1%child, 0, 't1')) .or. t1%isSet) error stop 4_4

    if ((.not. validateChildData(t2%child, 1, 't2')) .or. t2%isSet) error stop 5_4

    if ((.not. validateChildData(t3%child,2,'t3')) .or. (.not. t3%isSet)) &
            error stop 6_4


    if (b1_m%id /= 0) error stop 7_4

    if (.not. validateChildData(c1_m, 1, '')) error stop 8_4

    if (.not. validateChildData(c2_m, 2, 'c2_m')) error stop 9_4

    if (.not. validateChildData(c3_m, 0, 'c3_m')) error stop 10_4

    if ((.not. validateChildData(t1_m%child, 0, '')) .or. &
        t1_m%isSet) error stop 11_4

    if ((.not. validateChildData(t2_m%child, 3, '')) .or. &
        (.not. t2_m%isSet)) error stop 12_4

    if ((.not. validateChildData(t3_m%child, 0, '')) .or. &
        t3_m%isSet) error stop 13_4



    ! re-assign t2 and t3 using struct_constructors
    t2 = thirdGeneration(4,4,1,20,1) (child = child(4,4,1,20)(base = base(4,4)()))
    t3 = thirdGeneration(4,4,1,20,1) (base = base(4,4)())

    if ((.not. validateChildData(t2%child,0,'')) .or. t2%isSet) error stop 14_4

    if ((.not. validateChildData(t3%child,0,'')) .or. t3%isSet) error stop 15_4
end
