! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr024.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr024.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (private parent type)
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
    type, private :: base(k1,k2)    ! (4,4)
        integer, kind :: k1,k2
        integer(k1)   :: id
        real(k2)      :: value
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type, extends(child) :: thirdGeneration(k4)    ! (4,4,1,20,1)
        integer, kind :: k4
        logical(k4)   :: isSet
    end type

    type (base(4,4)) :: b1_m = base(4,4) (1, 1.0)
    type (child(4,4,1,20)) :: c1_m = child(4,4,1,20) (base = base(4,4)(2, 2.0), name = 'c1_m')

    type (thirdGeneration(4,4,1,20,1)) :: t1_m = thirdGeneration(4,4,1,20,1) ( &
                isSet = .true., base = base(4,4) (3, 3.0), name = 't1_m')

    contains

    logical function isChildCorrect (c, intVal, realVal, charVal)
        type (child(4,4,1,*)), intent(in) :: c
        integer*4, intent(in) :: intVal
        real*4, intent(in) :: realVal
        character(*), intent(in) :: charVal

        isChildCorrect = ((c%id == intVal) .and. (c%value == realVal) &
                    .and. (c%name == charVal))
    end function
end module


program fconstr024
use m

    ! in the main program base type is inaccessible, parent component base is
    ! inaccessible; but id and value are

    type (child(4,4,1,20)) :: c1 = child(4,4,1,20) (2, 2.0, 'c1')

    type (child(4,4,1,20)) :: c2 = child(4,4,1,20) (value = 3.0, name = 'c2', id = 3)

    type (thirdGeneration(4,4,1,20,1)) :: t1 = thirdGeneration(4,4,1,20,1) (4, 4, 't1', .true.)

    type (thirdGeneration(4,4,1,20,1)) :: t2 = thirdGeneration(4,4,1,20,1) (isSet = .true., &
                    child = child(4,4,1,20) (5, 5.0, 't2'))

    if (.not. isChildCorrect (c1_m, 2, 2.0, 'c1_m')) error stop 1_4

    if (.not. isChildCorrect (t1_m%child, 3, 3.0, 't1_m')) error stop 2_4
    if (.not. t1_m%isSet) error stop 3_4

    if (.not. isChildCorrect (c1, 2, 2.0, 'c1')) error stop 4_4

    if (.not. isChildCorrect (c2, 3, 3.0, 'c2')) error stop 5_4

    if (.not. isChildCorrect (t1%child, 4, 4.0, 't1')) error stop 6_4
    if (.not. t1%isSet) error stop 7_4

    if (.not. isChildCorrect (t2%child, 5, 5.0, 't2')) error stop 8_4
    if (.not. t2%isSet) error stop 9_4
end
