!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext031.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 10, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (extends and private on the same
!*                               type definition)
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
    type, extends(child) :: thirdGeneration
        logical*2 :: isSet
    end type

    type, extends(base), private :: child
        character(20) :: name
    end type

    type base
        integer*4 :: id
    end type

    type (child) :: c1_m
    type (thirdGeneration) :: t1_m
end module

program fext031
    use m

    type (thirdGeneration) :: t1

    c1_m%id = 1
    c1_m%name = 'c1_m'

    t1_m%id = 2
    t1_m%name = 't1_m'
    t1_m%isSet = (1_4 == 1_2)

    t1%id = 3
    t1%name = 't1'
    t1%isSet = (t1%id == (c1_m%id+t1_m%id))

    !validate all the variables

    if (c1_m%id /= 1) error stop 1_4
    if (c1_m%name /= 'c1_m') error stop 2_4
    if (c1_m%id /= c1_m%base%id) error stop 3_4

    if (t1_m%id /= 2) error stop 4_4
    if (t1_m%name /= 't1_m') error stop 5_4
    if (.not. t1_m%isSet) error stop 6_4

    if (t1_m%id /= t1_m%base%id) error stop 7_4

    if (t1%id /= 3) error stop 8_4
    if (t1%name /= 't1') error stop 9_4
    if (.not. t1%isSet) error stop 10_4

    if (t1%id /= t1%base%id) error stop 11_4

end
