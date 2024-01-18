!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext010.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 07, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extension (component inherited,
!*                               parent's components accessed via short-hand or
!*                               full name in a third generation, all derived
!*                               types defined in a module
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
    type base(k)
        integer, kind :: k
        integer(k) :: id
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type, extends(child) :: thirdGeneration(kl)
        integer, kind :: kl
        logical(kl) :: isSet
    end type

    type (child(4,20)) :: c1_m
    type (thirdGeneration(4,20,2)) :: t1_m
end module

program fext010
    use m

    type (thirdGeneration(4,20,2)) :: t1
    type (child(4,20)) :: c1

    t1%child%base%id = 100
    if (t1%id /= 100) error stop 1_4

    t1%child%id = 10
    if (t1%id /= 10) error stop 2_4

    t1%id = 1
    if (t1%id /= 1) error stop 3_4

    t1%child%name = 'Test child 1'
    if (t1%name /= 'Test child 1') error stop 4_4

    t1%name = 'Test child again'
    if (t1%name /= 'Test child again') error stop 5_4

    t1%isSet = .true.
    if (.not. t1%isSet) error stop 6_4

    c1%id = 2
    c1%name = 'c1'

    if (c1%base%id /= 2) error stop 7_4
    if (c1%name /= 'c1') error stop 8_4
    if (c1%id /= c1%base%id) error stop 9_4

    c1_m%id = 3
    c1_m%name = 'c1_m'

    if (c1_m%base%id /= 3) error stop 10_4
    if (c1_m%name /= 'c1_m') error stop 11_4
    if (c1_m%id /= c1_m%base%id) error stop 12_4

    t1_m%child%base%id = 4
    t1_m%child%name = 't1_m'
    t1_m%isSet = (1<2)

    if (t1_m%id /= 4) error stop 13_4
    if (t1_m%name /= 't1_m') error stop 14_4
    if (.not. t1_m%isSet) error stop 15_4

    if ((t1_m%id /= t1_m%child%id) .or. (t1_m%id /= t1_m%base%id) .or. &
        (t1_m%id /= t1_m%child%base%id)) error stop 16_4

    if (t1_m%child%name /= 't1_m') error stop 17_4
end
