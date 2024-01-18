!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext009.f
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
!*                               full name in a third generation in main program
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

end module

program fext009
    use m
    type, extends(child) :: thirdGeneration(kl)
        integer, kind :: kl = 2
        logical(kl) :: isSet
    end type

    type (thirdGeneration(4,20)) :: t1

    t1%child%base%id = 100
    if (t1%id /= 100) error stop 1_4

    t1%child%id = 10
    if (t1%id /= 10) error stop 2_4

    t1%id = 1
    if (t1%id /= 1) error stop 3_4

    t1%base%id = 1000
    if (t1%id /= 1000) error stop 7_4

    t1%child%name = 'Test child 1'
    if (t1%name /= 'Test child 1') error stop 4_4

    t1%name = 'Test child again'
    if (t1%name /= 'Test child again') error stop 5_4

    t1%isSet = .true.
    if (.not. t1%isSet) error stop 6_4
end
