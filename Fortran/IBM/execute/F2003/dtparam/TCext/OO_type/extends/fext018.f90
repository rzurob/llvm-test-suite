!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext018.f
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
!*  DESCRIPTION                : derived-type extension (base type is renamed
!*                               via use association. Test base type name)
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
end module

module m1
use m, newBase => base

    type, extends(newBase) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (child(4,20)) :: c1_m
    type (newBase(4)) :: b1_m

end module

program fext018
    use m1

    type (newBase(4)) :: b1
    type (child(4,20)) :: c1

    b1%id = 10
    b1_m%id = 15

    c1%newbase%id = 20
    c1%name = 'Test1'

    c1_m%id = 25
    c1_m%name = 'c1_m'


    if (b1%id /= 10) error stop 1_4
    if (c1%id /= 20) error stop 2_4
    if (c1%name /= 'Test1') error stop 3_4

    c1%id = 100
    if (c1%newbase%id /= 100) error stop 4_4

    if (b1_m%id /= 15) error stop 5_4
    if (c1_m%name /= 'c1_m') error stop 6_4
    if (c1_m%id /= 25) error stop 7_4
    if (c1_m%id /= c1_m%newbase%id) error stop 8_4
end
