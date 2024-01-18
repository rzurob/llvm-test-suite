!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext004.f
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
!*  DESCRIPTION                : derived-type extended (component inherited,
!                                parent introduced via use, public components)
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
use m
    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type
end module

program fext004
    use m1

    type (base(4)) :: b1
    type (child(4,20)) :: c1

    b1%id = 10

    c1%base%id = 20
    c1%name = 'This is a test'

    if ( c1%base%id /= 20) error stop 1_4
    if ( c1%name /= 'This is a test') error stop 2_4

    if (b1%id /= 10) error stop 3_4
end
