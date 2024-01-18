!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext014.f
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
!*  DESCRIPTION                : derived-type extended (base type has no
!*                               components)
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
    type base
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type (base) :: b1_m
    type (child(20)) :: c1_m
end module

program fext014
    use m

    type, extends(base) :: secondChild(ks)
        integer, kind :: ks
        integer(ks) :: id
    end type

    type (child(20)) :: c1
    type (base) :: b1
    type (secondChild(4)) :: s1

    print *, b1
    print *, b1_m

    c1%name = 'This is a test'
    c1_m%name = 'c1_m'
    s1%id = 100

    if ( c1%name /= 'This is a test') error stop 1_4

    if (s1%id /= 100) error stop 2_4

    if (c1_m%name /= 'c1_m') error stop 3_4
end
