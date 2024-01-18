!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fext029d2.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (diagnostic test: private parent
!*                               type results in private parent component)
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
    private
    type base(k)
        integer, kind :: k
        integer(k) :: id
    end type

    type, extends(base), public :: child(n)
        integer, len :: n
        character (n) :: name
    end type
end module

program fext029d2
use m
    type(child(4,20)) :: c1

    c1%id = 10
    c1%base%id = 10     !<-- c1%base is illegal reference

    print *, c1%base    !<-- c1%base is illegal reference
end
