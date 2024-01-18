!#######################################################################
! *********************************************************************
! %START
! %MAIN:
! %PRECMD:
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fext002d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/08/2004
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : derived-type extended (component inherited,
!                                private components)
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
        integer(k), private :: id = 10
    end type

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type

end module

program fext002d
    use m
    type, extends(base) :: secondChild(n)
        integer, len :: n
        character(n) :: name
    end type

    type (secondChild(4,20)) :: c2

    c2%name = 'c2'

    print *, c2     !<-- illegal as c2 has inaccessible component
end
