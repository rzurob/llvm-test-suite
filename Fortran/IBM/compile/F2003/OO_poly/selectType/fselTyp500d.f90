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
! %POSTCMD: tcomp fselTyp500d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type construct (abstract type can not in
!                               type-is type-guard)
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
    type, abstract :: base
    end type

    type, extends(base) :: child
        integer id
    end type

end module

program fselTyp500d
use m
    class (*), pointer :: x

    type (child), target :: c1

    x => c1

    select type (x)
        type is (base)       !<-- this is illegal
!            print *, 'bad'

        class is (base)       !<-- this ougtht to be legal, see defect 293988
            print *, 'good'
    end select
end


