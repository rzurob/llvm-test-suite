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
! %POSTCMD: tcomp fabst002d1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ABSTRACT (C401, abstract type can not appear in
!                               type is type-guard statement but is legal for
!                               class-is type-guard)
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
    type, abstract :: base(k1)
        integer, kind :: k1
    end type

    type, extends(base) :: child(k2)
        integer, kind :: k2
        integer(k2) id
    end type
end module


program fabst002d1
use m
    class (*), pointer :: x

    type (child(4,4)), target :: c1

    x => c1

    select type (x)
        type is (base)      !<-- this is illegal

        class is (base(4))     !<-- this is legal

            print *, 'good'
    end select
end

