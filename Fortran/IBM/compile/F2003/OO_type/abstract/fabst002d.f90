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
! %POSTCMD: tcomp fabst002d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ABSTRACT (C611, if appearing at the right-most
!                               part in a data-ref, it must not be polymorphic
!                               for abstract type)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
    end type
end module

use m
    type (child), target :: c1

    class (base), pointer :: b

    print *, c1%base%id     !<-- this is legal
    b => c1%base            !<-- this is illegal
end
