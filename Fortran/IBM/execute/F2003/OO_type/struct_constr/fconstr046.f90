!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr046.f
! %VERIFY: fconstr046.out:fconstr046.vf
! %STDIN:
! %STDOUT: fconstr046.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (structure constructor
!*                               used as actual argument)
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
        integer*4, pointer :: i1 => null()
        integer*4 :: id = 0
        character*20 :: name = ''
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        if (associated (b%i1)) then
            print *, b%i1, b%id, b%name
        else
            print *, 'null', b%id, b%name
        end if
    end subroutine
end module

program fconstr046
use m
    integer*4, target :: i1 = 10

    call printBase (base ())

    call printBase (base(name = 'temp', id = 10, i1 = i1))

    call printBase (base(null()))
end
