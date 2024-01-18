!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fselTyp503.f
! %VERIFY: fselTyp503.out:fselTyp503.vf
! %STDIN:
! %STDOUT: fselTyp503.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : select type (poly function return as selector)
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
        integer(4) id
    end type

    type, extends(base) :: child
        character (15) :: name
    end type

    interface
        class (base) function genData (id, name)
        import base
            integer(4),intent(in) :: id
            character(*), intent(in), optional :: name
            allocatable genData
        end function
    end interface
end module


program fselType503
use m
    select type (x => genData(100, 'x'))
        type is (base)
            print *, 'base', x
        type is (child)
            print *, 'child', x
    end select
end


class (base) function genData (id, name)
use m, only : base, child
    integer(4),intent(in) :: id
    character(*), intent(in), optional :: name
    allocatable genData

    if (present(name)) then
        allocate (genData, source=child(id,name))
    else
        allocate (genData, source=base(id))
    end if
end function
