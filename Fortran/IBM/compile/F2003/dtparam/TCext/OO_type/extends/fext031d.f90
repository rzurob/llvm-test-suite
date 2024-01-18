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
! %POSTCMD: dcomp fext031d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 10, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (extends and private on the same
!*                               type definition)
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
    type base(k1)
        integer, kind :: k1
        integer(k1) :: id
    end type

    type, extends(base), private :: child(n)
        integer, len :: n
        character(n) :: name
    end type

    type, extends(child) :: thirdGeneration(k2)
        integer, kind :: k2
        logical(k2) :: isSet
    end type

    type (thirdGeneration(4,20,2)) :: t1_m
    type (child(4,20)) :: c1_m

end module

program fext031d
    use m

    type (thirdGeneration(4,20,2)) :: t1

    t1_m%child%id = 1   !<-- illegal
    t1%child%id = 2     !<-- illegal

    t1%child = c1_m     !<-- illegal
    t1_m%child = c1_m   !<-- illegal
end
