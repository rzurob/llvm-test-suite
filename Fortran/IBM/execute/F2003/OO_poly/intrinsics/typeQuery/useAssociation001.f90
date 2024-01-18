! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: useAssociation001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/27/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DESCRIPTION                : Type renaming in use statement.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

module n
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program useAssociation001
use m, myBase => Base, myChild => Child
use m, only : Base
use n, only : Child
    type(myChild) :: arg1
    class(myBase), allocatable :: mold1
    class(Base), pointer :: mold2 => null()
    type(Base) :: mold3
    type(Child) :: arg2

    if(.NOT. extends_type_of(arg1, mold1)) error stop 1_4
    if(.NOT. extends_type_of(arg1, mold2)) error stop 2_4
    if(extends_type_of(arg2, mold3)) error stop 3_4

    if(same_type_as(arg1, mold1)) error stop 4_4
    if(.NOT. same_type_as(mold1, mold2)) error stop 5_4
    if(same_type_as(arg2, mold3)) error stop 6_4
end
