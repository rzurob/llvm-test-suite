! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent004.f
! %VERIFY: structureComponent004.out:structureComponent004.vf
! %STDIN:
! %STDOUT: structureComponent004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is a structure component, which is a scalar.
!*  The object containing the component is an array.
!*
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        type(Base) :: b2
    end type
end module

program structureComponent004
use m
    type(Child) :: c1(4,5)

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    c1%b2 = reshape((/(Base(i),i=1,20)/),(/4,5/))

    print *, transfer(c1%b2, c1)
    if(size(transfer(c1%b2, c1)) .NE. 10) error stop 1_4
    associate(name1=>transfer(c1%b2, c1))
        print *, name1
        if(size(name1) .NE. 10) error stop 2_4
    end associate
end
