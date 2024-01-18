! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent002.f
! %VERIFY: structureComponent002.out:structureComponent002.vf
! %STDIN:
! %STDOUT: structureComponent002.out
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
!*    SOURCE or MOLD is a structure component, which is poly
!*  array. The object containing the component is a scalar.
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
        integer j
    end type

    type, extends(Base) :: Child
        class(Base), pointer :: b2(:,:)
    end type

    type Base1
        integer :: j(2,2)
    end type
end module

program structureComponent002
use m
    type(Base) :: b1(20)
    type(Child) :: c1
    type(Base1) :: b2

    b1 = (/ (Base(i,i*2), i=1,20) /)

    allocate(c1%b2(5,5), SOURCE=reshape(b1, (/5,5/), &
     (/Base(-1,-2),Base(-3,-4)/), (/2,1/)))

    associate(name1=>transfer(c1%b2, b2%j, 20))
        print *, name1
        if(size(name1) .NE. 20) error stop 1_4
    end associate

    associate(name1=>transfer(c1%b2, b2%j))
        print *, name1
        if(size(name1) .NE. 50) error stop 2_4
    end associate
end
