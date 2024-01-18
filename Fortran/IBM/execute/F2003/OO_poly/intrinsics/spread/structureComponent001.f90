! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent001.f
! %VERIFY: structureComponent001.out:structureComponent001.vf
! %STDIN:
! %STDOUT: structureComponent001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a structure component, which is non-poly
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
    end type

    type Base1
        integer m
        integer n
    end type

    type, extends(Base) :: Child
        type(Base) :: b1(20)
        type(Base1) :: b2(5,5)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1

    c1%b1 = (/ (Base(i), i=1,20) /)
    c1%b2 = reshape((/(Base1(i,-i),i=1,20)/), (/5,5/), &
     (/Base1(88,99)/), (/2,1/))

    associate(name1=>spread(c1%b1, 2, 2))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>spread(c1%b2, 3, 2))
        if(.NOT. same_type_as(name1, Base1(1,1))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
