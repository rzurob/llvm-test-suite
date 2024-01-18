! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
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
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a structure component, which is poly array.
!*  The object containing the component is a scalar.
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
        class(Base), pointer :: b1(:)
        class(Base1), allocatable :: b2(:,:)
    end type
end module

program structureComponent002
use m
    type(Child) :: c1

    allocate(c1%b1(10), SOURCE=(/(Base(i),i=1,10)/))

    allocate(c1%b2(5,5), SOURCE=reshape((/(Base1(i,-i),i=1,20)/), &
     (/5,5/), (/Base1(-1,-2),Base1(-3,-4)/), (/2,1/)))

    select type(name1=>spread(c1%b1, 2, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(c1%b2, 3, 2))
        type is (Base1)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
