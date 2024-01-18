! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent003.f
! %VERIFY: structureComponent003.out:structureComponent003.vf
! %STDIN:
! %STDOUT: structureComponent003.out
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
!*    SOURCE is a structure component, which is unlimited poly
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
        class(*), pointer :: b1
        class(*), pointer :: b2(:)
        class(*), allocatable :: b3(:,:)
    end type
end module

program structureComponent003
use m
    type(Child) :: c1

    allocate(c1%b1, SOURCE=Base(8))

    allocate(c1%b2(10), SOURCE=(/(Base(i),i=1,10)/))

    allocate(c1%b3(5,5), SOURCE=reshape((/(Base1(i,-i),i=1,20)/), &
     (/5,5/), (/Base1(-1,-2),Base1(-3,-4)/), (/2,1/)))

    select type(name1=>spread(c1%b1, 1, 10))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(c1%b2, 2, 2))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select

    select type(name1=>spread(c1%b3, 3, 2))
        type is (Base1)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select
end
