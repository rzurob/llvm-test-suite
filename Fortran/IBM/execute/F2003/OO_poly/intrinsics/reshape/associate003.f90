! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate003.f
! %VERIFY: associate003.out:associate003.vf
! %STDIN:
! %STDOUT: associate003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 10/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is an associate name. Selector
!*    is array section.
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
        integer j
    end type
end module

program associate003
use m
    class(AbstractParent), pointer :: ap1(:,:) => null()
    class(AbstractParent), allocatable :: ap2(:,:,:)
    class(AbstractParent), allocatable :: p(:)

    allocate(p(3), SOURCE=(/Base(-1),Base(-2),Base(-3)/))

    allocate(ap1(5,6), SOURCE=reshape((/(Base(i),i=1,40)/), (/5,6/)))

    associate(name1=>ap1(3:,1:6:2))
        allocate(ap2(2,3,2), SOURCE= &
         reshape(name1, (/2,3,2/), p, (/1,2,3/)))
    end associate

    select type (ap2)
        type is (Base)
            print *, ap2
        class default
            error stop 1_4
    end select
end
