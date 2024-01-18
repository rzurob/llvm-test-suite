! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: associate002.f
! %VERIFY: associate002.out:associate002.vf
! %STDIN:
! %STDOUT: associate002.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is an associate name. Unlimited
!*    poly.
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
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program associate002
use m
    class(*), pointer :: ap1(:,:) => null()
    class(*), allocatable :: ap2(:,:)
    class(*), allocatable :: p(:)

    allocate(Child::p(2))

    allocate(ap1(4,4), SOURCE= &
     reshape((/(Child(i,i+1),i=1,20)/),(/4,4/)))

    associate(name1=>ap1)
        allocate(ap2(3,5), SOURCE=reshape(name1, (/3,5/), p, (/2,1/)))
    end associate

    select type (ap1)
        type is (Child)
            print *, ap1
        class default
            error stop 1_4
    end select

    select type (ap2)
        type is (Child)
            print *, ap2
        class default
            error stop 2_4
    end select
end
