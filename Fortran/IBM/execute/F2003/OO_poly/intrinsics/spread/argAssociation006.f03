! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/20/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, unlimited poly, and is array.
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1(10)
        class(*) :: arg2(:,:)
        class(*) :: arg3(:)
        class(*) :: arg4(2,2)
        class(*) :: arg5(:,:)

        select type(name1=>spread(arg1, 2, 2))
            type is (Base)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 3, 2))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select

        select type(name1=>spread(arg3, 2, 3))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 3_4
        end select

        select type(name1=>spread(arg4, 3, 2))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 4_4
        end select

        select type(name1=>spread(arg5, 3, 3))
            type is (Base)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 5_4
        end select
    end subroutine
end module

program argAssociation006
use m
    type(Base) :: b1(10)
    type(Child) :: c1(2,3)
    class(Base), pointer :: b2(:)
    class(Child), allocatable :: c2(:,:)
    class(*), allocatable :: u1(:,:)

    b1 = (/ (Base(i),i=1,10) /)
    c1 = reshape((/(Child(i, i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))
    allocate(u1(3,2), SOURCE=reshape((/(Base(i),i=4,9)/),(/3,2/)))

    call sub1(b1, c1, b2, c2, u1)
end