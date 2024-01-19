! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is a dummy argument. Dummy argument is
!*  non-pointer, non-allocatable, poly, and is array.
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
        class(*) :: arg4(2,3)
        class(*) :: arg5(2,3)
        logical :: m1(10)
        m1 = (/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE., &
         .FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./)

        select type(name1=>merge(arg1, arg3, m1))
            type is (Base)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>merge(arg2, arg4, reshape(m1,(/2,3/))))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select

        select type(name1=>merge(arg4, arg5, reshape(m1,(/2,3/))))
            type is (Child)
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 3_4
        end select
    end subroutine
end module

program argAssociation003
use m
    type(Base) :: b1(10)
    type(Child) :: c1(2,3)
    class(Base), pointer :: b2(:)
    class(Child), allocatable :: c2(:,:)
    class(*), pointer :: u1(:,:)

    b1 = (/ (Base(i),i=1,10) /)
    c1 = reshape((/(Child(i,i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(10), SOURCE=(/(Base(i),i=2,11)/))
    allocate(c2(2,3), SOURCE=reshape((/(Child(j=i-1,i=i), &
     i=12,17)/), (/2,3/)))
    allocate(u1(2,3), SOURCE=reshape((/(Child(i,-i), &
     i=1,6)/), (/2,3/)))

    call sub1(b1, c1, b2, c2, u1)
end
