! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/intrinsics/spread/argAssociation006.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1(10)
        class(*) :: arg2(:,:)
        class(*) :: arg3(:)
        class(*) :: arg4(2,2)
        class(*) :: arg5(:,:)

        select type(name1=>spread(arg1, 2, 2))
            type is (Base(*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 1_4
        end select

        select type(name1=>spread(arg2, 3, 2))
            type is (Child(*,4,*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 2_4
        end select

        select type(name1=>spread(arg3, 2, 3))
            type is (Child(*,4,*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 3_4
        end select

        select type(name1=>spread(arg4, 3, 2))
            type is (Child(*,4,*,4))
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            class default
                error stop 4_4
        end select

        select type(name1=>spread(arg5, 3, 3))
            type is (Base(*,4))
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
    type(Base(20,4)) :: b1(10)
    type(Child(20,4,20,4)) :: c1(2,3)
    class(Base(:,4)), pointer :: b2(:)
    class(Child(:,4,:,4)), allocatable :: c2(:,:)
    class(*), allocatable :: u1(:,:)

    b1 = (/ (Base(20,4)(i),i=1,10) /)
    c1 = reshape((/(Child(20,4,20,4)(i, i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(20,4,20,4)(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(20,4,20,4)(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))
    allocate(u1(3,2), SOURCE=reshape((/(Base(20,4)(i),i=4,9)/),(/3,2/)))

    call sub1(b1, c1, b2, c2, u1)
end
