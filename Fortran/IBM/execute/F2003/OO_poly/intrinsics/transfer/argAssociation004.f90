! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation004.f
! %VERIFY: argAssociation004.out:argAssociation004.vf
! %STDIN:
! %STDOUT: argAssociation004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, poly, and is array.
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

    type Base1
        integer j
        type(Base) :: k
        integer m
        integer n
    end type

    contains

    subroutine sub1(arg1, arg2, arg3, arg4)
        class(Base) :: arg1(10)
        class(Base) :: arg2(:,:)
        class(Base) :: arg3(:)
        class(Base) :: arg4(2,2)

        select type (name1=>transfer(arg1, arg3))
            type is (Child)
                print *, name1
            class default
                error stop 1_4
        end select

        associate(name1=>transfer(arg2, Base(1), 8))
            print *, name1
            if(.NOT. same_type_as(name1, Base(1))) error stop 2_4
        end associate

        associate(name1=>transfer(arg4, (/Base1(1,Base(1),1,1)/)))
            print *, name1
            if(.NOT. same_type_as(name1, Base1(1,Base(1),1,1))) &
             error stop 3_4
        end associate
    end subroutine
end module

program argAssociation004
use m
    type(Base) :: b1(10)
    type(Child) :: c1(2,3)
    class(Base), pointer :: b2(:)
    class(Child), allocatable :: c2(:,:)

    b1 = (/ (Base(i),i=1,10) /)
    c1 = reshape((/(Child(i, i+1),i=5,15,2)/),(/2,3/))
    allocate(b2(6), SOURCE=(/(Child(i,i+1),i=2,7)/))
    allocate(c2(2,2), SOURCE=reshape((/(Child(j=i-1,i=i), &
     i=12,15)/), (/2,2/)))

    call sub1(b1, c1, b2, c2)
end
