! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation003.f
! %VERIFY: argAssociation003.out:argAssociation003.vf
! %STDIN:
! %STDOUT: argAssociation003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MATRIX is a dummy argument. Dummy argument is non-pointer,
!*  non-allocatable, and unlimited poly.
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
end module

program argAssociation003
use m
    type(Base) :: b1(3,4)
    type(Child) :: c1(2,3)
    class(Base), pointer :: b2(:,:)
    class(Child), allocatable :: c2(:,:)
    class(*), allocatable :: u1(:,:)

    b1 = reshape((/(Base(i),i=1,12)/), (/3,4/))
    c1 = reshape((/(Child(i,i-1),i=2,7)/), (/2,3/))

    allocate(b2(4,2), SOURCE=reshape((/(Child(i,i+1),i=2,9)/), (/4,2/)))
    allocate(c2(3,3), SOURCE=reshape((/(Child(i,i),i=-9,-1)/), (/3,3/)))
    allocate(u1(2,4), SOURCE=reshape((/(Base(i),i=3,10)/), (/2,4/)))

    call sub1(b1, c1, b2, c2, u1)

    contains

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1(3, 4)
        class(*) :: arg2(:,:)
        class(*) :: arg3(4, 2)
        class(*) :: arg4(:,:)
        class(*) :: arg5(:,:)

        select type(name1=>transpose(arg1))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        select type(name1=>transpose(arg2))
            type is (Child)
                print *, name1
                if(ubound(name1, DIM=1) .NE. 3) error stop 2_4
                if(ubound(name1, DIM=2) .NE. 2) error stop 3_4
            class default
                error stop 4_4
        end select

        select type(name1=>transpose(arg3))
            type is (Child)
                print *, name1
            class default
                error stop 5_4
        end select

        select type(name1=>transpose(arg4))
            type is (Child)
                print *, name1
                if(ubound(name1, DIM=1) .NE. 3) error stop 6_4
                if(ubound(name1, DIM=2) .NE. 3) error stop 7_4
            class default
                error stop 8_4
        end select

        select type(name1=>transpose(arg5))
            type is (Base)
                print *, name1
                if(ubound(name1, DIM=1) .NE. 4) error stop 9_4
                if(ubound(name1, DIM=2) .NE. 2) error stop 10_4
            class default
                error stop 11_4
        end select
    end subroutine
end
