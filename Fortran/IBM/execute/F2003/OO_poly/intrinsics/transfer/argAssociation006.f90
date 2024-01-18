! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation006.f
! %VERIFY: argAssociation006.out:argAssociation006.vf
! %STDIN:
! %STDOUT: argAssociation006.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/29/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE or MOLD of TRANSFER is a dummy argument. Dummy argument
!*  is non-pointer, non-allocatable, unlimited poly, and is array.
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

    subroutine sub1(arg1, arg2, arg3, arg4, arg5)
        class(*) :: arg1(10)
        class(*) :: arg2(:,:)
        class(*) :: arg3(:)
        class(*) :: arg4(2,2)
        class(*) :: arg5(:,:)

        select type (name1=>transfer(arg2, arg1, 8))
            type is (Base)
                print *, name1
            class default
                error stop 1_4
        end select

        select type (name1=>transfer(arg3, arg5))
            type is (Base1)
                print *, name1
            class default
                error stop 2_4
        end select

        select type (name1=>transfer(arg5, arg4, 10))
            type is (Child)
                print *, name1
            class default
                error stop 3_4
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
    allocate(u1(3,2), SOURCE=reshape((/(Base1(i,Base(i),i+1,i+1), &
     i=4,9)/), (/3,2/)))

    call sub1(b1, c1, b2, c2, u1)
end
