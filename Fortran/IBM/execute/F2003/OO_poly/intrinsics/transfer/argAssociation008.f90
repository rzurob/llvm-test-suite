! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argAssociation008.f
! %VERIFY: argAssociation008.out:argAssociation008.vf
! %STDIN:
! %STDOUT: argAssociation008.out
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
!*  is a pointer or allocatable, non-poly, and is array.
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
        integer j
    end type

    type Base1
        integer j
        type(Base) :: k
        integer m
    end type
end module

program argAssociation008
use m
    type(Base), pointer :: b(:)
    type(Base1), allocatable :: b1(:,:)

    allocate(b(10), SOURCE=(/(Base(i,i+1),i=1,10)/))
    allocate(b1(2,3), SOURCE=reshape((/(Base1(i, Base(i+1,i+2), &
     i+3),i=3,8)/), (/2,3/), (/Base1(1,Base(1,1),1)/), (/2,1/)))

    call sub1(b, b1)

    contains

    subroutine sub1(arg1, arg2)
        type(Base), pointer :: arg1(:)
        type(Base1), allocatable :: arg2(:,:)

        print *, transfer(arg1, arg2, 2)
        print *, transfer(arg2, arg1, 8)
    end subroutine
end
