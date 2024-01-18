! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape018.f
! %VERIFY: reshape018.out:reshape018.vf
! %STDIN:
! %STDOUT: reshape018.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
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
        integer :: i = 88
    end type

    type, extends(Base) :: Child
        integer :: j = 99
    end type
end module

program reshape018
use m
    class(AbstractParent), pointer :: b1(:) => null()
    class(*), allocatable :: c1(:,:)
    class(AbstractParent), allocatable :: b2(:)

    allocate(b1(10), SOURCE = (/ (Base(i), i=1,10) /))
    allocate(b2(2), SOURCE = (/Base(-1),Base(-2)/))
    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/), b2, (/2,1/)))

    select type (b1)
        type is (Base)
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Base)
            print *, c1
        class default
            error stop 2_4
    end select
end
