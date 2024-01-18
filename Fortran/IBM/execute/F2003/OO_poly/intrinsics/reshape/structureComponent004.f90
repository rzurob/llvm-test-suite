! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent004.f
! %VERIFY: structureComponent004.out:structureComponent004.vf
! %STDIN:
! %STDOUT: structureComponent004.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/06/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is a scalar. The object containing the component is an array.
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
        type(Base) :: b2
    end type
end module

program structureComponent004
use m
    class(Base), allocatable :: b0(:,:,:)
    type(Child) :: c1(4,5)

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    c1%b2 = reshape((/(Base(i),i=1,20)/),(/4,5/))

    allocate(b0(3,2,4), SOURCE=reshape(c1%b2, (/3,2,4/), &
     (/Base(-1),Base(-2)/), (/3,2,1/)))

    print *, c1

    select type (b0)
        type is (Base)
            print *, b0
        class default
            error stop 1_4
    end select
end
