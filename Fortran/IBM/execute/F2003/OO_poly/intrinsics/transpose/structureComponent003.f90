! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent003.f
! %VERIFY: structureComponent003.out:structureComponent003.vf
! %STDIN:
! %STDOUT: structureComponent003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is a structure component, which is unlimited poly
!*  array. The object containing the component is a scalar.
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
        integer j
    end type

    type, extends(Base) :: Child
        class(*), allocatable :: b2(:,:)
    end type
end module

program structureComponent003
use m
    type(Base) :: b1(20)
    type(Child) :: c1

    b1 = (/ (Base(i,i*2), i=1,20) /)

    allocate(c1%b2(4,6), SOURCE=reshape(b1, (/4,6/), &
     (/Base(-1,-2),Base(-3,-4)/), (/2,1/)))

    select type(name1=>transpose(c1%b2))
        type is (Base)
            print *, name1
            if(size(name1) .NE. 24) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 6) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
        class default
            error stop 4_4
    end select
end
