! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent001.f
! %VERIFY: structureComponent001.out:structureComponent001.vf
! %STDIN:
! %STDOUT: structureComponent001.out
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
!*    MATRIX is a structure component, which is non-poly array.
!*  The object containing the component is a scalar.
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
        type(Base) :: b1(4,6)
    end type
end module

program structureComponent001
use m
    type(Child) :: c1

    c1%b1 = reshape((/(Base(i), i=1,20)/), (/4,6/), &
     (/Base(-1),Base(-2)/), (/2,1/))

    associate(name1=>transpose(c1%b1))
        print *, name1
        if(size(name1) .NE. 24) error stop 1_4
        if(ubound(name1, DIM=1) .NE. 6) error stop 2_4
        if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
    end associate
end
