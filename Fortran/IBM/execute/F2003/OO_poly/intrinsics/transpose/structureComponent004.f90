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
!*    MATRIX is a structure component, which is a scalar. The object
!*  containing the component is an array.
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
    type(Child) :: c1(4,5)

    c1%b2 = reshape((/(Base(i),i=1,20)/),(/4,5/))

    print *, transpose(c1%b2)
    if(size(transpose(c1%b2)) .NE. 20) error stop 1_4
    associate(name1=>transpose(c1%b2))
        print *, name1
        if(size(name1) .NE. 20) error stop 2_4
        if(ubound(name1, DIM=1) .NE. 5) error stop 3_4
        if(ubound(name1, DIM=2) .NE. 4) error stop 4_4
    end associate
end
