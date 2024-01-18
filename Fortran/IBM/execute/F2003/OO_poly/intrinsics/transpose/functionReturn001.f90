! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn001.f
! %VERIFY: functionReturn001.out:functionReturn001.vf
! %STDIN:
! %STDOUT: functionReturn001.out
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
!*    MATRIX is the return value of an internal function call.
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

program functionReturn001
use m
    class(*), pointer :: b1(:,:) => null()

    print *, transpose(func1())

    select type(name1=>transpose(func2()))
        type is (Child)
            print *, name1
            if(size(name1) .NE. 8) error stop 1_4
            if(ubound(name1, DIM=1) .NE. 2) error stop 2_4
            if(ubound(name1, DIM=2) .NE. 4) error stop 3_4
        class default
            error stop 4_4
    end select

    contains

    function func1()
        type(Base) :: func1(3,4)
        func1 = reshape((/(Base(i), i=1,20)/), (/3,4/))
    end function

    function func2()
        class(Base), pointer :: func2(:,:)
        allocate(func2(4,2), SOURCE=reshape((/(Child(i,i+1),i=1,9)/), &
         (/4,2/)))
    end function
end
