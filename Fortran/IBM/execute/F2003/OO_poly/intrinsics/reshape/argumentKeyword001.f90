! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argumentKeyword001.f
! %VERIFY: argumentKeyword001.out:argumentKeyword001.vf
! %STDIN:
! %STDOUT: argumentKeyword001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                : Actual arguments are specified using
!*    argument keywords.
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
        character(10) :: c
    end type
end module

program argumentKeyword001
use m
    type(Base) :: b1(10)
    type(Base) :: b2(2,4)
    type(Base) :: b3(3,4)
    type(Base) :: b4(3,5)

    b1 = (/ (Base(i), i=1,10) /)

    b2 = reshape(b1, ORDER=(/2,1/), PAD=(/Base(-1),Base(-2)/), SHAPE=(/2,4/))

    b3 = reshape(PAD=(/Base(-1),Base(-2)/), ORDER=(/2,1/), &
     SHAPE=(/3,4/), SOURCE=b1)

    b4 = reshape(SOURCE=b1, SHAPE=(/3,5/), &
     PAD=(/Base(-1),Base(-2)/), ORDER=(/2,1/))

    print *, b1
    print *, b2
    print *, b3
    print *, b4
end
