! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape001.f
! %VERIFY: reshape001.out:reshape001.vf
! %STDIN:
! %STDOUT: reshape001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/21/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE or MOLD is function return of reshape.
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
        integer j
    end type
end module

program reshape001
use m
    type(Child) :: c1(5)

    c1 = (/(Child(i,i-1),i=101,105)/)

    associate(name1=>transfer(reshape(c1, (/2,4/), &
     (/Child(-1,-2)/), (/2,1/)), reshape((/(Base(i),i=1,30)/), &
     (/2,3,3/))))
        print *, name1
        if(size(name1) .NE. 16) error stop 1_4
    end associate
end
