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
!*  DATE                       : 12/31/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is function return of reshape.
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

    print *, transpose(reshape(c1,(/2,4/),(/Child(-1,-2)/),(/2,1/)))

    associate(name1=>transpose(reshape(c1, (/2,4/), &
     (/Child(-1,-2)/), (/2,1/))))
        print *, name1
        if(size(name1) .NE. 8) error stop 1_4

        associate(name2=>shape(name1))
            if(size(name2) .NE. 2) error stop 2_4
            if(name2(1) .NE. 4) error stop 3_4
            if(name2(2) .NE. 2) error stop 4_4
        end associate
    end associate
end
