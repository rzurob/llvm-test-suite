! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape002.f
! %VERIFY: reshape002.out:reshape002.vf
! %STDIN:
! %STDOUT: reshape002.out
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
!*    Function return of transfer is the SOURCE of reshape.
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

program reshape002
use m
    type(Child) :: c1(5)

    c1 = (/(Child(i,i-1),i=101,105)/)

    print *, reshape(transfer(c1, Base(1), 10), (/2,6/), &
     (/Base(-8)/), (/1,2/))
    print *, size(reshape(transfer(c1, Base(1), 10), (/2,6/), &
     (/Base(-8)/), (/1,2/)))

    associate(name1=>reshape(transfer(c1, Base(1), 10), (/2,6/), &
     (/Base(-8)/), (/1,2/)))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        if(size(name1) .NE. 12) error stop 2_4
    end associate
end
