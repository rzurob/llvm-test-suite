! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer001.f
! %VERIFY: transfer001.out:transfer001.vf
! %STDIN:
! %STDOUT: transfer001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is function return of transfer.
!*    Non-poly.
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

program transfer001
use m
    type(Base) :: b1(6)

    b1 = (/(Base(i),i=101,106)/)

    associate(name1=>spread(transfer(b1, (/Child(-1,-2)/)), 1, 2))
        if(.NOT. same_type_as(name1, Child(1,1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
