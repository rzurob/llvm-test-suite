! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/intrinsics/spread/transpose002.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transpose002.f
! %VERIFY: transpose002.out:transpose002.vf
! %STDIN:
! %STDOUT: transpose002.out
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
!*    Function return of spread is the SOURCE of transpose.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program transpose002
use m
    type(Base(4)) :: b1(5)

    b1 = (/(Base(4)(i),i=1,5)/)

    associate(name1=>transpose(spread(b1,1,3)))
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
