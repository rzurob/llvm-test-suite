!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fext036.f
! %VERIFY: fext036.out:fext036.vf
! %STDIN:
! %STDOUT: fext036.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 11, 2003
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : type extension (componet ordering in
!*                               intrinsic IO)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer*4 :: i1 = 100
        real*4 :: r(2) = (/1.0, 2.0/)
    end type

    type, extends(base) :: child
        character(20) :: c2 = 'Child type'
        logical*2 :: l2(2) = (/.true., .true./)
    end type

    type, extends(child) :: thirdGeneration
        complex(4) :: c3 = (10.0, 5.0)
        real*8 :: r3 = -1.0d0
        integer*8 :: i3 = 10000
    end type

end module

program fext036

use m

    type (child) :: c1
    type (thirdGeneration) :: t1

    t1%c2 = 'third Generation'

    print *, "c1's component ordering"
    print *, c1

    print *, "t1's component ordering"
    print *, t1
end
