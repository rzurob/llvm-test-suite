! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape003.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape003.f
! %VERIFY: reshape003.out:reshape003.vf
! %STDIN:
! %STDOUT: reshape003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are specified, PAD has different declared type
!*      but same dynamic type as SOURCE
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 99
    end type
end module

program reshape003
use m
    type(Child(4)) :: c1(10)
    type(Child(4)) :: c2(3,5)
    class(Base(4)), allocatable :: b1(:)
    c1 = (/ (Child(4)(i,i+100), i=1,10) /)
    allocate(Child(4)::b1(2))

    c2 = reshape(c1, (/3,5/), b1, (/2,1/))

    print *, c1
    print *, c2
end
