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
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is non-poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
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
end module

program reshape002
use m
    type(Base) :: b1(10)
    type(Base) :: b2(3,5)
    b1 = (/ (Base(i), i=1,10) /)
    b2(:,1) = (/ Base(21),Base(22),Base(23) /)
    b2(:,2) = (/ Base(24),Base(25),Base(26) /)
    b2(:,3) = (/ Base(27),Base(28),Base(29) /)
    b2(:,4) = (/ Base(30),Base(31),Base(32) /)
    b2(:,5) = (/ Base(33),Base(34),Base(35) /)

    b2 = reshape(b1, (/3,5/), (/Base(0),Base(1)/), (/2,1/))

    print *, b1
    print *, b2
end
