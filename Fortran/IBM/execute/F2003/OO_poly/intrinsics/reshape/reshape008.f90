! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape008.f
! %VERIFY: reshape008.out:reshape008.vf
! %STDIN:
! %STDOUT: reshape008.out
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
!*    Assigned data entity is unlimited poly
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

    type, extends(Base) :: Child
        integer j
    end type
end module

program reshape008
use m
    type(Child) :: c1(10)
    class(*), allocatable :: b2(:,:)

    c1 = (/ (Child(i,i+100), i=1,10) /)

    allocate(b2(3,5), SOURCE = reshape(c1, (/3,5/), &
     (/Child(-1,1),Child(-2,2)/),(/2,1/)))

    print *, c1
    select type (b2)
        type is (Child)
            print *, b2
        class default
            error stop 1_4
    end select
end
