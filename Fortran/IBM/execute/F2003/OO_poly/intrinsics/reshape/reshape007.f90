! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape007.f
! %VERIFY: reshape007.out:reshape007.vf
! %STDIN:
! %STDOUT: reshape007.out
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
!*    PAD and ORDER are not specified
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

program reshape007
use m
    type(Child) :: c1(20)
    class(*), allocatable :: b2(:,:)

    c1 = (/ (Child(i,i+1), i=1,20) /)

    allocate(b2(3,5), SOURCE = reshape(c1, (/3,5/)))

    print *, c1
    select type (b2)
        type is (Child)
            print *, b2
        class default
            error stop 1_4
    end select
end
