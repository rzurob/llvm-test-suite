! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape024.f
! %VERIFY: reshape024.out:reshape024.vf
! %STDIN:
! %STDOUT: reshape024.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
!*    SOURCE is rank four
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

program reshape024
use m
    type(Child) :: c1(10)
    type(Child) :: c2(2,3,2,2)
    class(*), allocatable :: b2(:,:)

    c1 = (/ (Child(i,i+100), i=1,10) /)
    c2 = reshape(c1, (/2,3,2,2/), (/Child(-1,1),Child(-2,2)/), (/4,3,1,2/))

    allocate(b2(3,5), SOURCE = reshape(c2, (/3,5/), &
     (/Child(-3,3),Child(-4,4)/),(/2,1/)))

    print *, c1
    print *, c2

    select type (b2)
        type is (Child)
            print *, b2
        class default
            error stop 1_4
    end select
end
