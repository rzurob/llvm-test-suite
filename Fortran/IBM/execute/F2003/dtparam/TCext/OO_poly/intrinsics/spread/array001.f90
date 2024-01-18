! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_poly/intrinsics/spread/array001.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: array001.f
! %VERIFY: array001.out:array001.vf
! %STDIN:
! %STDOUT: array001.out
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
!*    SOURCE is array element or array section
!*    Poly and unlimited poly
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j
    end type
end module

program array001
use m
    class(Base(4)), allocatable :: b1(:,:)
    class(*), pointer :: u1(:,:)

    allocate(b1(2,3), SOURCE=reshape((/(Child(4)(i,i+1),i=1,6)/), &
     (/2,3/)))
    allocate(u1(3,4), SOURCE=reshape((/(Child(4)(i,i+1),i=1,23,2)/), &
     (/3,4/)))

    select type(name1=>spread(b1(2,3), 1, 3))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>spread(u1(:2,2:3), 3, 3))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
