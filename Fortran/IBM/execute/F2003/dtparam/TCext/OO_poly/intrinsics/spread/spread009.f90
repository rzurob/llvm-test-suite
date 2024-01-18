! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/spread/spread009.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: spread009.f
! %VERIFY: spread009.out:spread009.vf
! %STDIN:
! %STDOUT: spread009.out
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
!*    Source is array and unlimited poly.
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 9
    end type
end module

program spread009
use m
    class(*), allocatable :: b1(:,:)
    allocate(b1(2,3), SOURCE=reshape((/(Child(20,4)(i,i+1),i=3,13,2)/), &
     (/2,3/)))

    select type(name1=>spread(b1, 3, 4))
        type is (Child(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select
end
