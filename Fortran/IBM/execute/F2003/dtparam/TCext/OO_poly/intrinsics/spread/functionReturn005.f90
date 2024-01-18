! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/functionReturn005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn005.f
! %VERIFY: functionReturn005.out:functionReturn005.vf
! %STDIN:
! %STDOUT: functionReturn005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/18/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is the return value of intrinsic function transpose.
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
        integer(k1)   :: i = -1
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = -2
    end type
end module

program functionReturn005
use m
    class(Base(:,4)), pointer :: b1(:)
    class(*), allocatable :: b2(:,:,:)

    allocate(b1(10), SOURCE=(/(Base(20,4)(i),i=1,10)/))

    select type(name1=>spread(transpose(reshape(b1,(/3,5/), &
     (/Base(20,4)(-1),Base(20,4)(-2)/),(/2,1/))), 3, 2))
        type is (Base(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b2(2,2,2), SOURCE=reshape((/(Child(20,4)(i,-i),i=1,8)/),(/2,2,2/)))

    select type(name1=>spread(transpose(reshape(b2,(/4,2/))),3,3))
        type is (Child(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
