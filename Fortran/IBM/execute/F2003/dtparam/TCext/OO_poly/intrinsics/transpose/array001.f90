! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/transpose/array001.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    MATRIX is array section
!*    Non-poly, poly, and unlimited poly
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
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program array001
use m
    type(Base(20,4)) :: b1(2,2,2,2)
    class(Base(:,4)), pointer :: b2(:,:)
    class(*), allocatable :: b3(:,:,:)

    b1 = reshape((/(Base(20,4)(i),i=1,16)/), (/2,2,2,2/))

    allocate(b2(4,4), SOURCE=reshape((/(Child(20,4)(i,i+1),i=5,20)/),(/4,4/)))
    allocate(b3(2,5,4), SOURCE=reshape((/(Base(20,4)(i),i=11,50)/),(/2,5,4/)))

    print *, transpose(b1(2,:,:2,1))

    select type(name1=>transpose(b2(2:3,2:)))
        type is (Child(*,4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>transpose(b3(2:4:3,3,2:)))
        type is (Base(*,4))
            print *, name1
        class default
            error stop 2_4
    end select
end
