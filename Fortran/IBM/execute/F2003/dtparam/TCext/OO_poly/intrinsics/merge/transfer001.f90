! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/merge/transfer001.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer001.f
! %VERIFY: transfer001.out:transfer001.vf
! %STDIN:
! %STDOUT: transfer001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is function return of transfer.
!*    Poly and unlimited poly.
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
    type, abstract :: AbstractParent(n1)    ! (20)
        integer, len :: n1
    end type

    type, extends(AbstractParent) :: Base(k1)    ! (20,4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(k2)    ! (20,4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program transfer001
use m
    class(AbstractParent(:)), pointer :: c1(:,:,:)
    class(AbstractParent(:)), allocatable :: b1(:)
    class(*), pointer :: b2
    class(*), pointer :: c2(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(20,4,4)(i,i-1),i=101,108)/), &
     (/2,2,2/)))
    allocate(b2, SOURCE=Base(20,4)(-2))
    allocate(c2(6), SOURCE=(/(Base(20,4)(i),i=1,6)/))

    select type(name1=>merge(transfer(c1, b2), c2, .TRUE.))
        type is (Base(*,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(8), SOURCE=(/(Child(20,4,4)(i,-i),i=1,8)/))

    select type(name1=>merge(transfer(b2,b1,3),transfer(c2,c1),.FALSE.))
        type is (Child(*,4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
