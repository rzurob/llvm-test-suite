! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/spread/transpose003.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/06/2005
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is function return of transpose.
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        integer(k1) j
    end type
end module

program transpose003
use m
    class(AbstractParent(4,:)), pointer :: c1(:,:)
    class(*), pointer :: b1(:,:)

    allocate(c1(2,4), SOURCE=reshape((/(Child(4,20)(i,i-1),i=1,8)/), (/2,4/)))

    select type(name1=>spread(transpose(c1), 3, 3))
        type is (Child(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(3,4), SOURCE=reshape((/(Base(4,20)(i),i=1,12)/), (/3,4/)))

    select type(name1=>spread(transpose(b1), 3, 2))
        type is (Base(4,*))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
