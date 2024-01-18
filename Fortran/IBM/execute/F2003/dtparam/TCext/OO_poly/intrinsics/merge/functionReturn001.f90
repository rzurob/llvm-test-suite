! GB DTP extension using:
! ftcx_dtp -qnock -qnok -ql -qreuse=self -qdefaultpv -qdeferredlp /tstdev/OO_poly/intrinsics/merge/functionReturn001.f
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is the return value of an internal function
!*  call.
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

    type, extends(Base) :: Child(k2)    ! (4,4)
        integer, kind :: k2
        integer(k2)      j
    end type
end module

program functionReturn001
use m
    logical :: m1(4,3)
    m1 = reshape((/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE./), (/4,3/))

    select type(name1=>merge(func2(), func1(), m1))
        type is (Child(4,4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    contains

    function func1()
        type(Child(4,4)) :: func1(4,3)
        func1 = reshape((/(Child(4,4)(i,i+1),i=1,23,2)/),(/4,3/))
    end function

    function func2()
        class(Base(4)), pointer :: func2(:,:)
        allocate(func2(4,3), SOURCE=reshape((/(Child(4,4)(i,-i),i=1,12)/), &
         (/4,3/)))
    end function
end
