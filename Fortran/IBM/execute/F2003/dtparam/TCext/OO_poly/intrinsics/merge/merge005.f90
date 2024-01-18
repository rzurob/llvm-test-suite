! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/merge005.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge005.f
! %VERIFY: merge005.out:merge005.vf
! %STDIN:
! %STDOUT: merge005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/21/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar/array
!*    FSOURCE is scalar/array
!*    MASK is scalar
!*    Poly
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
        integer(k1)   :: i = 8
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) :: j = 9
    end type
end module

program merge005
use m
    class(Base(4)), pointer :: c1(:,:)
    class(Base(4)), allocatable :: c2

    allocate(c1(3,2), SOURCE=reshape((/(Child(4)(i,-i),i=1,6)/), (/3,2/)))
    allocate(c2, SOURCE=Child(4)(4,-4))

    select type(name1=>merge(c1, c2, .TRUE.))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>merge(c2, c1, .TRUE.))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select

    deallocate(c1, c2)
    allocate(c1(4,5), SOURCE=reshape((/(Base(4)(i),i=1,20)/), (/4,5/)))
    allocate(c2, SOURCE=Base(4)(6))

    select type(name1=>merge(c2, c1, .FALSE.))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 3_4
    end select

    select type(name1=>merge(c1, c2, .FALSE.))
        type is (Base(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 4_4
    end select
end
