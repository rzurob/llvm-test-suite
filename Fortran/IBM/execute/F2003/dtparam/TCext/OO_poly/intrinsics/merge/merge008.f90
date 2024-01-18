! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/merge008.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge008.f
! %VERIFY: merge008.out:merge008.vf
! %STDIN:
! %STDOUT: merge008.out
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
!*    TSOURCE is array
!*    FSOURCE is array
!*    MASK is scalar or array
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

program merge008
use m
    class(Base(4)), pointer :: b1(:,:)
    class(Base(4)), allocatable :: b2(:,:)
    logical :: m1(3,2)

    allocate(b1(3,2), SOURCE=reshape((/(Child(4)(i,i+1), &
     i=1,11,2)/),(/3,2/)))
    allocate(b2(3,2), SOURCE=reshape((/(Child(4)(i,i-1), &
     i=-1,-11,-2)/),(/3,2/)))
    m1 = reshape((/.TRUE., .FALSE., .FALSE., .TRUE., .FALSE., &
     .TRUE./),(/3,2/))

    select type(name1=>merge(b1, b2, .TRUE.))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    select type(name1=>merge(b1, b2, m1))
        type is (Child(4))
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
