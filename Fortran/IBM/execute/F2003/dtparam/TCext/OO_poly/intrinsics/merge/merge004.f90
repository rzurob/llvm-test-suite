! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/merge/merge004.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge004.f
! %VERIFY: merge004.out:merge004.vf
! %STDIN:
! %STDOUT: merge004.out
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
!*    Non-poly
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
end module

program merge004
use m
    type(Base(4)) :: b1(3,2)
    type(Base(4)) :: b2

    associate(name1=>merge(reshape((/(Base(4)(i),i=1,12)/),(/4,3/)), &
     Base(4)(-2), .TRUE.))
        if(.NOT. same_type_as(name1, Base(4)(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>merge(reshape((/(Base(4)(i),i=1,12)/),(/4,3/)), &
     Base(4)(-2), .FALSE.))
        if(.NOT. same_type_as(name1, Base(4)(1))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    b1%i = reshape((/(i,i=1,6)/),(/3,2/))
    b2%i = 4

    associate(name1=>merge(b1, b2, .TRUE.))
        if(.NOT. same_type_as(name1, Base(4)(1))) error stop 3_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>merge(b2, b1, .TRUE.))
        if(.NOT. same_type_as(name1, Base(4)(1))) error stop 4_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
