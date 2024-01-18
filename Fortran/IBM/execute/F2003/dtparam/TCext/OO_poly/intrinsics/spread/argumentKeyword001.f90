! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/intrinsics/spread/argumentKeyword001.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: argumentKeyword001.f
! %VERIFY: argumentKeyword001.out:argumentKeyword001.vf
! %STDIN:
! %STDOUT: argumentKeyword001.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : spread
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    Actual arguments are specified using argument keywords.
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
end module

program argumentKeyword001
use m
    class(Base(:,4)), pointer :: b1(:,:)
    allocate(b1(2,3), SOURCE=reshape((/(Base(20,4)(i),i=1,6)/), (/2,3/)))

    print *, spread(SOURCE=Base(20,4)(10), DIM=1, NCOPIES=2)
    print *, spread(NCOPIES=2, DIM=1, SOURCE=(/Base(20,4)(1),Base(20,4)(2)/))

    select type(name1=>spread(DIM=1, NCOPIES=2, SOURCE=b1))
        type is(Base(*,4))
            print *, name1
        class default
            error stop 1_4
    end select
end
