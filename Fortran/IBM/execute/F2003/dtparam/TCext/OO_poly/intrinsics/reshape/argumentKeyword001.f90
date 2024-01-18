! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/intrinsics/reshape/argumentKeyword001.f
! opt variations: -qck -qnol

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
!*  DATE                       : 11/25/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : Actual arguments are specified using
!*    argument keywords.
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

    type, extends(Base) :: Child(n2)    ! (20,4,10)
        integer, len  :: n2
        character(n2) :: c
    end type
end module

program argumentKeyword001
use m
    type(Base(20,4)) :: b1(10)
    type(Base(20,4)) :: b2(2,4)
    type(Base(20,4)) :: b3(3,4)
    type(Base(20,4)) :: b4(3,5)

    b1 = (/ (Base(20,4)(i), i=1,10) /)

    b2 = reshape(b1, ORDER=(/2,1/), PAD=(/Base(20,4)(-1),Base(20,4)(-2)/), SHAPE=(/2,4/))

    b3 = reshape(PAD=(/Base(20,4)(-1),Base(20,4)(-2)/), ORDER=(/2,1/), &
     SHAPE=(/3,4/), SOURCE=b1)

    b4 = reshape(SOURCE=b1, SHAPE=(/3,5/), &
     PAD=(/Base(20,4)(-1),Base(20,4)(-2)/), ORDER=(/2,1/))

    print *, b1
    print *, b2
    print *, b3
    print *, b4
end
