! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn005.f
! opt variations: -qnol -qreuse=none

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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/02/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is the return value of
!*                               intrinsic function transfer().
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
        integer(k1)   :: i = 1
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 2
        integer(k1) :: k = 3
        integer(k1) :: l = 4
        integer(k1) :: m = 5
    end type
end module

program functionReturn005
use m
    type(Child(20,4)) :: b1
    class(*), pointer :: arg1(:,:) => null()
    b1%i = 8
    b1%j = 9

    allocate(arg1(2,2), SOURCE=reshape(transfer(b1,Base(20,4)(1),5),(/2,2/)))

    select type (arg1)
        type is (Base(*,4))
            print *, arg1
        class default
            error stop 1_4
    end select
end
