! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/OO_poly/intrinsics/transfer/transfer013.f
! opt variations: -qnol -qdefaultpv -qdeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer013.f
! %VERIFY: transfer013.out:transfer013.vf
! %STDIN:
! %STDOUT: transfer013.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is scalar
!*    SOURCE is array
!*    Physical representation of result has the same length as that
!*  of SOURCE.
!*    The result is a scalar of the same type and type parameters as
!*  MOLD, and has the same physical representation as SOURCE.
!*    Non-poly and MOLD can be an undefined variable.
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

    type Base1(n2,k2)    ! (20,4)
        integer, kind     :: k2
        integer, len      :: n2
        integer(k2)          k
        type(Base(n2,k2)) :: b
        integer(k2)          i
        integer(k2)          j
    end type
end module

program transfer013
use m
    type(Base1(20,4)), pointer :: m1
    type(Base(20,4)) :: src1(4)
    src1 = (/ (Base(20,4)(i), i=3,6) /)
    nullify(m1)

!   if(.NOT. same_type_as(transfer((/(Base(20,4)(i),i=1,4)/), m1), &
!    Base1(20,4)(1,Base(20,4)(1),1,1))) then

    if(.NOT. same_type_as(transfer((/Base(20,4)(1),Base(20,4)(2), &
     Base(20,4)(3),Base(20,4)(4)/), m1), &
     Base1(20,4)(1,Base(20,4)(1),1,1))) then
        error stop 1_4
    end if

!   print *, transfer((/(Base(20,4)(i),i=5,8)/), m1)
    print *, transfer((/Base(20,4)(5),Base(20,4)(6),Base(20,4)(7),Base(20,4)(8)/), m1)

    print *, transfer(reshape(src1,(/2,2/),(/Base(20,4)(-1)/),(/2,1/)), m1)
end
