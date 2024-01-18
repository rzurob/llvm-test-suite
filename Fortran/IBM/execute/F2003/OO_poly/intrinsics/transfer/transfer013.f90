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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
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
    type Base
        integer i
    end type

    type Base1
        integer k
        type(Base) :: b
        integer i
        integer j
    end type
end module

program transfer013
use m
    type(Base1), pointer :: m1
    type(Base) :: src1(4)
    src1 = (/ (Base(i), i=3,6) /)
    nullify(m1)

    if(.NOT. same_type_as(transfer((/(Base(i),i=1,4)/), m1), &
     Base1(1,Base(1),1,1))) then
        error stop 1_4
    end if

    print *, transfer((/(Base(i),i=5,8)/), m1)

    print *, transfer(reshape(src1,(/2,2/),(/Base(-1)/),(/2,1/)), m1)
end
