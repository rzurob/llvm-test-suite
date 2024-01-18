! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn003.f
! %VERIFY:
! %STDIN:
! %STDOUT:
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
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : A or MOLD is the return value of
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
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        character(10) :: c
    end type
end module

program functionReturn003
use m
    type(Base) :: b1, b2, b3
    type(Child) :: c1, c2, c3
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2
    allocate(Complex::arg1)

    allocate(arg2, SOURCE=transfer((/1.0,2.0,3.0/), (0.0,0.0)))
    if(extends_type_of(arg1, arg2)) error stop 1_4
    if(.NOT. same_type_as(arg1, arg2)) error stop 2_4

    deallocate(arg1, arg2)
    allocate(Base::arg1)
    allocate(arg2, SOURCE=transfer((/b1,b2,b3/), Child(1, "abc")))
    if(extends_type_of(arg1, arg2)) error stop 3_4
    if(.NOT. extends_type_of(arg2, arg1)) error stop 4_4
    if(same_type_as(arg1, arg2)) error stop 5_4

    deallocate(arg2)
    allocate(arg2, SOURCE=transfer((/c1,c2,c3/), Base(1)))
    if(.NOT. extends_type_of(arg1, arg2)) error stop 6_4
    if(.NOT. extends_type_of(arg2, arg1)) error stop 7_4
    if(.NOT. same_type_as(arg1, arg2)) error stop 8_4
end
