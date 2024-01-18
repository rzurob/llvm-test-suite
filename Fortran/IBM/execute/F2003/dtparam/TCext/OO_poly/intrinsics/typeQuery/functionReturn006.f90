! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/intrinsics/typeQuery/functionReturn006.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn006.f
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
!*  DATE                       : 11/11/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : A or MOLD is the return value of a
!*                               type bound procedure call.
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

        contains

        procedure, pass :: create => createBase
    end type

    type, extends(Base) :: Child(k2,n2)    ! (20,4,4,10)
        integer, kind :: k2
        integer, len  :: n2
        character(n2) :: c

        contains

        procedure, pass :: create => createChild
    end type

    contains

    function createBase(a)
        class(Base(*,4)), intent(in) :: a
        class(Base(:,4)), allocatable :: createBase
        allocate(Base(20,4)::createBase)
    end function

    function createChild(a)
        class(Child(*,4,4,*)), intent(in) :: a
        class(Base(:,4)), allocatable :: createChild
        allocate(Child(20,4,4,10)::createChild)
    end function
end module

program functionReturn006
use m
    class(Base(:,4)), pointer :: ap1 => null()
    allocate(Base(20,4)::ap1)

    if(.NOT. extends_type_of(Base(20,4)(1), ap1%create())) error stop 1_4
    if(.NOT. extends_type_of(ap1%create(), Base(20,4)(1))) error stop 2_4
    if(.NOT. same_type_as(ap1%create(), Base(20,4)(1))) error stop 3_4

    deallocate(ap1)
    allocate(Child(20,4,4,10)::ap1)
    if(.NOT. extends_type_of(Child(20,4,4,10)(1,"a"), ap1%create())) error stop 4_4
    if(.NOT. extends_type_of(ap1%create(), Child(20,4,4,10)(1,"a"))) error stop 5_4
    if(.NOT. same_type_as(ap1%create(), Child(20,4,4,10)(1,"a"))) error stop 6_4
end
