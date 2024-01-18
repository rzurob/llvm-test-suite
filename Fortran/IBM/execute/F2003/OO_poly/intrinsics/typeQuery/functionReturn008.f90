! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn008.f
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
!*  DESCRIPTION                : Test the SAVE attribute.
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

    type, extends(Base) :: Child
        character(10) :: c
    end type

    contains

    logical function func1()
        class(*), allocatable, SAVE :: i
        logical :: r
        func1 = extends_type_of(Base(1), i)
        if(.NOT. allocated(i)) allocate(Child::i)
    end function

    logical function func2()
        class(*), allocatable, SAVE :: i
        logical :: r
        func2 = extends_type_of(i, Base(1))
        if(.NOT. allocated(i)) allocate(Child::i)
    end function

    logical function func3()
        class(*), allocatable, SAVE :: i
        logical :: r
        func3 = same_type_as(i, Child(1, "abc"))
        if(.NOT. allocated(i)) allocate(Child::i)
    end function
end module

program functionReturn008
use m
    if(.NOT. func1()) error stop 1_4
    if(func1()) error stop 2_4

    if(func2()) error stop 3_4
    if(.NOT. func2()) error stop 4_4

    if(func3()) error stop 5_4
    if(.NOT. func3()) error stop 6_4
end
