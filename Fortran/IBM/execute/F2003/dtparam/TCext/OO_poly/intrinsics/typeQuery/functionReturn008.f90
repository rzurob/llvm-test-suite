! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/intrinsics/typeQuery/functionReturn008.f
! opt variations: -qck -ql

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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child(n1)    ! (4,10)
        integer, len  :: n1
        character(n1) :: c
    end type

    contains

    logical function func1()
        class(*), allocatable, SAVE :: i
        logical :: r
        func1 = extends_type_of(Base(4)(1), i)
        if(.NOT. allocated(i)) allocate(Child(4,10)::i)
    end function

    logical function func2()
        class(*), allocatable, SAVE :: i
        logical :: r
        func2 = extends_type_of(i, Base(4)(1))
        if(.NOT. allocated(i)) allocate(Child(4,10)::i)
    end function

    logical function func3()
        class(*), allocatable, SAVE :: i
        logical :: r
        func3 = same_type_as(i, Child(4,10)(1, "abc"))
        if(.NOT. allocated(i)) allocate(Child(4,10)::i)
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
