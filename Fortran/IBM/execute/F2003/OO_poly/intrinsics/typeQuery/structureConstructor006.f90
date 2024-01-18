! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureConstructor006.f
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
!*  DATE                       : 10/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MOLD: is specified using structure constructor with extensible
!*          non abstract type.
!*    A   : unlimited polymorphic, dynamic type is non-extensible.
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

    type SequenceBase
        sequence
        integer i
        integer j
    end type
end module

program structureConstructor006
use m
    class(*), pointer :: arg1 => null()
    class(*), allocatable :: arg2

    allocate(integer::arg1)
    allocate(SequenceBase::arg2)

    if(extends_type_of(arg1, Base(1))) error stop 1_4
    if(extends_type_of(arg2, Base(1))) error stop 2_4
    if(same_type_as(arg1, Base(1))) error stop 3_4
    if(same_type_as(arg2, Base(1))) error stop 4_4
end
