! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeDeclaration010.f
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
!*  DATE                       : 10/26/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : extends_type_of(A, MOLD)
!*  SECONDARY FUNCTIONS TESTED : same_type_as(A, B)
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    MOLD: non polymorphic and declared type is extensible.
!*    A   : unlimited polymorphic, but not disassociated pointer or
!*          unallocated allocatable. Dynamic type is non extensible.
!*    Note: If A is unlimited polymorphic, the dynamic type of A
!*          is allowed to be non extensible.
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
    type SequenceBase
        sequence
        integer i
        integer j
    end type

    type Base
        integer i
    end type
end module

program typeDeclaration010
use m
    type(Base) :: mold1
    class(*), pointer :: arg1 => null()

    allocate(integer::arg1)
    if(extends_type_of(arg1, mold1)) error stop 1_4
    if(same_type_as(arg1, mold1)) error stop 2_4

    deallocate(arg1)
    allocate(SequenceBase::arg1)
    if(extends_type_of(arg1, mold1)) error stop 3_4
    if(same_type_as(arg1, mold1)) error stop 4_4
end
