! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/diag/polyAssign1018d_errorOfnonUnlimited.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 05 August 2015
!*  ORIGIN                      : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!* SECONDARY FUNTIONS TESTED    : LHS of intrinsic assignment with derived child type.
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : LHS is a allocatable base type scalar, RHS is a derived type which is not a child of the base type. 
!*                              : Test non-properly used polymorphic assignment, with conversion of types on derived types without extension.
!*                              : Define LHS as a non-unlimited polymorphic variable, produce errors on polymorphic assignment when type is not extensible. 
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/06/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main
  Type t
    integer:: i=0
    integer:: j=1
  end type

  Type t1
    integer:: k=2
  end type

  !class (*), allocatable :: x
  class(t), allocatable :: x
  x=t()
  x=t1() !<--A conversion from type t1 is not permitted

  select type (x)
    type is (t)
      print *, "type is t"
      print *, x%i
      print *, x%j
    type is (t1) !<--The type specified in the type guard statement is not compatible with the declared type of the selector.
      print *, "type is t1"
  end select 
end program
