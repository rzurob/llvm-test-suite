! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/diag/polyAssign1001d_LHSscaRHSarr1.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 07 August 2015
!* ORIGIN                       : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!* SECONDARY FUNTIONS TESTED    : LHS of intrinsic assignment with scalars and arrays.
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION               
!*                              : Test the following conditions 
!*                              : LHS is an unallocated allocatable scalar,  RHS is an unallocated allocatable array with rank 1.
!*                              : LHS is an unallocated allocatable scalar,  RHS is an allocated array with rank 1.
!*                              : LHS is an allocated scalar,  RHS is an unallocated allocatable array with rank 1.
!*                              : LHS is an allocated scalar,  RHS is an allocated array with rank 1.
!*                              : Test when LHS and RHS are not comfortable. 
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/20/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890
   
module m
   type base
      integer :: i1
   end type
   type, extends(base) :: child
      integer :: i2
   end type
end module

Program polyAssign1001
    use m
    class(base), allocatable :: b1(:), b2(:) !same derived type
    class(base), allocatable :: x1, x2

    allocate(base :: b1(2:11))  !type base

    allocate(child :: x2)

    b1 = base(1)

    x1 = b1 !<--Conformable array is required in this context.
    x1 = b2 !<--Conformable array is required in this context.

    x2 = b1 !<--Conformable array is required in this context.
    x2 = b2 !<--Conformable array is required in this context.

End

