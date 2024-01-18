! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/diag/polyAssign1002d_LHSscaRHSarr2.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 07 August 2015
!* ORIGIN                       : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!* SECONDARY FUNTIONS TESTED    : LHS of intrinsic assignment with scalars and arrays of rank 2.
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION               
!*                              : Test the following conditions
!*                              : LHS is an unallocated allocatable scalar,  RHS is an unallocated allocatable array with rank 2.
!*                              : LHS is an unallocated allocatable scalar,  RHS is an allocated array with rank 2.
!*                              : LHS is an allocated scalar,  RHS is an unallocated allocatable array with rank 2.
!*                              : LHS is an allocated scalar,  RHS is an allocated array with rank 2.
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

Program polyAssign1002
    use m
    class(base), allocatable :: b1(:), b2(:), a1(:,:), a2(:,:) !same derived type
    class(base), allocatable :: x1, y1, x2, y2

    allocate(base :: a1(-2:-1,2:11))

    allocate(child :: y2)
    
    a1 = base(1) !<--LHS is allocated initially
    if ( allocated(a1) .eqv.  .false. ) error stop "allocated(a1) status should not be false after the initiallization."

    y1 = a1 !<--Conformable array is required in this context.
    y1 = a2 !<--Conformable array is required in this context.

    y2 = a1 !<--Conformable array is required in this context.
    y2 = a2 !<--Conformable array is required in this context.

End

