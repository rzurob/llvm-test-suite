! *********************************************************************
!* ===================================================================
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* DATE                         : 07 August 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test whether the array of an intrinsic assignment is allowed to be polymorphic for different dynamic type, extents, and bounds.
!*                              : We test array polymorphic assignment to extensible derived types with integer type inside.
!*                              : Test the initialization for an unallocated allocatable array with base type.
!*                              : Disabled
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
end module

Program polyAssign1018
    use m
    class(base), allocatable :: b1(:)
    if ( allocated(b1) .neqv.  .false. ) error stop "allocated(b1) status should be false initially."
End

