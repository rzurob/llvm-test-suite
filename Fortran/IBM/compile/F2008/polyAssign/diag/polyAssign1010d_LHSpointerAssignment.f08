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
!* DESCRIPTION                  : LHS is a base type of pointer, not allocatable, RHS is an object of base type.
!*                              : Test polymorphic assignment with derived type objects with allocatable and pointer.
!*                              : When the derived type object is defined with pointer attribute, it produce errors on "The polymorphic variable being defined in the intrinsic assignment must be allocatable".
!---------------------------------------------------------------------
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/12/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

Program test
    Type t
     integer :: i=1
    End Type
    Type,Extends(t) :: t2
     integer :: j=2
    End Type
    Class(t),allocatable:: x,y
    Class(t),pointer:: z
    Type(t),Allocatable :: a
    z = t() !The polymorphic variable being defined in the intrinsic assignment must be allocatable.
    a = t(2)
    x = t(3)
    y = t2(4)
End
