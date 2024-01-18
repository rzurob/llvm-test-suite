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
!* DESCRIPTION                  : LHS is a base type of pointer, not allocatable, RHS are allocatable scalars with class and type attributes.
!*                              : Test polymorphic assignment with derived type objects.
!*                              : Test attributes with class, type, allocatable and pointer variable for polymorphic assignment.
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
    allocate(z)
    print *, 'after allocate(z)'
    print *, 'z%i=', z%i
    x = t(3)
    print *, 'after x = t(3)'
    print *, 'x%i=', x%i
    y = t2(4)
    print *, 'after y = t2(4)'
    print *, 'y%i=', y%i
    allocate(a)
    print *, 'after allocate(a):', 'a%i=',a%i
    x = a
    print *, 'after LHS intrinsic assignment "x = a", "x%i="', x%i
    y = a
    print *, 'after LHS intrinsic assignment "y = a", "y%i="', y%i
    x = z
    y = z
    z = a !<--The polymorphic variable being defined in the intrinsic assignment must be allocatable.
    z = x !<--The polymorphic variable being defined in the intrinsic assignment must be allocatable.
    z = y !<--The polymorphic variable being defined in the intrinsic assignment must be allocatable.
    a = x
    a = y
    a = z
End
