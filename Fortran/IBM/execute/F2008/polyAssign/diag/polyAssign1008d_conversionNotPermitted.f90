! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/diag/polyAssign1008d_conversionNotPermitted.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 07 August 2015
!* ORIGIN                       : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION
!*                              : LHS is a base type allocatable scalar, not a class allocatable scalar, RHS is a derived type scalar.
!*                              : Test polymorphic assignment with derived types objects with attributes of class and type.
!*                              : The above produce the type conversion error when object is defined with type attributes rather than class attributes.
!---------------------------------------------------------------------
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/12/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


Program polyAssign1008d 
    Type t
      integer :: i = 10
    End Type
    
    Type,Extends(t) :: t2
       !integer :: j = 20
    End Type

    Type,Extends(t) :: t3
       integer :: j = 20
    End Type

    Class(t),Allocatable :: x1, x2, x3
    Type(t),Allocatable :: a
    Type(t),Allocatable :: b

    x1 = t()
    print *, 'after x1 = t(), x1%i=', x1%i

    x2 = t2()
    print *, 'after x2 = t2(), x2%i=', x2%i

    x2 = t2(11)
    print *, 'after x2 = t2(11), x2%i=', x2%i

    x3 = t3()
    print *, 'after x3 = t3(), x3%i=', x3%i
    
    x3 = t3(12, 21)
    print *, 'after x3 = t3(12, 21), x3%i=', x3%i

    a = t()
    print *, 'a%i=', a%i

    x2 = t2(15)
    print *, 'after x2 = t2(15), x2%i=', x2%i

    x3 = t2(16)
    print *, 'after x3 = t2(16), x3%i=', x3%i

    a = t2() !<--A conversion from type t2 is not permitted.
    print *, 'a%i=', a%i

    b = t2(12)! A conversion from type t2 is not permitted. 
    print *, 'b%i=', b%i
End
