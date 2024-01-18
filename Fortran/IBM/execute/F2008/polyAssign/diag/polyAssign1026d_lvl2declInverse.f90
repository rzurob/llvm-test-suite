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
!* DESCRIPTION
!*                              : Test object declared with child type is assigned with an object declared with parent type.
!*                              : Test whether the variable of an intrinsic assignment is polymorphic.
!*                              : In this situation the compiler should give the error of "A conversion from the base type to child type is not permitted".
!*                              : We test polymorphic assignment to two levels of extensible derived types described as bellow.
!*---------------------------------------------------------------------
!* t is the base derived type, and t2 and t3 are extensibe derived
!* t2 and t3 are extensibe derived type of t
!*
!*                           t
!*                          / \
!*                         /   \
!*                        /     \
!*                       t2     t3
!*
!---------------------------------------------------------------------
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/12/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

Program polyAssign1026d
    Type t
      integer :: i = 10
    End Type
    Type, Extends(t) :: t2
    End Type
    Type, Extends(t) :: t3
       integer :: j3 = 30
    End Type
    Class(t), Allocatable :: x1, x2, x3
    Class(t3), Allocatable :: x4
    x1 = t()
    if (x1%i .ne. 10) error stop 1
    x2 = t2()
    if (x2%i .ne. 10) error stop 2
    x2 = t2(11)
    if (x2%i .ne. 11) error stop 3
    x3 = t3()
    if (x3%i .ne. 10) error stop 4
    select type (x3)
      type is (t)
        error stop 17
      type is (t3)
        if (x3%i .ne. 10) error stop 19
        if (x3%j3 .ne. 30) error stop 20
      class default
        error stop  22
    end select
    x4 = t() !<-- A conversion from type t is not permitted.
    x4 = t2() !<-- A conversion from type t is not permitted.

End Program
