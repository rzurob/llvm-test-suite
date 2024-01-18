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
!* DESCRIPTION                  : Test whether the variable of an intrinsic assignment is polymorphic.
!*                              : Variables should be allocated and assigned with the corresponding dynamic type.
!*                              : We test polymorphic assignment to four levels of extensible derived types described as bellow.
!*                              : Add inverse assignment:low level child type object is assigned with high level parent type.
!*---------------------------------------------------------------------
!* t is the base derived type, and t2 and t3 are extensibe derived
!* t2 and t3 are extensibe derived type of t
!* t4 and t5 are extensibe derived type of t3
!* t6 and t7 are extensibe derived type of t5
!*
!*                           t
!*                          / \
!*                         /   \
!*                        /     \
!*                       t2     t3
!*                              /\
!*                             /  \
!*                            /    \
!*                           t4    t5
!*                                 /\
!*                                /  \
!*                               /    \
!*                              t6    t7
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

Program polyAssign1004fa
    Type t
      integer :: i = 10
    End Type
    Type, Extends(t) :: t2
    End Type
    Type, Extends(t) :: t3
       integer :: j3 = 30
    End Type
    Type, Extends(t3) :: t4
       integer :: j4 = 40
    End Type
    Type, Extends(t3) :: t5
       integer :: j5 = 50
    End Type
    Type, Extends(t5) :: t6
       integer :: j6 = 66
    End Type
    Type, Extends(t5) :: t7
       integer :: j7 = 77
    End Type
    Class(t), Allocatable :: x1, x2, x3, x4, x5, x6, x7
    x1 = t()
    if (x1%i .ne. 10) error stop 1
    x2 = t2()
    if (x2%i .ne. 10) error stop 2
    x2 = t2(11)
    if (x2%i .ne. 11) error stop 3
    x3 = t3()
    if (x3%i .ne. 10) error stop 4
    x4 = t4()
    if (x4%i .ne. 10) error stop 5
    x5 = t5()
    if (x5%i .ne. 10) error stop 6
    x6 = t6()
    if (x6%i .ne. 10) error stop 7
    x7 = t7()
    if (x7%i .ne. 10) error stop 8
    select type (x4)
      type is (t)
        error stop 17
      type is (t3)
        error stop 18
      type is (t4)
        if (x4%i .ne. 10) error stop 19
        if (x4%j3 .ne. 30) error stop 20
        if (x4%j4 .ne. 40) error stop 21
      class default
        error stop  22
    end select
    x5 = t5()
    if (x5%i .ne. 10) error stop 23
    select type (x5)
      type is (t)
        error stop 24
      type is (t3)
        error stop 25
      type is (t5)
        if (x5%i .ne. 10) error stop 26
        if (x5%j3 .ne. 30) error stop 27
        if (x5%j5 .ne. 50) error stop 28
      class default
        error stop 29
    end select
    x5 = x4
    if (x5%i .ne. 10) error stop 30
    select type (x5)
      type is (t)
        error stop 31
      type is (t3)
        error stop 32
      type is (t4)
        if (x5%i .ne. 10) error stop 33
        if (x5%j3 .ne. 30) error stop 34
        if (x5%j4 .ne. 40) error stop 35
      type is (t5)
        error stop 36
      class default
        error stop 37
    end select
    x7 = t7()
    select type (x7)
      type is (t)
        error stop 38
      type is (t3)
        error stop 39
      type is (t5)
        error stop 40
      type is (t7)
        if (x7%i .ne. 10) error stop 41
        if (x7%j3 .ne. 30) error stop 42
        if (x7%j5 .ne. 50) error stop 43
        if (x7%j7 .ne. 77) error stop 44
      class default
        error stop 45
    end select
    x7 = x6
    select type (x7)
      type is (t)
        error stop 46
      type is (t3)
        error stop 47
      type is (t4)
        error stop 48
      type is (t5)
        error stop 49
      type is (t6)
        if (x7%i .ne. 10) error stop 50
        if (x7%j3 .ne. 30) error stop 51
        if (x7%j5 .ne. 50) error stop 52
        if (x7%j6 .ne. 66) error stop 53
      type is (t7)
        error stop 54
      class default
        error stop 55
    end select
    x7 = x1
    if (x7%i .ne. 10) error stop 56
    select type (x7)
      type is (t)
        if (x7%i .ne. 10) error stop 57
      type is (t3)
        error stop 58
      type is (t4)
        error stop 59
      type is (t5)
        error stop 60
      type is (t6)
        error stop 61
      type is (t7)
        error stop 62
      class default
        error stop 63
    end select

End Program
