! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/diag/polyAssign1025d_2level.f
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
!*                              : Test compiling with Fortran language standards: Fortran 2003std, 95std, 90std, 77std.
!*                              : Test whether the variable of an intrinsic assignment is polymorphic. 
!*                              : Variables should be allocated and assigned with the corresponding dynamic type.
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

Program polyAssign1025d 
    Type t
      integer :: i = 10
    End Type
    Type, Extends(t) :: t2
    End Type
    Type, Extends(t) :: t3
       integer :: j3 = 30
    End Type
    Class(t), Allocatable :: x1, x2, x3
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

End Program
