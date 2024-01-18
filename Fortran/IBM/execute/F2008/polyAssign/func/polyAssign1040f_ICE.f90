! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/diag/polyAssign1040f_ICE.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 07 September 2015
!* ORIGIN                       : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                 
!*                              : Test when LHS is an unlimited poly array component.
!*                              : Test allocatable array within a derived type for polymorphic assignment.
!*                              : We test polymorphic assignment that unlimited-polymorphic-allocatable-assumed-array inside a derived type is assigned with an integer array.
!*                              : This test case produced a defect 117584: F2008: Incorrect result when LHS is an unlimited poly array component.
!---------------------------------------------------------------------
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/12/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program polyAssign1040f
    type base
        class (*), allocatable :: data(:)
    end type

    type (base) b1
   
    integer(8) a1(0:2)

    integer :: i

    a1 = (/0, 1, 2/)
    !print *, "a1==", a1
    if (a1(0) /= 0)  error stop 1
    if (a1(1) /= 1)  error stop 2
    if (a1(2) /= 2)  error stop 3

    !print *, "lbound(a1,1)==", lbound(a1,1), "ubound(a1,1)==", ubound(a1,1)  
    if ( lbound(a1,1) /= 0) error stop 4
    if ( ubound(a1,1) /= 2) error stop 5

    !print *, " loc(a1)==", loc(a1) 

    b1%data = a1

    select type (aa=>b1%data) 
      type is (integer(8))
        if ( lbound(aa,1) /= 0) error stop 6
        if ( ubound(aa,1) /= 2) error stop 9
        if (aa(0) /= 0)  error stop 10
        if (aa(1) /= 1)  error stop 11
        if (aa(2) /= 2)  error stop 12
        !print *, " loc(aa)==", loc(aa)
      class default
        error stop 13
    end select

end program

