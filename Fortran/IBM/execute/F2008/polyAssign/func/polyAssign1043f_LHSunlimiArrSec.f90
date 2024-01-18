! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1043f_LHSunlimiArrSec.f
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* PROGRAMMER                   : Aaron Liu
!* DATE                         : 05 August 2015
!*  ORIGIN                      : IBM XL Compiler Development, IBM Software Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* DRIVER STANZA                :
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : LHS is an unlimited polymorphic array section, RHS is an integer.
!*                              : LHS of intrinsic assignment is allowed to be polymorphic for array sections.
!*
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/05/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program main

class(*), allocatable :: b1(:)
allocate(integer :: b1(10))
b1(1:10:2) = 5
b1(2:10:2) = 6
select type(b1)
  type is (integer)
    !print*, b1
    if ( b1(1) /= 5 ) error stop 1
    if ( b1(2) /= 6 ) error stop 2
    if ( b1(3) /= 5 ) error stop 3
    if ( b1(4) /= 6 ) error stop 4
    if ( b1(5) /= 5 ) error stop 5
    if ( b1(6) /= 6 ) error stop 6
    if ( b1(7) /= 5 ) error stop 7
    if ( b1(8) /= 6 ) error stop 8
    if ( b1(9) /= 5 ) error stop 9
    if ( b1(10) /= 6 ) error stop 10
  class default
    !print*, "indefault"
end select
end 
