! *********************************************************************
!* ===================================================================
!* XL Fortran Test Case                         IBM INTERNAL USE ONLY
!* ===================================================================
!*
!* TEST CASE TITLE              : F2008/polyAssign/func/polyAssign1044f_LHSunlimiArrSecUnalloc.f
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
!*                              : Adding LHS is unallocated.
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

class(*), allocatable :: b1(:),a1(:),a2(:)
allocate(integer :: b1(10))

allocate(integer :: a1(8))

b1(1:10:2) = 5
b1(2:10:2) = 6

a1(1:8:2) = 1
a1(2:8:2) = 2

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
    error stop 11
end select

a2 = b1
if (lbound(a2,1) /= 1 ) error stop 12
if (ubound(a2,1) /= 10 ) error stop 13
select type(a2)
  type is (integer)
    if ( a2(1) /= 5 ) error stop 21
    if ( a2(2) /= 6 ) error stop 22
    if ( a2(3) /= 5 ) error stop 23
    if ( a2(4) /= 6 ) error stop 24
    if ( a2(5) /= 5 ) error stop 25
    if ( a2(6) /= 6 ) error stop 26
    if ( a2(7) /= 5 ) error stop 27
    if ( a2(8) /= 6 ) error stop 28
    if ( a2(9) /= 5 ) error stop 29
    if ( a2(10) /= 6 ) error stop 30
  class default
    error stop 31
end select

a2 = a1
if (lbound(a2,1) /= 1 ) error stop 32
if (ubound(a2,1) /= 8 ) error stop 33
select type(a2)
  type is (integer)
    if ( a2(1) /= 1 ) error stop 51
    if ( a2(2) /= 2 ) error stop 52
    if ( a2(3) /= 1 ) error stop 53
    if ( a2(4) /= 2 ) error stop 54
    if ( a2(5) /= 1 ) error stop 55
    if ( a2(6) /= 2 ) error stop 56
    if ( a2(7) /= 1 ) error stop 57
    if ( a2(8) /= 2 ) error stop 58
  class default
    error stop 59
end select

end 
