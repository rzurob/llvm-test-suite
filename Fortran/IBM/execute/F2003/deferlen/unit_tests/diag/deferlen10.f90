!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The procedure pointer assignment
!*                               statement target must be declared with 
!*                               the same type and type parameters
!*                               as the procedure pointer.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
interface
   function fun1(x)
      character*10 x
      character(10), allocatable :: fun1
   end function
   function fun2(x)
      character*10 x
      character (:), allocatable :: fun2
   end function
end interface

procedure (fun2), pointer :: ptr

!issue error message here
ptr=>fun1
end


