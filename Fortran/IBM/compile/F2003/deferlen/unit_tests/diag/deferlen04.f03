!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
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

procedure (fun1), pointer :: ptr

!issue error message here
ptr=>fun2
end

