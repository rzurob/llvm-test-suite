!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : submodule17f
!*
!*  PROGRAMMER                 : Bernard Kan
!*  DATE                       : December 20, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : 
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : F2008 submodule
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*  based on enum/func/fxenum0009.f
!*
!*  Use enumerators in a submodule defined in host scope
!*  
!*  Secondary tests:
!*  - bind(c) used in a module function
!*  - local helper method defined in the submodule
!*  - local variable defined in submodule and verified
!*  
!*  Verify that the results match the values of the original test case
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module m
   real :: t=1.0

   enum, bind(c)
      enumerator :: N=5
   end enum

   interface
      module subroutine nested_routine
      end subroutine
   end interface

contains

   ! internal procedure
   subroutine internal_routine

      logical precision_R4

      integer i

         do i =1, N
            t = t*i
         end do

         if (.not. precision_R4(t, 120.0)) error stop 1

   end subroutine internal_routine
   
end module m    

! external procedure
subroutine external_routine

   enum, bind(c)
      enumerator :: N=10
   end enum

   logical precision_R4

   real :: t=1.0

   integer i

   do i =1, N
      t = t*i
   end do

   if (.not. precision_R4(t, 3628800.0)) then
      print *, "t=", t
      error stop 2
   end if

end subroutine external_routine

submodule (m) msub
      real :: t=1.0
contains

   ! modify local variable t
   module subroutine nested_routine

      logical precision_R4

      ! we expect t to be the t defined in this submodule
      if (.not. precision_R4(t, 1.0))then
         print *, "t=", t
         error stop 3
      end if

      call inner_routine

      if (.not. precision_R4(t, 0.1307674280E+13)) then
         print *, "t=", t
         error stop 4
      end if

   end subroutine nested_routine

   ! helper method, local to the submodule
   subroutine inner_routine

      enum, bind(c)
         enumerator :: N=15
      end enum

      integer i

      do i =1, N
         t = t*i
      end do

   end subroutine inner_routine

end submodule msub

program  fxenum0009
use m
implicit none

   logical precision_R4

   call internal_routine

   call external_routine

   call nested_routine
end program
