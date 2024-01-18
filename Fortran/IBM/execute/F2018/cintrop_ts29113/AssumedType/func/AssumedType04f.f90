!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType04f
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : BIND(C) procedures defined in Fortran, 
!*                               call in Fortran 
!*                               dummy argument is scalar assumed type 
!*                               actual argument is of Fortran intrinsic type 
!*                              
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType04f
implicit none

interface
   logical(c_bool) function fnc(a) bind(c)
      use, intrinsic :: iso_c_binding
      type(*) :: a

   end function fnc 
   subroutine sub(a) bind(c)
      use, intrinsic :: iso_c_binding
      type(*) :: a
   end subroutine sub
end interface

integer       :: i 
integer*1     :: i1 
integer*2     :: i2 
integer*4     :: i4 
integer*8     :: i8 
integer(8)    :: i88
real          :: r 
real*4        :: r4
real(4)       :: r44
real*8        :: r8
real*16       :: r16
complex       :: z 
complex*8     :: z8
complex(4)    :: z88 
complex*16    :: z32
complex(16)   :: z16
logical       :: l 
logical*1     :: l1
logical*2     :: l2
logical*4     :: l4
logical*8     :: l8
character*10  :: c1
character(10) :: c2

call sub(i) 
call sub(i1) 
call sub(i2) 
call sub(i4) 
call sub(i8) 
call sub(i88) 
call sub(r) 
call sub(r4) 
call sub(r44) 
call sub(r8) 
call sub(r16) 
call sub(z) 
call sub(z8) 
call sub(z88) 
call sub(z32) 
call sub(z16) 
call sub(l) 
call sub(l1) 
call sub(l2) 
call sub(l4) 
call sub(l8) 
call sub(c1) 
call sub(c2) 

l = fnc(i) 
l = fnc(i1) 
l = fnc(i2) 
l = fnc(i4) 
l = fnc(i8) 
l = fnc(i88) 
l = fnc(r) 
l = fnc(r4) 
l = fnc(r44) 
l = fnc(r8) 
l = fnc(r16) 
l = fnc(z) 
l = fnc(z8) 
l = fnc(z88) 
l = fnc(z32) 
l = fnc(z16) 
l = fnc(l) 
l = fnc(l1) 
l = fnc(l2) 
l = fnc(l4) 
l = fnc(l8) 
l = fnc(c1) 
l = fnc(c2) 

end program AssumedType04f

logical(c_bool) function fnc(a) 
   use, intrinsic :: iso_c_binding
   type(*) :: a

   fnc = .true. 
end function fnc 
subroutine sub(a) 
   use, intrinsic :: iso_c_binding
   type(*) :: a
end subroutine sub
