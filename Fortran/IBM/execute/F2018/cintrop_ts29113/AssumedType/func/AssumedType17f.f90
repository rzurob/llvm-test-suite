!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType17f
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
!*  DESCRIPTION                : non-BIND(C) procedures defined in Fortran, 
!*                               call in Fortran 
!*                               dummy argument is assumed-shape assumded-type 
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  implicit none

  contains
  function mod_fnc(a) result(res)
     type(*) :: a(:)
     logical :: res 

     res = .true.
  end function mod_fnc 

  subroutine mod_sub(a) 
     type(*) :: a(:,:)

  end subroutine mod_sub
end module mod

program AssumedType17f
use mod
implicit none
integer, parameter :: N = 10

integer       :: k, i(N), j(N,N)
integer*1     :: i1(N), j1(N,N)
integer*2     :: i2(2), j2(N,N)
integer*4     :: i4(2*N), j4(2*N,2*N)
integer*8     :: i8(5), j8(5,5)
real          :: r(N), q(N,N)
real*4        :: r4(N), q4(N,N)
real*8        :: r8(5), q8(2,2)
real*16       :: r16(N), q16(3,3)
complex       :: z(N), y(N/2,N)
complex*8     :: z8(N), y8(N,N-1)
complex*16    :: z32(1), y32(32,23)
logical       :: l(1), t(1,N)
character*10  :: c(N), d(N*N,1)

call sub(i) 
call sub(i(2:N-1))
call sub(i(2:N:2))
call sub(i1) 
call sub(i2) 
call sub(i4) 
call sub(i8) 
call sub(r) 
call sub(r4) 
call sub(r8) 
call sub(r16) 
call sub(z) 
call sub(z8) 
call sub(z32) 
call sub(l) 
call sub(c) 
call sub([(k, k=1,N)])
call sub((/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'hh', 'ii', 'jj' /))

l = fnc(j) 
l = fnc(j1) 
l = fnc(j2) 
l = fnc(j4) 
l = fnc(j8) 
l = fnc(q) 
l = fnc(q4) 
l = fnc(q8) 
l = fnc(q16) 
l = fnc(y) 
l = fnc(y8) 
l = fnc(y32) 
l = fnc(t) 
l = fnc(d) 

call mod_sub(j) 
call mod_sub(j(1:N,2:N-1))
call mod_sub(j(2:N:2,:2))
call mod_sub(j1) 
call mod_sub(j2) 
call mod_sub(j4) 
call mod_sub(j8) 
call mod_sub(q) 
call mod_sub(q4) 
call mod_sub(q8) 
call mod_sub(q16) 
call mod_sub(y) 
call mod_sub(y8) 
call mod_sub(y32) 
call mod_sub(t) 
call mod_sub(d) 
call mod_sub(reshape([1,2,3,4],[2,2]))

l = mod_fnc(i) 
l = mod_fnc(i1) 
l = mod_fnc(i2) 
l = mod_fnc(i4) 
l = mod_fnc(i8) 
l = mod_fnc(r) 
l = mod_fnc(r4) 
l = mod_fnc(r8) 
l = mod_fnc(r16) 
l = mod_fnc(z) 
l = mod_fnc(z8) 
l = mod_fnc(z32) 
l = mod_fnc(l) 
l = mod_fnc(c) 

contains 

  logical function fnc(a) 
     type(*) :: a(:,:)

     fnc = .true. 
  end function fnc 

  subroutine sub(a) 
     type(*) :: a(:)
  end subroutine sub
  
end program AssumedType17f
