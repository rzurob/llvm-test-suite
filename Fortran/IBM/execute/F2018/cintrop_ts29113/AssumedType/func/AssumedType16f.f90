!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : non-BIND(C) procedures defined in Fortran,
!*                               call in Fortran
!*                               dummy argument is assumed-size assumded-type
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
module mod
  implicit none

  contains
  function mod_fnc(a) result(res)
     type(*) :: a(*)
     logical :: res

     res = .true.
  end function mod_fnc

  subroutine mod_sub(a)
     type(*) :: a(*)

  end subroutine mod_sub
end module mod

program AssumedType16f
use mod
implicit none
integer, parameter :: N = 10
integer       :: k

integer       :: i(N)
integer*1     :: i1(N,N)
integer*2     :: i2(2,2,2,2)
integer*4     :: i4(2*N)
integer*8     :: i8(5,4,3,2,1)
real          :: r(N,N)
real*4        :: r4(N,N,N)
real*8        :: r8(5,5)
real*16       :: r16(N)
complex       :: z(N,N)
complex*8     :: z8(1,N)
complex*16    :: z32(N,1)
logical       :: l(1,1,1,1,1,1,1,1)
character*10  :: c(N)

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
call sub(reshape([1,2,3,4],[2,2]))
call sub((/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'hh', 'ii', 'jj' /))

l = fnc(i)
l = fnc(i1)
l = fnc(i2)
l = fnc(i4)
l = fnc(i8)
l = fnc(r)
l = fnc(r4)
l = fnc(r8)
l = fnc(r16)
l = fnc(z)
l = fnc(z8)
l = fnc(z32)
l = fnc(l)
l = fnc(c)

call mod_sub(i)
call mod_sub(i(2:N-1))
call mod_sub(i(2:N:2))
call mod_sub(i1)
call mod_sub(i2)
call mod_sub(i4)
call mod_sub(i8)
call mod_sub(r)
call mod_sub(r4)
call mod_sub(r8)
call mod_sub(r16)
call mod_sub(z)
call mod_sub(z8)
call mod_sub(z32)
call mod_sub(l)
call mod_sub(c)
call mod_sub([(k, k=1,N)])
call mod_sub(reshape([1,2,3,4],[2,2]))
call mod_sub((/ 'aa', 'bb', 'cc', 'dd', 'ee', 'ff', 'gg', 'hh', 'ii', 'jj' /))

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
     type(*) :: a(*)

     fnc = .true.
  end function fnc

  subroutine sub(a)
     type(*) :: a(*)
  end subroutine sub

end program AssumedType16f
