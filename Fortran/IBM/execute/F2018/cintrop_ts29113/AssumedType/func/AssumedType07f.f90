!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 13, 2012
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : BIND(C) procedures defined in Fortran,
!*                               call in Fortran
!*                               dummy argument is assumed-size assumded-type
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType07f
implicit none
integer, parameter :: N = 10
integer       :: k

interface
  function fnc(a) result(res) bind(c)
     use, intrinsic :: iso_c_binding
     type(*) :: a(*)
     logical(c_bool) :: res
  end function fnc

  subroutine sub(a) bind(c)
     use, intrinsic :: iso_c_binding
     type(*) :: a(*)
  end subroutine sub
end interface

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

end program AssumedType07f
function fnc(a) result(res) bind(c)
     use, intrinsic :: iso_c_binding
     type(*) :: a(*)
     logical(c_bool) :: res

     res = .true.
end function fnc

subroutine sub(a)  bind(c)
     use, intrinsic :: iso_c_binding
     type(*) :: a(*)

end subroutine sub
