!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : June 2nd, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :testing expressions with derived types
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
type A
  integer :: i
  integer :: j
end type

type B
  real, allocatable :: r(:)
  complex :: c
end type

type, extends(A) :: C
  character(20) :: char
end type

type D(kd,ld)
  integer, kind :: kd
  integer, len  :: ld

  character(len=ld) :: c
  integer, allocatable :: i
end type

type E
  type(A) :: A1
end type

type F(k1,k2)
  integer, kind :: k1
  integer, kind :: k2

  type(A) :: A1=A(k1+k2*k1,(k1+k2)/k1)
  type(B) :: B1=B(null(),(k2-k1,k2*2))
  type(C) :: C1=C(k2-k1,-k1,achar(20*4+8))
  type(D(k2-k1,+k2)) :: D1=D(k2-k1,k2)("abcd"//"efg",null())
  type(E) :: E1=E(A(k1,k2*2))

end type
end module

use m
type(F(10,20)) :: F1


if(F1%A1%i.ne.210.or.F1%A1%j.ne.3)  error stop 1
if(allocated(F1%B1%r).or. &
   & F1%B1%c.ne.(10,40))  error stop 2
if(F1%C1%i.ne.10.or.F1%C1%j.ne.-10.or. &
   & F1%C1%char.ne."X")  error stop 3
if(F1%D1%kd.ne.10.or.F1%D1.ld.ne.20 .or. &
   & F1%D1%c.ne."abcdefg".or.allocated(F1%D1%i))  error stop 4
if(F1%E1%A1%i.ne.10.or.F1%E1%A1%j.ne.40)  error stop 5

end

