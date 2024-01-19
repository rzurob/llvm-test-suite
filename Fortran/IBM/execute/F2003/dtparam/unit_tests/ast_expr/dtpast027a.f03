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
type A(k1,k2)
  integer, kind :: k1
  integer, kind  :: k2

  real((k1+k1)*(k1-2)) :: r1=cos(real(k2+k2-k1, (k1+k1)*(k1-2)))
  real(k1+k1) ::  r2=sin(real(k1*k2, k1+k1))
  real(k1) :: r3=tan(real(k2, k1))
  real(k2) :: r4(2:4)=(/real(k1*k2, k2),real(k2+k1, k2),real(k2-k1, k2)/)
  real(k1) :: r5(k1+k2+(k1*k2)) = (/(real(k2*k1+l, k1),l=1,k1+k2+(k1*k2))/)

end type
end module

use m
type(A(4,8)) :: A1

if(kind(A1%r1).ne.16)  error stop 1
if(kind(A1%r2).ne.8)  error stop 2
if(kind(A1%r3).ne.4)  error stop 3
if(kind(A1%r4).ne.8)  error stop 4
if(kind(A1%r5).ne.4)  error stop 5
if(A1%r1.ne.cos(real(12.0, kind=A1%r1%kind))) error stop 6
if(A1%r2.ne.sin(real(32.0, kind=A1%r2%kind))) error stop 7
if(A1%r3.ne.tan(real(8.0, kind=A1%r3%kind))) error stop 8
if(lbound(A1%r4,1).ne.2) error stop 9
if(ubound(A1%r4,1).ne.4) error stop 10
if(ubound(A1%r5,1).ne.44) error stop 11
if(any(A1%r4.ne.(/32.0,12.0,4.0/))) error stop 12
if(any(A1%r5.ne.(/(real(32+i),i=1,44)/))) error stop 13


end

