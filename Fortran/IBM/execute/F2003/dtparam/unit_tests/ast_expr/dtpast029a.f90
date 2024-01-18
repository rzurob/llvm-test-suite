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
type A(k1,k2,l)
  integer, kind :: k1
  integer, kind :: k2
  integer, len :: l

   character(len=l) ::c1 = "abcdefg"
   character(len=l-6) :: c2=achar(k1*k2*11)
   character(len=k2*k1) ::c3=repeat(achar(k1*k2*11),k2*k1)
   character(len=l+3) :: c4(k1*k1:5*k2)=(/(achar(i),i=k1*k1,5*k2)/)
   character(len=l-k2-k1) :: c5(k2:l)=(/achar(7*12+3),achar(k1*k2*11),achar(89),achar(k1*k2*7+34)/)
end type

type(A(2,4,7)), save :: A1
end module

use m
if(len(A1%c1).ne.7) error stop 1
if(len(A1%c2).ne.1) error stop 2
if(len(A1%c3).ne.8) error stop 3
if(len(A1%c4).ne.10) error stop 4
if(len(A1%c5).ne.1) error stop 5

if(A1%c1.ne."abcdefg") error stop 6
if(A1%c2.ne."X") error stop 7
if(A1%c3.ne."XXXXXXXX") error stop 8
if(lbound(A1%c4,1).ne.4) error stop 9
if(ubound(A1%c4,1).ne.20) error stop 10
if(lbound(A1%c5,1).ne.4) error stop 11
if(ubound(A1%c5,1).ne.7) error stop 12
if(any(A1%c4.ne.(/(achar(i),i=4,20)/))) error stop 13
if(any(A1%c5.ne.(/"W","X","Y","Z"/))) error stop 14
end

