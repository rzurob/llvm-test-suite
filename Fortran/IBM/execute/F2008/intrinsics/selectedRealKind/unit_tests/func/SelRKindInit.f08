!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : November 05, 2010
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic function
!*                               selected_real_kind(p,r,radix)
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 376076
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION:
!*  -----------
!*  Test initialization expressions for selected_real_kind intrinsic
!*  specified by the argument p,r,radix
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program SelRKindReturnVal

integer :: a1 = selected_real_kind(2,2,2)
integer :: a2 = selected_real_kind(11,294,2)
integer :: a3 = selected_real_kind(16,290,2)
integer :: a4 = selected_real_kind(12,9,-2)

integer :: b1 = selected_real_kind(33,290,2)
integer :: b2 = selected_real_kind(15,330,2)
integer :: b3 = selected_real_kind(33,310,2)
integer :: b4 = selected_real_kind(17,300,2)
integer :: b5 = selected_real_kind(7,30,3)

integer :: c1 = selected_real_kind(0,0,2)
integer :: c2 = selected_real_kind(0,37,2)
integer :: c3 = selected_real_kind(0,291,2)
integer :: c4 = selected_real_kind(0,307,2)
integer :: c5 = selected_real_kind(0,308,2)

integer :: d1 = selected_real_kind(6,0,2)
integer :: d2 = selected_real_kind(6,37,2)
integer :: d3 = selected_real_kind(6,291,2)
integer :: d4 = selected_real_kind(6,307,2)
integer :: d5 = selected_real_kind(6,308,2)

integer :: e1 = selected_real_kind(15,0,2)
integer :: e2 = selected_real_kind(15,37,2)
integer :: e3 = selected_real_kind(15,291,2)
integer :: e4 = selected_real_kind(15,307,2)
integer :: e5 = selected_real_kind(15,308,2)

integer :: f1 = selected_real_kind(31,0,2)
integer :: f2 = selected_real_kind(31,37,2)
integer :: f3 = selected_real_kind(31,291,2)
integer :: f4 = selected_real_kind(31,307,2)
integer :: f5 = selected_real_kind(31,308,2)

integer :: g1 = selected_real_kind(32,0,2)
integer :: g2 = selected_real_kind(32,37,2)
integer :: g3 = selected_real_kind(32,291,2)
integer :: g4 = selected_real_kind(32,307,2)
integer :: g5 = selected_real_kind(32,308,2)
integer :: g6 = selected_real_kind(32,308,0)

print *, a1
print *, a2
print *, a3
print *, a4

print *, b1
print *, b2
print *, b3
print *, b4
print *, b5

print *, c1
print *, c2
print *, c3
print *, c4
print *, c5

print *, d1
print *, d2
print *, d3
print *, d4
print *, d5

print *, e1
print *, e2
print *, e3
print *, e4
print *, e5

print *, f1
print *, f2
print *, f3
print *, f4
print *, f5

print *, g1
print *, g2
print *, g3
print *, g4
print *, g5
print *, g6

end
