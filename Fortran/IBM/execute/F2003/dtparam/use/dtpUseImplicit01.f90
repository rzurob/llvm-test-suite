!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseImplicit01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : USEd type in implicit statement
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : implicit
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that implicit types correspond to expectations.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseModule

  implicit none

  type, abstract :: allbase
  end type allbase

  type, extends(allbase), abstract :: tklbase
  end type tklbase

  type, extends(allbase), abstract :: tkbase
  end type tkbase

  type, extends(allbase), abstract :: tlbase
  end type tlbase

  type, extends(tklbase) :: tkl(k,l)
     integer, kind :: k
     integer, len  :: l
     integer(k) :: ifld(l) = -99
  end type tkl

  type, extends(tkbase) :: tk(k)
     integer, kind :: k
     integer(k) :: ifld
  end type tk

  type, extends(tlbase) :: tl(l)
     integer, len  :: l
     integer(1) :: ifld(l) = -42
  end type tl

end module dtpUseModule


program dtpUseImplicit01

  use :: dtpUseModule
  implicit type(tkl(4,:))(e), &
           type(tkl(4,5))(f), &
           type(tkl(2,3))(g), &
           type(tk(2))(o), &
           type(tk(4))(p), &
           type(tl(3))(q), &
           type(tl(5))(r), &
           type(tl(:))(s)
  pointer :: ep, ep2, fp, gp, op, pp, qp, rp, sp, sp2
  allocatable :: e2, s2
  target  :: f2, g2, o2, p2, q2, r2

  print *, "A: start"

  f1 % ifld = 11
  g1 % ifld = 12
  o1 % ifld = 13
  p1 % ifld = 14
  q1 % ifld = 15
  r1 % ifld = 16

  allocate(e2, source=f1)
  allocate(s2, source=r1)

  f2 % ifld = 21
  g2 % ifld = 22
  o2 % ifld = 23
  p2 % ifld = 24
  q2 % ifld = 25
  r2 % ifld = 26

  ep => f2
  fp => f2
  gp => g2
  op => o2
  pp => p2
  qp => q2
  rp => r2
  sp => r2

  allocate(ep2, source=f2)
  allocate(sp2, source=r2)

  print *, "B: display"

  print *, "C:",   21, ep, kind(ep%ifld), ep % k, ep % l
  print *, "D:",   21, ep2, kind(ep2%ifld), ep2 % k, ep2 % l
  print *, "E:",   11, e2, kind(e2%ifld), e2 % k, e2 % l
  print *, "F:",   11, f1, kind(f1%ifld), f1 % k, f1 % l
  print *, "G:",   12, g1, kind(g1%ifld), g1 % k, g1 % l
  print *, "H:",   13, o1, kind(o1%ifld), o1 % k
  print *, "I:",   14, p1, kind(p1%ifld), p1 % k
  print *, "J:",   15, q1, kind(q1%ifld), q1 % l
  print *, "K:",   16, r1, kind(r1%ifld), r1 % l

  print *, "L:",   21, f2, kind(f2%ifld), f2 % k, f2 % l
  print *, "M:",   22, g2, kind(g2%ifld), g2 % k, g2 % l
  print *, "N:",   23, o2, kind(o2%ifld), o2 % k
  print *, "O:",   24, p2, kind(p2%ifld), p2 % k
  print *, "P:",   25, q2, kind(q2%ifld), q2 % l
  print *, "Q:",   26, r2, kind(r2%ifld), r2 % l

  print *, "S:",   26, sp, kind(sp%ifld), sp % l
  print *, "T:",   26, sp2, kind(sp2%ifld), sp2 % l
  print *, "U:",   16, s2, kind(s2%ifld), s2 % l

  print *, "V:",  -99, f3, kind(f3%ifld), f3 % k, f3 % l
  print *, "W:",  -99, g3, kind(g3%ifld), g3 % k, g3 % l
  print *, "X:",  -42, q3, kind(q3%ifld), q3 % l
  print *, "Y:",  -42, r3, kind(r3%ifld), r3 % l

  print *, "Z: end"

end program dtpUseImplicit01
