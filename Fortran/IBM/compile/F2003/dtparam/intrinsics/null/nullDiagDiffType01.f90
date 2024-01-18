!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : nullDiagDiffType01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Sept. 26 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.88
!*  2. DIAGNOSE DIFFERENT TYPE,NULL(MOLD) SHOULD HAVE SAME CHARACTERISTICS AS MOLD
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(k)
      integer,kind :: k
      integer(k) :: i
   end type 
   type B(k)
      integer,kind :: k
      integer(k) :: i 
   end type
end module

program nullDiagDiffType01

  use m
  implicit none

  integer,pointer     :: i1
  integer,allocatable :: i2
  character,pointer   :: c1
  character,allocatable :: c2
  real,pointer        :: r1
  real,allocatable    :: r2
  logical,pointer     :: l1
  logical,allocatable :: l2


  type(A(4)),pointer     :: a1
  type(A(4)),allocatable :: a2
  type(B(4)),pointer     :: b1
  type(B(4)),allocatable :: b2

 
  i1=>null(r1)
  r1=>null(c1)
  l1=>null(i1)
  c1=>null(l1)

  i2=null(c1)
  r2=null(l1)
  l1=null(c2)
  c1=null(i2)  

  a1=>b1
  b1=>a1
  a2=b2
  b1=a1
end program

