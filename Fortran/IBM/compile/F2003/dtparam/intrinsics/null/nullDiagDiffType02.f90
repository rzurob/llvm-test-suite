!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 26 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : NULL([MOLD])
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. SECTION 13.7.88
!*  2. DIAGNOSE DIFFERENT TYPE PARAMETER,NULL(MOLD) SHOULD HAVE SAME CHARACTERISTIC AS MOLD
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(k,l)
      integer,kind :: k
      integer,len  :: l
   end type
end module

program nullDiagDiffType01

  use m
  implicit none

  integer(2),pointer     :: i1
  integer(4),pointer     :: i2
  real(4),pointer        :: r1
  real(8),pointer        :: r2
  logical(1),pointer     :: l1
  logical(2),pointer     :: l2

  type(A(4,2)),pointer     :: a1
  type(A(2,2)),pointer     :: a2
  type(A(4,4)),pointer     :: a3

  type(A(4,2)),allocatable :: a4
  type(A(2,2)),allocatable :: a5
  type(A(4,4)),allocatable :: a6

  i1=>null(i2)
  r1=>null(r2)
  l1=>null(l2)
  a1=>null(a2)
  a3=>null(a1)

  a4=null(a5)
  a4=null(a6)

end program

