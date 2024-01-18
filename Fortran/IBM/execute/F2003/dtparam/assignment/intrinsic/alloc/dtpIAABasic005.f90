!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABasic005
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (len parameter) with allocatable component via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAABasic002 (<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP variable with a length parameter, we verify
!*  that the variables are initially unallocated, allocated after assignment, and deallocated
!*  before assignment. We can only verify the latter indirectly, by printing a message when
!*  something is finalized, which only happens here after structure constructors in exprs
!*  have been referenced, and after allocated instances of derived types are deallocated.
!*  There are two important caveats about using finalization in this context:
!*  1. Where multiple objects may be finalized, the precise order of finalization
!*     may be processor dependent.  The program below has been constructed so as to
!*     avoid this situation.
!*  2. The entities below are in the main program, and are not finalized immediately
!*     before the END statement.  If the code were executed instead in a subroutine,
!*     additional finalizations would be performed (and the order in which each entity
!*     is finalized would not be guaranteed).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABasic005mod

  implicit none
  type dla(l)
     integer, len :: l
     character(l) :: label = ''
     integer, allocatable :: iarr(:)
   contains
     final :: fin
  end type dla

contains

  subroutine fin(a)
    type(dla(*)) :: a
    if (allocated(a%iarr)) then
       print *, "fin:", a%l, len(a%label), size(a%iarr), ">", a%label, "<", a%iarr
       deallocate(a%iarr)
    else
       print *, "fin:", a%l, len(a%label), 0, ">", a%label, "<, iarr not allocated"
    end if
  end subroutine fin

end module dtpIAABasic005mod



program dtpIAABasic005

  use dtpIAABasic005mod
  implicit none

  type(dla(:)), allocatable :: v1, v2, v3

  print *, allocated(v1), allocated(v2), allocated(v3)
  print *, "v1 = dla(1)('a',[1234321])"
  v1 = dla(1)('a',[1234321])
  print *, "v2 = v1 {v1=", allocated(v1), allocated(v1%iarr), v1%label, v1%iarr, "}"
  v2 = v1
  v2%label = 'b'
  v2%iarr  = [2345432]
  v1%label = 'c'
  v1%iarr  = [3456543]
  print *, "v1 = v2 {v1=", allocated(v1), allocated(v1%iarr), v1%label, v1%iarr, ", v2=", allocated(v2), allocated(v2%iarr), v2%label, v2%iarr, "}"
  v1 = v2
  print *

  print *, "v3 = dla(3)('def',[44444,55555,66666])"
  v3 = dla(3)('def',[44444,55555,66666])
  print *, "v2 = v3 {v2=", allocated(v2), allocated(v2%iarr), v2%label, v2%iarr, ", v3=", allocated(v3), allocated(v3%iarr), v3%label, v3%iarr, "}"
  v2 = v3
  print *

  print *, "v3 = v1 {v3=", allocated(v3), allocated(v3%iarr), v3%label, v3%iarr, ", v1=", allocated(v1), allocated(v1%iarr), v1%label, v1%iarr, "}"
  v3 = v1
  print *

  print *, allocated(v1), allocated(v1%iarr), v1%l, len(v1%label), size(v1%iarr), v1%label, v1%iarr
  print *, allocated(v2), allocated(v2%iarr), v2%l, len(v2%label), size(v2%iarr), v2%label, v2%iarr
  print *, allocated(v3), allocated(v3%iarr), v3%l, len(v3%label), size(v3%iarr), v3%label, v3%iarr

end program dtpIAABasic005
