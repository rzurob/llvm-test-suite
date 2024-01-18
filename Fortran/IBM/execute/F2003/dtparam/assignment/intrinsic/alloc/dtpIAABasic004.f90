!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABasic004
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (kind parameter) with allocatable component via intrinsic assignment
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP variable with a kind parameter, with allocatable
!*  components, we verify that the variables are initially unallocated, allocated after assignment,
!*  and deallocated before assignment. We can only verify the latter indirectly, by printing a
!*  message when something is finalized, which only happens here after structure constructors in
!*  exprs have been referenced, and after allocated instances of derived types are deallocated.
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

module dtpIAABasic004mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
   contains
     final :: f1, f4
  end type dka

contains

  subroutine f1(a)
    type(dka(1)) :: a
    if (allocated(a%ivar)) then
       print *, "f1:", a%k, kind(a%ivar), a%ivar
       deallocate(a%ivar)
    else
       print *, "f1:", a%k, kind(a%ivar), "not allocated"
    end if
  end subroutine f1

  subroutine f4(a)
    type(dka(4)) :: a
    if (allocated(a%ivar)) then
       print *, "f4:", a%k, kind(a%ivar), a%ivar
       deallocate(a%ivar)
    else
       print *, "f4:", a%k, kind(a%ivar), "not allocated"
    end if
  end subroutine f4

end module dtpIAABasic004mod



program dtpIAABasic004

  use dtpIAABasic004mod
  implicit none

  type(dka(1)), allocatable :: v1, v1a
  type(dka(4)), allocatable :: v4, v4a

  print *, allocated(v1), allocated(v1a)
  print *, "v1 = dka(1)(101)"
  v1 = dka(1)(101)
  print *, "v1a = v1 {v1=", allocated(v1), allocated(v1%ivar), v1%ivar, "}"
  v1a = v1
  v1a%ivar = 99
  v1 %ivar = 98
  print *, "v1 = v1a {v1=", allocated(v1), allocated(v1%ivar), v1%ivar, ", v1a=", allocated(v1a), allocated(v1a%ivar), v1a%ivar, "}"
  v1 = v1a
  print *

  print *, allocated(v4), allocated(v4a)
  print *, "v4 = dka(4)(40000004)"
  v4 = dka(4)(40000004)
  print *, "v4a = v4 {v4=", allocated(v4), allocated(v4%ivar), v4%ivar, "}"
  v4a = v4
  v4a%ivar = 30000003
  v4 %ivar = 20000002
  print *, "v4 = v4a {v4=", allocated(v4), allocated(v4%ivar), v4%ivar, ", v4a=", allocated(v4a), allocated(v4a%ivar), v4a%ivar, "}"
  v4 = v4a
  print *

  print *, allocated(v1),  allocated(v1%ivar),  v1%k,  kind(v1%ivar),  v1%ivar
  print *, allocated(v1a), allocated(v1a%ivar), v1a%k, kind(v1a%ivar), v1a%ivar
  print *, allocated(v4),  allocated(v4%ivar),  v4%k,  kind(v4%ivar),  v4%ivar
  print *, allocated(v4a), allocated(v4a%ivar), v4a%k, kind(v4a%ivar), v4a%ivar

end program dtpIAABasic004
