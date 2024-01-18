!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABasicPointer007
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (container) with allocatable components via intrinsic assignment and assign to pointer
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAABasic007 (<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  An allocatable DTP container variable with a kind and a length parameter, but with
!*  references to types which themselves have allocatable components:
!*  we assign references to these to a pointer, and verify that it does not reallocate.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABasicPointer007mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
   contains
     final :: f1, f4
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer :: iarr(l) = 0
   contains
     final :: fin
  end type dl

  type containera(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)), allocatable :: dkvar
     type(dl(l)), allocatable :: dlvar
   contains
     final :: fin1, fin4
  end type containera

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dk(1)) :: a
    print *, step, "f1:", a%k, kind(a%ivar), a%ivar
  end subroutine f1

  subroutine f4(a)
    type(dk(4)) :: a
    print *, step, "f4:", a%k, kind(a%ivar), a%ivar
  end subroutine f4

  subroutine fin(a)
    type(dl(*)) :: a
    print *, step, "fin:", a%l, len(a%chvar), size(a%iarr), ">", a%chvar, "<", a%iarr
  end subroutine fin

  subroutine fin1(a)
    type(containera(1,*)) :: a
    if (allocated(a%dkvar) .and. allocated(a%dlvar)) then
       print *, step, "fin1:", a%l, a%dkvar%ivar, a%dlvar%chvar, size(a%dlvar%iarr), a%dlvar%iarr
       if (a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) stop 2
       if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l) stop 3
       deallocate(a%dkvar, a%dlvar)
    else if(allocated(a%dkvar)) then
       print *, step, "fin1:", a%l, a%dkvar%ivar, "dlvar not allocated"
       if (a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) stop 4
       deallocate(a%dkvar)
    else if(allocated(a%dlvar)) then
       print *, step, "fin1:", a%l, "dkvar not allocated", a%dlvar%chvar, size(a%dlvar%iarr), a%dlvar%iarr
       if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l) stop 5
       deallocate(a%dlvar)
    else
       print *, step, "fin1:", a%l, "no allocations"
    end if
    if (a%k /= 1) stop 9
  end subroutine fin1

  subroutine fin4(a)
    type(containera(4,*)) :: a
    if (allocated(a%dkvar) .and. allocated(a%dlvar)) then
       print *, step, "fin4:", a%l, a%dkvar%ivar, a%dlvar%chvar, size(a%dlvar%iarr), a%dlvar%iarr
       if (a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) stop 6
       if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l) stop 7
       deallocate(a%dkvar, a%dlvar)
    else if(allocated(a%dkvar)) then
       print *, step, "fin4:", a%l, a%dkvar%ivar, "dlvar not allocated"
       if (a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) stop 8
       deallocate(a%dkvar)
    else if(allocated(a%dlvar)) then
       print *, step, "fin4:", a%l, "dkvar not allocated", a%dlvar%chvar, size(a%dlvar%iarr), a%dlvar%iarr
       if (a%dlvar%l /= a%l .or. len(a%dlvar%chvar) /= a%l) stop 9
       deallocate(a%dlvar)
    else
       print *, step, "fin4:", a%l, "no allocations"
    end if
    if (a%k /= 4) stop 10
  end subroutine fin4

end module dtpIAABasicPointer007mod



program dtpIAABasicPointer007

  use dtpIAABasicPointer007mod
  implicit none

  type(containera(1,:)), allocatable, target :: o1, o2
  type(containera(4,:)), allocatable, target :: v1, v2
  type(containera(1,:)), pointer :: o1p, o2p
  type(containera(4,:)), pointer :: v1p, v2p

  ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
  ! start at step 10 so that the later sort doesn't put "12 ..." before "3 ...":

  step = -1
  nullify(o1p, o2p, v1p, v2p) ! no finalization should happen here, so we should see no output

  step = 10
  print *, step, allocated(o1), allocated(o2), associated(o1p), associated(o2p)

  step = 11
  print *, step, "o1 = containera(1,2)(dk(1)(34),dl(2)('ab',[35,36]))"
  step = 12
  o1 = containera(1,2)(dk(1)(34),dl(2)('ab',[35,36]))
  step = 13
  print *, step, "o2 = containera(1,2)(dk(1)(37),dl(2)('cd',[38,39]))"
  step = 14
  o2 = containera(1,2)(dk(1)(37),dl(2)('cd',[38,39]))

  step = 15
  print *, step, "o1 = o2 {o1=", allocated(o1), o1%dkvar%ivar, o1%dlvar%chvar, o1%dlvar%iarr, &
           ", o2=", allocated(o2), o2%dkvar%ivar, o2%dlvar%chvar, o2%dlvar%iarr, "}"
  step = 16
  o1 = o2

  step = -1
  o1p => o1 ! no reallocation, so no finalization, so no output
  o2p => o2
  o1p => o2p
  o2p => o1

  ! Repeat the above with a containera of kind 4
  step = 21
  print *, step, allocated(v1), allocated(v2), associated(v1p), associated(v2p)

  step = 22
  print *, step, "v1 = containera(4,2)(dk(4)(44),dl(2)('AB',[45,46]))"
  step = 23
  v1 = containera(4,2)(dk(4)(44),dl(2)('AB',[45,46]))
  step = 24
  print *, step, "v2 = containera(4,2)(dk(4)(47),dl(2)('CD',[48,49]))"
  step = 25
  v2 = containera(4,2)(dk(4)(47),dl(2)('CD',[48,49]))

  step = -1
  v1p => v1 ! no reallocation, so no finalization, so no output
  v2p => v2
  v1p => v2p
  v2p => v1


  ! Just use a higher number in place of "step":
  print *, 91, allocated(o1), o1%k, o1%l, len(o1%dlvar%chvar), size(o1%dlvar%iarr), o1%dkvar%ivar, o1%dlvar%chvar, o1%dlvar%iarr
  print *, 92, allocated(o2), o2%k, o2%l, len(o2%dlvar%chvar), size(o2%dlvar%iarr), o2%dkvar%ivar, o2%dlvar%chvar, o2%dlvar%iarr
  print *, 93, associated(o1p), o1p%k, o1p%l, len(o1p%dlvar%chvar), size(o1p%dlvar%iarr), o1p%dkvar%ivar, o1p%dlvar%chvar, o1p%dlvar%iarr
  print *, 94, associated(o2p), o2p%k, o2p%l, len(o2p%dlvar%chvar), size(o2p%dlvar%iarr), o2p%dkvar%ivar, o2p%dlvar%chvar, o2p%dlvar%iarr

  print *, 95, allocated(v1), v1%k, v1%l, len(v1%dlvar%chvar), size(v1%dlvar%iarr), v1%dkvar%ivar, v1%dlvar%chvar, v1%dlvar%iarr
  print *, 96, allocated(v2), v2%k, v2%l, len(v2%dlvar%chvar), size(v2%dlvar%iarr), v2%dkvar%ivar, v2%dlvar%chvar, v2%dlvar%iarr
  print *, 97, associated(v1p), v1p%k, v1p%l, len(v1p%dlvar%chvar), size(v1p%dlvar%iarr), v1p%dkvar%ivar, v1p%dlvar%chvar, v1p%dlvar%iarr
  print *, 98, associated(v2p), v2p%k, v2p%l, len(v2p%dlvar%chvar), size(v2p%dlvar%iarr), v2p%dkvar%ivar, v2p%dlvar%chvar, v2p%dlvar%iarr

end program dtpIAABasicPointer007
