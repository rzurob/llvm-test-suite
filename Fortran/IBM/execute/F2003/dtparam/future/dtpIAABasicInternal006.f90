!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAABasicInternal006
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-05-22
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP variable (container) with indirectly allocatable components via intrinsic assignment (in internal subroutine)
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*  ADAPTED FROM               : dtpIAABasic006 (<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP container variable with a kind and a length
!*  parameter, but with references to types which themselves have allocatable components:
!*  as Array006, but processed in an internal subroutine.  That also means that some
!*  finalizations are carried out that are not done when the main program has control.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABasicInternal006mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
   contains
     final :: f1, f4
  end type dka

  type dla(l)
     integer, len :: l
     character(l) :: label = ''
     integer, allocatable :: iarr(:)
   contains
     final :: fin
  end type dla

  type acontainer(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dka(k)) :: dkvar
     type(dla(l)) :: dlvar
   contains
     final :: fin1, fin4
  end type acontainer

  integer, save :: step = 0

contains

  subroutine f1(a)
    type(dka(1)) :: a
    if (allocated(a%ivar)) then
       print *, step, "f1:", a%k, kind(a%ivar), a%ivar
       deallocate(a%ivar)
    else
       print *, step, "f1:", a%k, kind(a%ivar), "not allocated"
    end if
  end subroutine f1

  subroutine f4(a)
    type(dka(4)) :: a
    if (allocated(a%ivar)) then
       print *, step, "f4:", a%k, kind(a%ivar), a%ivar
       deallocate(a%ivar)
    else
       print *, step, "f4:", a%k, kind(a%ivar), "not allocated"
    end if
  end subroutine f4

  subroutine fin(a)
    type(dla(*)) :: a
    if (allocated(a%iarr)) then
       print *, step, "fin:", a%l, len(a%label), size(a%iarr), ">", a%label, "<", a%iarr
       deallocate(a%iarr)
    else
       print *, step, "fin:", a%l, len(a%label), 0, ">", a%label, "<, iarr not allocated"
    end if
  end subroutine fin

  subroutine fin1(a)
    type(acontainer(1,*)) :: a
    if (allocated(a%dkvar%ivar) .and. allocated(a%dlvar%iarr)) then
       print *, step, "fin1:", a%l, a%dkvar%ivar, a%dlvar%label, size(a%dlvar%iarr), a%dlvar%iarr
    else if(allocated(a%dkvar%ivar)) then
       print *, step, "fin1:", a%l, a%dkvar%ivar, a%dlvar%label, "dlvar.iarr not allocated"
    else if(allocated(a%dlvar%iarr)) then
       print *, step, "fin1:", a%l, "dkvar.ivar not allocated", a%dlvar%label, size(a%dlvar%iarr), a%dlvar%iarr
    else
       print *, step, "fin1:", a%l, a%dlvar%label, "no allocations"
    end if
    if (a%k /= 1 .or. a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) stop 5
    if (a%dlvar%l /= a%l .or. len(a%dlvar%label) /= a%l) stop 6
  end subroutine fin1

  subroutine fin4(a)
    type(acontainer(4,*)) :: a
    if (allocated(a%dkvar%ivar) .and. allocated(a%dlvar%iarr)) then
       print *, step, "fin4:", a%l, a%dkvar%ivar, a%dlvar%label, size(a%dlvar%iarr), a%dlvar%iarr
    else if(allocated(a%dkvar%ivar)) then
       print *, step, "fin4:", a%l, a%dkvar%ivar, a%dlvar%label, "dlvar.iarr not allocated"
    else if(allocated(a%dlvar%iarr)) then
       print *, step, "fin4:", a%l, "dkvar.ivar not allocated", a%dlvar%label, size(a%dlvar%iarr), a%dlvar%iarr
    else
       print *, step, "fin4:", a%l, a%dlvar%label, "no allocations"
    end if
    if (a%k /= 4 .or. a%dkvar%k /= a%k .or. kind(a%dkvar%ivar) /= a%k) stop 7
    if (a%dlvar%l /= a%l .or. len(a%dlvar%label) /= a%l) stop 8
  end subroutine fin4

end module dtpIAABasicInternal006mod



program dtpIAABasicInternal006

  use dtpIAABasicInternal006mod
  implicit none

  type(acontainer(1,:)), allocatable :: o1, o2
  type(acontainer(4,:)), allocatable :: v1, v2

  call test(o1,o2,v1,v2)

contains

  subroutine test (o1,o2,v1,v2)
    type(acontainer(1,:)), allocatable :: o1, o2, o3
    type(acontainer(4,:)), allocatable :: v1, v2, v3

    ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
    ! start at step 10 so that the later sort doesn't put "12 ..." before "3 ...":
    step = 10
    print *, step, allocated(o1), allocated(o2), allocated(o3)

    step = 11
    print *, step, "o1 = acontainer(1,2)(dka(1)(34),dla(2)('ab',[35,36]))"

    step = 12
    o1 = acontainer(1,2)(dka(1)(34),dla(2)('ab',[35,36]))

    step = 13
    print *, step, "o2 = acontainer(1,2)(dka(1)(37),dla(2)('cd',[38,39]))"

    step = 14
    o2 = acontainer(1,2)(dka(1)(37),dla(2)('cd',[38,39]))

    step = 15
    print *, step, "o1 = o2 {o1=", allocated(o1), o1%dkvar%ivar, o1%dlvar%label, o1%dlvar%iarr, &
             ", o2=", allocated(o2), o2%dkvar%ivar, o2%dlvar%label, o2%dlvar%iarr, "}"

    step = 16
    o1 = o2

    step = 17
    print *, step, "o3 = acontainer(1,3)(dka(1)(40),dla(3)('efg',[41,42,43]))"
    step = 18
    o3 = acontainer(1,3)(dka(1)(40),dla(3)('efg',[41,42,43]))

    step = 19
    print *, step, "o2 = o3 {o2=", allocated(o2), o2%dkvar%ivar, o2%dlvar%label, o2%dlvar%iarr, &
             ", o3=", allocated(o3), o3%dkvar%ivar, o3%dlvar%label, o3%dlvar%iarr, "}"
    step = 20
    o2 = o3

    ! Repeat the above with a acontainer of kind 4
    step = 21
    print *, step, allocated(v1), allocated(v2), allocated(v3)

    step = 22
    print *, step, "v1 = acontainer(4,2)(dka(4)(44),dla(2)('AB',[45,46]))"

    step = 23
    v1 = acontainer(4,2)(dka(4)(44),dla(2)('AB',[45,46]))

    step = 24
    print *, step, "v2 = acontainer(4,2)(dka(4)(47),dla(2)('CD',[48,49]))"

    step = 25
    v2 = acontainer(4,2)(dka(4)(47),dla(2)('CD',[48,49]))

    step = 26
    print *, step, "v1 = v2 {v1=", allocated(v1), v1%dkvar%ivar, v1%dlvar%label, v1%dlvar%iarr, &
             ", v2=", allocated(v2), v2%dkvar%ivar, v2%dlvar%label, v2%dlvar%iarr, "}"

    step = 27
    v1 = v2

    step = 28
    print *, step, "v3 = acontainer(4,3)(dka(4)(50),dla(3)('EFG',[51,52,53]))"
    step = 29
    v3 = acontainer(4,3)(dka(4)(50),dla(3)('EFG',[51,52,53]))

    step = 30
    print *, step, "v2 = v3 {v2=", allocated(v2), v2%dkvar%ivar, v2%dlvar%label, v2%dlvar%iarr, &
             ", v3=", allocated(v3), v3%dkvar%ivar, v3%dlvar%label, v3%dlvar%iarr, "}"
    step = 31
    v2 = v3

    ! Just use a higher number in place of "step":
    print *, 91, allocated(o1), o1%k, o1%l, len(o1%dlvar%label), size(o1%dlvar%iarr), o1%dkvar%ivar, o1%dlvar%label, o1%dlvar%iarr
    print *, 92, allocated(o2), o2%k, o2%l, len(o2%dlvar%label), size(o2%dlvar%iarr), o2%dkvar%ivar, o2%dlvar%label, o2%dlvar%iarr
    print *, 93, allocated(o3), o3%k, o3%l, len(o3%dlvar%label), size(o3%dlvar%iarr), o3%dkvar%ivar, o3%dlvar%label, o3%dlvar%iarr

    print *, 94, allocated(v1), v1%k, v1%l, len(v1%dlvar%label), size(v1%dlvar%iarr), v1%dkvar%ivar, v1%dlvar%label, v1%dlvar%iarr
    print *, 95, allocated(v2), v2%k, v2%l, len(v2%dlvar%label), size(v2%dlvar%iarr), v2%dkvar%ivar, v2%dlvar%label, v2%dlvar%iarr
    print *, 96, allocated(v3), v3%k, v3%l, len(v3%dlvar%label), size(v3%dlvar%iarr), v3%dkvar%ivar, v3%dlvar%label, v3%dlvar%iarr

    step = 99

  end subroutine test

end program dtpIAABasicInternal006
