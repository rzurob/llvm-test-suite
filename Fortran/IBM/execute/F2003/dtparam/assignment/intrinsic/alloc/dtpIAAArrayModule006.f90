!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate DTP array (container) with indirectly allocatable components via intrinsic assignment (in module subroutine)
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAAArrayInternal006 (<-dtpIAAArray006<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP container array with a kind and a length
!*  parameter, but with references to types which themselves have allocatable components:
!*  as Array006, but processed in an internal subroutine.  That also means that some
!*  finalizations are carried out that are not done when the main program has control.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAArrayModule006mod

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
     final :: fin1, fin1arr, fin4, fin4arr
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

  subroutine fin1arr(a)
    type(acontainer(1,*)) :: a(:)
    print *, step, "fin1arr:", a%k, a%l, size(a), a%dlvar%label
  end subroutine fin1arr

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

  subroutine fin4arr(a)
    type(acontainer(4,*)) :: a(:)
    print *, step, "fin4arr:", a%k, a%l, size(a), a%dlvar%label
  end subroutine fin4arr


  subroutine test (o1,o2,v1,v3)
    type(acontainer(1,:)), allocatable :: o1(:), o2(:), o3(:)
    type(acontainer(4,:)), allocatable :: v1(:), v2(:), v3(:)

    type(acontainer(1,4)) :: o4
    type(acontainer(4,4)) :: v4
    integer :: i

    ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
    ! start at step 10 so that the later sort doesn't put "12 ..." before "3 ...":
    step = 10
    print *, step, allocated(o1), allocated(o2), allocated(o3)

    step = 11
    print *, step, "o1 = [acontainer(1,2)(dka(1)(34),dla(2)('ab',[35,36]))]"
    step = 12
    o1 = [acontainer(1,2)(dka(1)(34),dla(2)('ab',[35,36]))]

    step = 13
    print *, step, "o2 = [acontainer(1,2)(dka(1)(37),dla(2)('cd',[38,39]))]"
    step = 14
    o2 = [acontainer(1,2)(dka(1)(37),dla(2)('cd',[38,39]))]

    step = 15
    print *, step, "o1 = o2 {o1=", allocated(o1), (o1(i)%dkvar%ivar, o1(i)%dlvar%label, o1(i)%dlvar%iarr, i=1,size(o1)), &
             ", o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%label, o2(i)%dlvar%iarr, i=1,size(o2)), "}"
    step = 16
    o1 = o2

    step = 17
    print *, step, "o3 = [acontainer(1,3)(dka(1)(40),dla(3)('efg',[41,42,43])),acontainer(1,3)(dka(1)(-40),dla(3)('gfe',[-41,-42,-43]))]"
    step = 18
    o3 = [acontainer(1,3)(dka(1)(40),dla(3)('efg',[41,42,43])),acontainer(1,3)(dka(1)(-40),dla(3)('gfe',[-41,-42,-43]))]

    step = 19
    print *, step, "o2 = o3 {o2=", allocated(o2), (o2(i)%dkvar%ivar, o2(i)%dlvar%label, o2(i)%dlvar%iarr, i=1,size(o2)), &
             ", o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%label, o3(i)%dlvar%iarr, i=1,size(o3)), "}"
    step = 20
    o2 = o3

    step = 21
    print *, step, "o4 = acontainer(1,4)(dka(1)(60),dla(4)('hijk',[61,62,63,64]))"
    step = 22
    o4 = acontainer(1,4)(dka(1)(60),dla(4)('hijk',[61,62,63,64]))

    step = 23
    print *, step, "o3 = o4 {o3=", allocated(o3), (o3(i)%dkvar%ivar, o3(i)%dlvar%label, o3(i)%dlvar%iarr, i=1,size(o3)), &
             ", o4=", o4%dkvar%ivar, o4%dlvar%label, o4%dlvar%iarr, "}"
    step = 24
    o3 = o4

    ! Repeat the above with an acontainer of kind 4
    step = 25
    print *, step, allocated(v1), allocated(v2), allocated(v3)

    step = 26
    print *, step, "v1 = [acontainer(4,2)(dka(4)(44),dla(2)('AB',[45,46]))]"
    step = 27
    v1 = [acontainer(4,2)(dka(4)(44),dla(2)('AB',[45,46]))]

    step = 28
    print *, step, "v2 = [acontainer(4,2)(dka(4)(47),dla(2)('CD',[48,49]))]"
    step = 29
    v2 = [acontainer(4,2)(dka(4)(47),dla(2)('CD',[48,49]))]

    step = 30
    print *, step, "v1 = v2 {v1=", allocated(v1), (v1(i)%dkvar%ivar, v1(i)%dlvar%label, v1(i)%dlvar%iarr, i=1,size(v1)), &
             ", v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%label, v2(i)%dlvar%iarr, i=1,size(v2)), "}"
    step = 31
    v1 = v2

    step = 32
    print *, step, "v3 = [acontainer(4,3)(dka(4)(50),dla(3)('EFG',[51,52,53])),acontainer(4,3)(dka(4)(-50),dla(3)('GFE',[-51,-52,-53]))]"
    step = 33
    v3 = [acontainer(4,3)(dka(4)(50),dla(3)('EFG',[51,52,53])),acontainer(4,3)(dka(4)(-50),dla(3)('GFE',[-51,-52,-53]))]

    step = 34
    print *, step, "v2 = v3 {v2=", allocated(v2), (v2(i)%dkvar%ivar, v2(i)%dlvar%label, v2(i)%dlvar%iarr, i=1,size(v2)), &
             ", v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%label, v3(i)%dlvar%iarr, i=1,size(v3)), "}"
    step = 35
    v2 = v3

    step = 36
    print *, step, "v4 = acontainer(4,4)(dka(4)(70),dla(4)('lmno',[71,72,73,74]))"
    step = 37
    v4 = acontainer(4,4)(dka(4)(70),dla(4)('lmno',[71,72,73,74]))

    step = 38
    print *, step, "v3 = v4 {v3=", allocated(v3), (v3(i)%dkvar%ivar, v3(i)%dlvar%label, v3(i)%dlvar%iarr, i=1,size(v3)), &
             ", v4=", o4%dkvar%ivar, o4%dlvar%label, o4%dlvar%iarr, "}"
    step = 39
    v3 = v4

    ! Just use a higher number in place of "step":
    print *, 91, allocated(o1), o1%k, o1%l, (len(o1(i)%dlvar%label), size(o1(i)%dlvar%iarr), o1(i)%dkvar%ivar, o1(i)%dlvar%label, o1(i)%dlvar%iarr, i=1,size(o1))
    print *, 92, allocated(o2), o2%k, o2%l, (len(o2(i)%dlvar%label), size(o2(i)%dlvar%iarr), o2(i)%dkvar%ivar, o2(i)%dlvar%label, o2(i)%dlvar%iarr, i=1,size(o2))
    print *, 93, allocated(o3), o3%k, o3%l, (len(o3(i)%dlvar%label), size(o3(i)%dlvar%iarr), o3(i)%dkvar%ivar, o3(i)%dlvar%label, o3(i)%dlvar%iarr, i=1,size(o3))

    print *, 94, allocated(v1), v1%k, v1%l, (len(v1(i)%dlvar%label), size(v1(i)%dlvar%iarr), v1(i)%dkvar%ivar, v1(i)%dlvar%label, v1(i)%dlvar%iarr, i=1,size(v1))
    print *, 95, allocated(v2), v2%k, v2%l, (len(v2(i)%dlvar%label), size(v2(i)%dlvar%iarr), v2(i)%dkvar%ivar, v2(i)%dlvar%label, v2(i)%dlvar%iarr, i=1,size(v2))
    print *, 96, allocated(v3), v3%k, v3%l, (len(v3(i)%dlvar%label), size(v3(i)%dlvar%iarr), v3(i)%dkvar%ivar, v3(i)%dlvar%label, v3(i)%dlvar%iarr, i=1,size(v3))

    step = 40

  end subroutine test

end module dtpIAAArrayModule006mod



program dtpIAAArrayModule006

  use dtpIAAArrayModule006mod
  implicit none

  type(acontainer(1,:)), allocatable :: o1(:), o2(:), o3(:)
  type(acontainer(4,:)), allocatable :: v1(:), v2(:), v3(:)

  call test(o1,o2,v1,v3)

end program dtpIAAArrayModule006
