!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-05-22
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : allocate basic DTP array (container) with allocatable components via intrinsic assignment and check lower and upper bounds (in internal subroutine)
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : dtpIAABounds007 (<-dtpIAAArray007<-dtpIAABasic007<-dtpIAABasic006<-dtpIAABasic003<-dtpIAABasic002<-dtpIAABasic001)
!*
!*  DESCRIPTION
!*
!*  The simplest case of an allocatable DTP container array with a kind and a length
!*  parameter, but with references to types which themselves have allocatable components,
!*  This adaptation passes variables in to a single test subroutine, like 006.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAABoundsInternal007mod

  implicit none
  type dk(k)
     integer, kind :: k
     integer(k) :: ivar = 0
  end type dk

  type dl(l)
     integer, len :: l
     character(l) :: chvar = ''
     integer :: iarr(l) = 0
  end type dl

  type containera(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dk(k)), allocatable :: dkvar
     type(dl(l)), allocatable :: dlvar
   contains
     final :: fin1, fin4
  end type containera

contains

  subroutine fin1(a)
    type(containera(1,*)) :: a
    if (allocated(a%dkvar)) deallocate(a%dkvar)
    if (allocated(a%dlvar)) deallocate(a%dlvar)
  end subroutine fin1

  subroutine fin4(a)
    type(containera(4,*)) :: a
    if (allocated(a%dkvar)) deallocate(a%dkvar)
    if (allocated(a%dlvar)) deallocate(a%dlvar)
  end subroutine fin4

end module dtpIAABoundsInternal007mod



program dtpIAABoundsInternal007

  use dtpIAABoundsInternal007mod
  implicit none

  type(containera(1,:)), allocatable :: o1(:), o2(:,:)
  type(containera(4,:)), allocatable :: v1(:), v3(:,:,:)

  call test(o1,o2,v1,v3)

contains

  subroutine test (o1,o2,v1,v3)
    type(containera(1,:)), allocatable :: o1(:), o2(:,:)
    type(containera(4,:)), allocatable :: v1(:), v3(:,:,:)

    type(containera(1,2)) :: o1a(3), o1b(-4:-1), o2a(1,2), o2b(-1003:-1001,10:11)
    type(containera(1,4)) :: o0
    type(containera(4,5)) :: v1a(2), v1b(1:2), v3a(1,1,1), v3b(4:5,3:4,2:3)
    type(containera(4,4)) :: v0

    integer :: i, j, five(5)

    ! assign similar structure constructors to o1 and o2, then one of a greater length to o3, which we then assign to o2
    print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
    if (allocated(o1) .or. allocated(o2) .or. allocated(v1) .or. allocated(v3)) stop 2

    five = [1,3,5,7,9]
    o1a = [(containera(1,2)(dk(1)(100+i), dl(2)(repeat(achar(99+i),2),[50+i,500+i])), i=1,size(o1a))]
    o1b = [(containera(1,2)(dk(1)(10+i), dl(2)(repeat(achar(69+i),2),[550+i,5500+i])), i=lbound(o1b,1),ubound(o1b,1))]

    o2a = reshape([containera(1,2)(dk(1)(49), dl(2)('xy',[1,2])), containera(1,2)(dk(1)(50), dl(2)('za',[2,1]))], [1,2])
    o2b = reshape([(containera(1,2)(dk(1)(10+i), dl(2)('xx',[10+i,11+i])), i=1,6)], [3,2])

    o0  = containera(1,4)(dk(1)(99), dl(4)('uvwx',[42,24,-42,-24]))

    o1 = o1a
    o2 = o2a

    print *, lbound(o1), ":", ubound(o1), "::", lbound(o1a), ":", ubound(o1a)
    print *, lbound(o2), ":", ubound(o2), "::", lbound(o2a), ":", ubound(o2a)
    if (any(lbound(o1)/=lbound(o1a)) .or. any(ubound(o1)/=ubound(o1a)) &
        .or. any(lbound(o2)/=lbound(o2a)) .or. any(ubound(o2)/=ubound(o2a))) stop 3

    o1 = o1b
    o2 = o2b

    print *, lbound(o1), ":", ubound(o1), "::", lbound(o1b), ":", ubound(o1b)
    print *, lbound(o2), ":", ubound(o2), "::", lbound(o2b), ":", ubound(o2b)
    if (any(lbound(o1)/=lbound(o1b)) .or. any(ubound(o1)/=ubound(o1b)) &
        .or. any(lbound(o2)/=lbound(o2b)) .or. any(ubound(o2)/=ubound(o2b))) stop 4

    o1 = o0
    o2 = o0

    print *, lbound(o1), ":", ubound(o1), "::", lbound(o1b), ":", ubound(o1b)
    print *, lbound(o2), ":", ubound(o2), "::", lbound(o2b), ":", ubound(o2b)
    if (any(lbound(o1)/=lbound(o1b)) .or. any(ubound(o1)/=ubound(o1b)) &
        .or. any(lbound(o2)/=lbound(o2b)) .or. any(ubound(o2)/=ubound(o2b))) stop 5

    o1 = [containera(1,2)(dk(1)(1),dl(2)('jj',[12,13])), containera(1,2)(dk(1)(1),dl(2)('jj',[12,13]))]
    o2 = reshape([containera(1,2)(dk(1)(1), dl(2)('qq',[-1,-2])), containera(1,2)(dk(1)(1), dl(2)('qq',[-1,-2]))], [1,2])

    print *, lbound(o1), ":", ubound(o1), "::", 1, ":", 2
    print *, lbound(o2), ":", ubound(o2), "::", 1,1, ":", 1,2
    if (any(lbound(o1)/=1) .or. any(ubound(o1)/=[2]) &
        .or. any(lbound(o2)/=1) .or. any(ubound(o2)/=[1,2])) stop 6


    v1a = [(containera(4,5)(dk(4)(100+i),dl(5)('aaaaa',[(i+j,j=1,5)])), i=1,size(v1a))]
    v1b = [(containera(4,5)(dk(4)(-100+i),dl(5)('bbbbb',[(i+2*j,j=1,5)])), i=lbound(v1b,1),ubound(v1b,1))]

    v3a = containera(4,5)(dk(4)(49),dl(5)('ccccc',[1,2,3,4,5]))
    v3b = reshape([(containera(4,5)(dk(4)(10+i), dl(5)('ddddd', [(i-j,j=1,5)])), i=1,8)], [2,2,2])

    v0  = containera(4,4)(dk(4)(99),dl(4)('eeee',[2,3,4,5]))

    v1 = v1a
    v3 = v3a

    print *, lbound(v1), ":", ubound(v1), "::", lbound(v1a), ":", ubound(v1a)
    print *, lbound(v3), ":", ubound(v3), "::", lbound(v3a), ":", ubound(v3a)
    if (any(lbound(v1)/=lbound(v1a)) .or. any(ubound(v1)/=ubound(v1a)) &
        .or. any(lbound(v3)/=lbound(v3a)) .or. any(ubound(v3)/=ubound(v3a))) stop 7

    v1 = v1b
    v3 = v3b

    print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
    print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
    if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
        .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) stop 8

    v1 = v0
    v3 = v0

    print *, lbound(v1), ":", ubound(v1), "::", lbound(v1b), ":", ubound(v1b)
    print *, lbound(v3), ":", ubound(v3), "::", lbound(v3b), ":", ubound(v3b)
    if (any(lbound(v1)/=lbound(v1b)) .or. any(ubound(v1)/=ubound(v1b)) &
        .or. any(lbound(v3)/=lbound(v3b)) .or. any(ubound(v3)/=ubound(v3b))) stop 9

    v1 = [containera(4,5)(dk(4)(1),dl(5)('fffff',five)), containera(4,5)(dk(4)(2),dl(5)('ggggg',2*five)), containera(4,5)(dk(4)(3),dl(5)('hhhhh',3*five))]
    v3 = reshape([(containera(4,5)(dk(4)(i),dl(5)('kkkkk',4*five)), i=1,24)], [4,2,3])

    print *, lbound(v1), ":", ubound(v1), "::", 1, ":", 3
    print *, lbound(v3), ":", ubound(v3), "::", 1,1,1, ":", 4,2,3
    if (any(lbound(v1)/=1) .or. any(ubound(v1)/=[3]) &
        .or. any(lbound(v3)/=1) .or. any(ubound(v3)/=[4,2,3])) stop 10

    print *, allocated(o1), allocated(o2), allocated(v1), allocated(v3)
    if (.not.(allocated(o1) .and. allocated(o2) .and. allocated(v1) .and. allocated(v3))) stop 10

  end subroutine test

end program dtpIAABoundsInternal007
