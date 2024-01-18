!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : extends, inheritance
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that kind and length parameters are correctly applied to descendants of DTP types.
!*  This differs from dtpUseExtends01 in that the types are extended in internal and external
!*  procedures.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseModule

  implicit none

  type :: NoParms
  end type NoParms

  type, extends(NoParms) :: OneParm (k)
    integer, kind :: k
    integer(k) :: ifld = -1
  end type OneParm

  type, extends(OneParm) :: TwoParms (l)
    integer, len :: l
    character(l) :: chfld = 'nopqrstuvwxyz'
    integer(k) :: iarr(l) = -2
  end type TwoParms

  type, extends(TwoParms) :: ThreeParms (k2)
    integer, kind :: k2
    real(k2) :: rarr(l) = -1.3
  end type ThreeParms

end module dtpUseModule


module other
  use :: dtpUseModule, only: Base => NoParms, Child => OneParm, GChild => TwoParms, GGChild => ThreeParms

  type, extends(Base) :: OneParm (k)
    integer, kind :: k
    integer(k) :: ifld = -4
  end type OneParm

  type, extends(Child) :: TwoParms (l)
    integer, len :: l
    character(l) :: chfld = 'abcdefghijklm'
    integer(k) :: iarr(l) = -5
  end type TwoParms

  type, extends(GChild) :: ThreeParms (k2)
    integer, kind :: k2
    real(k2) :: rarr(l) = -6.3
  end type ThreeParms

end module other

program dtpUseExtends03

  implicit none

  print *, "int:"
  call intSub
  print *, "ext:"
  call extSub
  print *, "end"

contains

  subroutine intSub
    use other

    type, extends(Base) :: Derived(k)
      integer, kind :: k
      integer(k) :: ifld = -4
    end type Derived

    type, extends(Derived) :: Derived2 (l)
      integer, len :: l
      character(l) :: chfld = 'abcdefghijklm'
      integer(k) :: iarr(l) = -5
    end type Derived2

    type, extends(Derived2) :: Derived3 (k2)
      integer, kind :: k2
      real(k2) :: rarr(l) = -6.3
    end type Derived3

    type(Base)              :: bv
    type(Child(2))          :: cv
    type(GChild(2,3))       :: gcv
    type(GGChild(2,5,4))    :: ggcv

    type(OneParm(8))        :: p1v
    type(TwoParms(1,5))     :: p2v
    type(ThreeParms(4,5,8)) :: p3v

    type(Derived(1))        :: d1v
    type(Derived2(8,3))     :: d2v
    type(Derived3(8,5,4))   :: d3v

    print *, cv, cv % k, kind(cv % ifld)
    print *, gcv, gcv % k, gcv % l, kind(gcv % ifld), kind(gcv % chfld), kind(gcv % iarr), &
         len(gcv % chfld), size(gcv % iarr)
    print *, ggcv, ggcv % k, ggcv % l, ggcv % k2, kind(ggcv % ifld), kind(ggcv % chfld), kind(ggcv % iarr), kind(ggcv % rarr), &
         len(ggcv % chfld), size(ggcv % iarr), size(ggcv % rarr)

    print *, p1v, p1v % k, kind(p1v % ifld)
    print *, p2v, p2v % k, p2v % l, kind(p2v % ifld), kind(p2v % chfld), kind(p2v % iarr), &
         len(p2v % chfld), size(p2v % iarr)
    print *, p3v, p3v % k, p3v % l, p3v % k2, kind(p3v % ifld), kind(p3v % chfld), kind(p3v % iarr), kind(p3v % rarr), &
         len(p3v % chfld), size(p3v % iarr), size(p3v % rarr)

    print *, d1v, d1v % k, kind(d1v % ifld)
    print *, d2v, d2v % k, d2v % l, kind(d2v % ifld), kind(d2v % chfld), kind(d2v % iarr), &
         len(d2v % chfld), size(d2v % iarr)
    print *, d3v, d3v % k, d3v % l, d3v % k2, kind(d3v % ifld), kind(d3v % chfld), kind(d3v % iarr), kind(d3v % rarr), &
         len(d3v % chfld), size(d3v % iarr), size(d3v % rarr)
  end subroutine intSub

end program dtpUseExtends03


subroutine extSub
  use other

  type, extends(Base) :: Derived(k)
    integer, kind :: k
    integer(k) :: ifld = -4
  end type Derived

  type, extends(Derived) :: Derived2 (l)
    integer, len :: l
    character(l) :: chfld = 'abcdefghijklm'
    integer(k) :: iarr(l) = -5
  end type Derived2

  type, extends(Derived2) :: Derived3 (k2)
    integer, kind :: k2
    real(k2) :: rarr(l) = -6.3
  end type Derived3

  type(Base)              :: bv
  type(Child(2))          :: cv
  type(GChild(2,3))       :: gcv
  type(GGChild(2,5,4))    :: ggcv

  type(OneParm(8))        :: p1v
  type(TwoParms(1,5))     :: p2v
  type(ThreeParms(4,5,8)) :: p3v

  type(Derived(1))        :: d1v
  type(Derived2(8,3))     :: d2v
  type(Derived3(8,5,4))   :: d3v

  print *, cv, cv % k, kind(cv % ifld)
  print *, gcv, gcv % k, gcv % l, kind(gcv % ifld), kind(gcv % chfld), kind(gcv % iarr), &
       len(gcv % chfld), size(gcv % iarr)
  print *, ggcv, ggcv % k, ggcv % l, ggcv % k2, kind(ggcv % ifld), kind(ggcv % chfld), kind(ggcv % iarr), kind(ggcv % rarr), &
       len(ggcv % chfld), size(ggcv % iarr), size(ggcv % rarr)

  print *, p1v, p1v % k, kind(p1v % ifld)
  print *, p2v, p2v % k, p2v % l, kind(p2v % ifld), kind(p2v % chfld), kind(p2v % iarr), &
       len(p2v % chfld), size(p2v % iarr)
  print *, p3v, p3v % k, p3v % l, p3v % k2, kind(p3v % ifld), kind(p3v % chfld), kind(p3v % iarr), kind(p3v % rarr), &
       len(p3v % chfld), size(p3v % iarr), size(p3v % rarr)

  print *, d1v, d1v % k, kind(d1v % ifld)
  print *, d2v, d2v % k, d2v % l, kind(d2v % ifld), kind(d2v % chfld), kind(d2v % iarr), &
       len(d2v % chfld), size(d2v % iarr)
  print *, d3v, d3v % k, d3v % l, d3v % k2, kind(d3v % ifld), kind(d3v % chfld), kind(d3v % iarr), kind(d3v % rarr), &
       len(d3v % chfld), size(d3v % iarr), size(d3v % rarr)
end subroutine extSub
