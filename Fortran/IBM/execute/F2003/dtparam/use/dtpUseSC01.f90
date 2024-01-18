!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSC01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : structure constructor
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : structure constructor
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Test the structure constructor.
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
    real(k2-k) :: rarr(l) = -1.3
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
    real(k2-k) :: rarr(l) = -6.3
  end type ThreeParms

end module other

program dtpUseSC01

  use other
  implicit none

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
    real(k2-k) :: rarr(l) = -6.3
  end type Derived3

  type(Child(2))          :: cv
  type(GChild(2,3))       :: gcv
  type(GGChild(2,5,6))    :: ggcv

  type(OneParm(8))        :: p1v
  type(TwoParms(1,5))     :: p2v
  type(ThreeParms(4,5,12)):: p3v

  type(Derived(1))        :: d1v
  type(Derived2(8,3))     :: d2v
  type(Derived3(8,5,12))  :: d3v

  integer(2) :: i
  integer(2), parameter :: i_const = 1

  cv = Child(2)(121)
  gcv = GChild(2,3)(99,'xyzq', [1025, 1029, 2054])
  ggcv = GGChild(2,5,6)(3013,'abcdex',[10900, 10899, 10901, 10888, 11234],[4.1, 5.1, 6.1, -12.3, 9.9])

  p1v = OneParm(8)(1234567890987654_8)
  p2v = TwoParms(1,5)(127,'fghijx',[-120,119,-118,117,-116])
  p3v = ThreeParms(4,5,12)(12345678_4,'klmnox',[1234120,1234119,1234118,1234117,1234116],[4.1d3,5.1001d3,1.0001d0,2.0d0,1.99d20])

  d1v = Derived(1)(-126)
  d2v = Derived2(8,3)(2234567890987654_8,'pqrx',[2231237890987654_8,2234564560987654_8,2234567897897654_8])
  d3v = Derived3(8,5,12)(3234567890987654_8,'stuvwxyz',[1131137890987654_8,1134564560987654_8,1134567897897654_8,1134567890980114_8,1134567890987653_8],&
       [4.1e3,5.1001e3,1.0001e0,2.0e0,1.99e20])

  print *, "A:", cv, cv % k, kind(cv % ifld)
  print *, "B:", gcv, gcv % k, gcv % l, kind(gcv % ifld), kind(gcv % chfld), kind(gcv % iarr), &
       len(gcv % chfld), size(gcv % iarr)
  print *, "C:", ggcv, ggcv % k, ggcv % l, ggcv % k2, kind(ggcv % ifld), kind(ggcv % chfld), kind(ggcv % iarr), kind(ggcv % rarr), &
       len(ggcv % chfld), size(ggcv % iarr), size(ggcv % rarr)

  print *, "D:", p1v, p1v % k, kind(p1v % ifld)
  print *, "E:", p2v, p2v % k, p2v % l, kind(p2v % ifld), kind(p2v % chfld), kind(p2v % iarr), &
       len(p2v % chfld), size(p2v % iarr)
  print *, "F:", p3v, p3v % k, p3v % l, p3v % k2, kind(p3v % ifld), kind(p3v % chfld), kind(p3v % iarr), kind(p3v % rarr), &
       len(p3v % chfld), size(p3v % iarr), size(p3v % rarr)

  print *, "G:", d1v, d1v % k, kind(d1v % ifld)
  print *, "H:", d2v, d2v % k, d2v % l, kind(d2v % ifld), kind(d2v % chfld), kind(d2v % iarr), &
       len(d2v % chfld), size(d2v % iarr)
  print *, "I:", d3v, d3v % k, d3v % l, d3v % k2, kind(d3v % ifld), kind(d3v % chfld), kind(d3v % iarr), kind(d3v % rarr), &
       len(d3v % chfld), size(d3v % iarr), size(d3v % rarr)

  i = 3
  cv = Child(kind(i_const))(121)
  gcv = GChild(kind(i_const),i)(998,'xyzq', [1025, 1029, 2054])
  ggcv = GGChild(2,kind(i_const) + i,3*kind(i_const))(3013,'abcdex',[10900, 10899, 10901, 10888, 11234],[4.1, 5.1, 6.1, -12.3, 9.9])

  p1v = OneParm(kind(i_const)*4)(1234567890987654_8)
  p2v = TwoParms(1,2*i-1)(127,'fghijx',[-120,119,-118,117,-116])
  p3v = ThreeParms(4,5,12)(12345678_4,'klmnox',[1234120,1234119,1234118,1234117,1234116],[4.1d3,5.1001d3,1.0001d0,2.0d0,1.99d20])

  d1v = Derived(kind(i_const)/2)(-126)
  d2v = Derived2(8,i)(2234567890987654_8,'pqrx',[2231237890987654_8,2234564560987654_8,2234567897897654_8,2234567890980124_8,2234567890987653_8])
  d3v = Derived3(4*kind(i_const),2*i-1,12)(3234567890987654_8,'stuvwxyz',[1131137890987654_8,1134564560987654_8,1134567897897654_8,1134567890980114_8,1134567890987653_8],&
       [4.1e3,5.1001e3,1.0001e0,2.0e0,1.99e20])

  print *, "J:", cv, cv % k, kind(cv % ifld)
  print *, "K:", gcv, gcv % k, gcv % l, kind(gcv % ifld), kind(gcv % chfld), kind(gcv % iarr), &
       len(gcv % chfld), size(gcv % iarr)
  print *, "L:", ggcv, ggcv % k, ggcv % l, ggcv % k2, kind(ggcv % ifld), kind(ggcv % chfld), kind(ggcv % iarr), kind(ggcv % rarr), &
       len(ggcv % chfld), size(ggcv % iarr), size(ggcv % rarr)

  print *, "M:", p1v, p1v % k, kind(p1v % ifld)
  print *, "N:", p2v, p2v % k, p2v % l, kind(p2v % ifld), kind(p2v % chfld), kind(p2v % iarr), &
       len(p2v % chfld), size(p2v % iarr)
  print *, "O:", p3v, p3v % k, p3v % l, p3v % k2, kind(p3v % ifld), kind(p3v % chfld), kind(p3v % iarr), kind(p3v % rarr), &
       len(p3v % chfld), size(p3v % iarr), size(p3v % rarr)

  print *, "P:", d1v, d1v % k, kind(d1v % ifld)
  print *, "Q:", d2v, d2v % k, d2v % l, kind(d2v % ifld), kind(d2v % chfld), kind(d2v % iarr), &
       len(d2v % chfld), size(d2v % iarr)
  print *, "R:", d3v, d3v % k, d3v % l, d3v % k2, kind(d3v % ifld), kind(d3v % chfld), kind(d3v % iarr), kind(d3v % rarr), &
       len(d3v % chfld), size(d3v % iarr), size(d3v % rarr)


  print *, "S:", Child(2)(121)
  print *, "T:", GChild(2,3)(999,'xyzq', [1025, 1029, 2054])
  print *, "U:", GGChild(2,5,6)(3013,'abcdex',[10900, 10899, 10901, 10888, 11234],[4.1, 5.1, 6.1, -12.3, 9.9])

  print *, "V:", OneParm(8)(1234567890987654_8)
  print *, "W:", TwoParms(1,5)(127,'fghijx',[-120,119,-118,117,-116])
  print *, "X:", ThreeParms(4,5,12)(12345678_4,'klmnox',[1234120,1234119,1234118,1234117,1234116],[4.1d3,5.1001d3,1.0001d0,2.0d0,1.99d20])

  print *, "Y:", Derived(1)(-126)
  print *, "Z:", Derived2(8,3)(2234567890987654_8,'pqrx',[2231237890987654_8,2234564560987654_8,2234567897897654_8])
  print *, "2:", Derived3(8,5,12)(3234567890987654_8,'stuvwxyz',[1131137890987654_8,1134564560987654_8,1134567897897654_8,1134567890980114_8,1134567890987653_8],&
       [4.1e3,5.1001e3,1.0001e0,2.0e0,1.99e20])

end program dtpUseSC01
