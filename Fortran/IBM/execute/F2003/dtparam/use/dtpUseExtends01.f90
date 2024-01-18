!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseExtends01
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2008-08-25
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : inheritance
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : extends, inheritance
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Verify that kind and length parameters are correctly applied to descendants of DTP types.
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

program dtpUseExtends01

  use other
  implicit none
  type(Base)              :: bv
  type(Child(2))          :: cv
  type(GChild(2,3))       :: gcv
  type(GGChild(2,5,4))    :: ggcv

  type(OneParm(8))        :: p1v
  type(TwoParms(1,5))     :: p2v
  type(ThreeParms(4,5,8)) :: p3v

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

end program dtpUseExtends01
