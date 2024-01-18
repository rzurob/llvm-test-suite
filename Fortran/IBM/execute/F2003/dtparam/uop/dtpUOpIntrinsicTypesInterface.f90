!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpIntrinsicTypesInterface
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : involvement of intrinsic types (generic interface)
!*
!*  REFERENCE                  : Feature Number 361989
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Define several types with binary operators (via generic interfaces)
!*  verify correct associativity, including intrinsic types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpIntrinsicTypesInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  interface operator(*)
     module procedure binaryStarK
     module procedure binaryStar
  end interface operator(*)

  interface operator(**)
     module procedure binaryPowerK
     module procedure binaryPower
  end interface operator(**)

  interface operator(+)
     module procedure binaryPlusK
     module procedure binaryPlus
  end interface operator(+)

  interface operator(-)
     module procedure binaryMinusK
     module procedure binaryMinus
  end interface operator(-)

  interface operator(/)
     module procedure binarySlashK
     module procedure binarySlash
  end interface operator(/)

contains

  type(dk(4)) function binaryPlusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK = dk(4)(this%ivar + that%ivar)
    print *, "binaryPlusK:", this%ivar, that%ivar, "->", binaryPlusK%ivar
  end function binaryPlusK

  type(dk(4)) function binaryPlus(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binaryPlus = dk(4)(this%ivar + that)
    print *, "binaryPlus:", this%ivar, that, "->", binaryPlus%ivar
  end function binaryPlus

  type(dk(4)) function binaryMinusK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryMinusK = dk(4)(this%ivar - that%ivar)
    print *, "binaryMinusK:", this%ivar, that%ivar, "->", binaryMinusK%ivar
  end function binaryMinusK

  type(dk(4)) function binaryMinus(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binaryMinus = dk(4)(this%ivar - that)
    print *, "binaryMinus:", this%ivar, that, "->", binaryMinus%ivar
  end function binaryMinus

  type(dk(4)) function binaryStarK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryStarK = dk(4)(this%ivar * that%ivar)
    print *, "binaryStarK:", this%ivar, that%ivar, "->", binaryStarK%ivar
  end function binaryStarK

  type(dk(4)) function binaryStar(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binaryStar = dk(4)(this%ivar * that)
    print *, "binaryStar:", this%ivar, that, "->", binaryStar%ivar
  end function binaryStar

  type(dk(4)) function binarySlashK(this,that)
    class(dk(4)), intent(in) :: this, that
    binarySlashK = dk(4)(this%ivar / that%ivar)
    print *, "binarySlashK:", this%ivar, that%ivar, "->", binarySlashK%ivar
  end function binarySlashK

  type(dk(4)) function binarySlash(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binarySlash = dk(4)(this%ivar / that)
    print *, "binarySlash:", this%ivar, that, "->", binarySlash%ivar
  end function binarySlash

  type(dk(4)) function binaryPowerK(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPowerK = dk(4)(this%ivar ** that%ivar)
    print *, "binaryPowerK:", this%ivar, that%ivar, "->", binaryPowerK%ivar
  end function binaryPowerK

  type(dk(4)) function binaryPower(this,that)
    class(dk(4)), intent(in) :: this
    integer, intent(in) :: that
    binaryPower = dk(4)(this%ivar ** that)
    print *, "binaryPower:", this%ivar, that, "->", binaryPower%ivar
  end function binaryPower

end module dtpUOpIntrinsicTypesInterfacemod


program dtpUOpIntrinsicTypesInterface

  use dtpUOpIntrinsicTypesInterfacemod
  implicit none

  type(dk(4)) :: xk4a, xk4b, xk4c, xk4d

  xk4a   = dk(4)(12)
  xk4b   = dk(4)(2)
  xk4c   = dk(4)(3)

  print *, "xk4d = xk4a+2+xk4b*xk4c*5"
  xk4d = xk4a+2+xk4b*xk4c*5
  print *, "xk4d = ((xk4a+2)+(xk4b*xk4c*5))"
  xk4d = ((xk4a+2)+(xk4b*xk4c*5))
  print *, "xk4d = xk4a*xk4b+5+xk4c+55+7"
  xk4d = xk4a*xk4b+5+xk4c+55+7
  print *, "xk4d = (((xk4a*xk4b+5)+xk4c)+55)+7"
  xk4d = (((xk4a*xk4b+5)+xk4c)+55)+7
  print *, "xk4d = (xk4a+xk4b)*xk4c"
  xk4d = (xk4a+xk4b)*xk4c
  print *, "xk4d = (xk4a-xk4b-77)*xk4c/xk4c"
  xk4d = (xk4a-xk4b-77)*xk4c/xk4c
  print *, "xk4d = xk4a*xk4b+66+xk4b*xk4c+44"
  xk4d = xk4a*xk4b+66+xk4b*xk4c+44
  print *, "xk4d = xk4a/xk4b*xk4c**2**3-xk4c"
  xk4d = xk4a/xk4b*xk4c**2**3-xk4c
  print *, "end"

end program dtpUOpIntrinsicTypesInterface
