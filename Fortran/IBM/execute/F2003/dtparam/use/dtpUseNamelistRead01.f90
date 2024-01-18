!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-10-07
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : simple input and output
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Verify that objects are read correctly via namelist I/O.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseNamelistRead01Basemod

  implicit none

  type :: Basic
     integer :: iComp = -1
  end type Basic

contains

  elemental logical function basicNeq(a1, a2)
    type(Basic), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Basic); basicNeq = a1%iComp /= a2%iComp
    end select
  end function basicNeq

end module dtpUseNamelistRead01Basemod

module dtpUseNamelistRead01mod

  use :: dtpUseNamelistRead01Basemod, only: Base => Basic, baseNeq => basicNeq
  implicit none

  type, extends(Base) :: Der(k)
     integer, kind :: k
     real(k) :: iCompD = -2.2
  end type

  type, extends(Der) :: Der2
     integer(k) :: iCompD2 = -3
  end type

  type, extends(Der2) :: Der3(l)
     integer, len :: l
     character(l) :: cCompD3 = 'abcdefghijklmnopqrstuvwxyz'
  end type

  type(Der3(8,9)), save  :: dmod38a, dmod38b, dmod38c, dmod38d
  type(Der3(4,10)), save :: dmod34a, dmod34b, dmod34c, dmod34d


  interface operator(.near.)
     module procedure near44
     module procedure near88
     module procedure near48
     module procedure near84
  end interface

  interface operator(.ne.)
     module procedure baseNeq
     module procedure der2Neq4
     module procedure der2Neq8
     module procedure der3Neq4
     module procedure der3Neq8
     module procedure derNeq4
     module procedure derNeq8
  end interface

contains

  elemental logical function near48(this, that)
    real(4), intent(in) :: this
    real(8), intent(in) :: that
    near48 = near44(this, real(that,kind(this)))
  end function near48

  elemental logical function near84(this, that)
    real(8), intent(in) :: this
    real(4), intent(in) :: that
    near84 = near44(real(this,kind(that)), that)
  end function near84

  elemental logical function near44(this, that)
    real(4), intent(in) :: this,that
    real(4) :: high_a,low_a,temp

    temp = that * 0.00001
    high_a = temp + that
    low_a = that - temp

    if(that < 0.0E0) then
       near44 = ((this >= high_a) .and. (this <= low_a))
    else
       near44 = ((this <= high_a) .and. (this >= low_a))
    end if
    return
  end function near44


  elemental logical function near88(this, that)
    real(8), intent(in) :: this,that
    real(8) :: high_a,low_a,temp

    temp = that * 0.0000000001d0
    high_a = temp + that
    low_a = that - temp

    if(that < 0.0E0) then
       near88 = ((this >= high_a) .and. (this <= low_a))
    else
       near88 = ((this <= high_a) .and. (this >= low_a))
    end if
    return
  end function near88


  elemental logical function derNeq4(a1, a2)
    type(Der(4)), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Der(4)); derNeq4 = .not. (a1%iCompD .near. a2%iCompD) .or. (a1%Base /= a2%Base)
    end select
  end function derNeq4

  elemental logical function der2Neq4(a1, a2)
    type(Der2(4)), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Der2(4)); der2Neq4 = (a1%iCompD2 /= a2%iCompD2) .or. (a1%Der /= a2%Der)
    end select
  end function der2Neq4

  elemental logical function der3Neq4(a1, a2)
    type(Der3(4,*)), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Der3(4,*)); der3Neq4 = (a1%cCompD3 /= a2%cCompD3) .or. (a1%Der2 /= a2%Der2)
    end select
  end function der3Neq4

  elemental logical function derNeq8(a1, a2)
    type(Der(8)), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Der(8)); derNeq8 = .not. (a1%iCompD .near. a2%iCompD) .or. (a1%Base /= a2%Base)
    end select
  end function derNeq8

  elemental logical function der2Neq8(a1, a2)
    type(Der2(8)), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Der2(8)); der2Neq8 = (a1%iCompD2 /= a2%iCompD2) .or. (a1%Der /= a2%Der)
    end select
    der2Neq8 = .false.
  end function der2Neq8

  elemental logical function der3Neq8(a1, a2)
    type(Der3(8,*)), intent(in) :: a1
    class(*), intent(in) :: a2
    select type (a2)
    type is (Der3(8,*)); der3Neq8 = (a1%cCompD3 /= a2%cCompD3) .or. (a1%Der2 /= a2%Der2)
    end select
  end function der3Neq8

end module dtpUseNamelistRead01mod


program dtpUseNamelistRead01

  use :: dtpUseNamelistRead01mod, only: Base, Derived => Der, Derived2 => Der2, Derived3 => Der3
  use :: dtpUseNamelistRead01mod, only: dmod38a, dmod34a, dmod38b, dmod34b, dmod38c, dmod34c, dmod38d, dmod34d, operator(.ne.)
  implicit none

  type (Base)           :: bb, bc, bd

  type (Derived(4))     :: d4a, d4b, d4c, d4d
  type (Derived2(4))    :: d24a, d24b, d24c, d24d
  type (Derived3(4,3))  :: d343a, d343b, d343c, d343d

  type (Derived(8))     :: d8a, d8b, d8c, d8d
  type (Derived2(8))    :: d28a, d28b, d28c, d28d
  type (Derived3(8,3))  :: d383a, d383b, d383c, d383d

  namelist /wDer1/ d4a, d8a
  namelist /wDer2/ d24a, d28a
  namelist /wDer3/ d343a, d383a, dmod34a, dmod38a

  namelist /wAll/ bb, d4b, d8b, d24b, d28b, d343b, d383b, dmod34b, dmod38b
  namelist /wFour/ bc, d4c, d24c, d343c, dmod34c
  namelist /wEight/ d8c, d28c, d383c, dmod38c


  read *, bd
  read *, d4d
  read *, d24d
  read *, d343d

  read *, d8d
  read *, d28d
  read *, d383d

  read *, dmod38d
  read *, dmod34d

  print *, "Comparison data read"

  read(*, nml=wDer1)
  read(*, nml=wDer2)
  read(*, nml=wDer3)

  read(*, nml=wAll)
  read(*, nml=wFour)
  read(*, nml=wEight)

  print *, "Namelist data read"

  print *, bb, bc, bd
  if (bb /= bd .or. bc /= bd) stop 11

  print *, d4a, d4b, d4c, d4d
  if (d4a /= d4d .or. d4b /= d4d .or. d4c /= d4d) stop 12

  print *, d24a, d24b, d24c, d24d
  if (d24a /= d24d .or. d24b /= d24d .or. d24c /= d24d) stop 13

  print *, d343a, d343b, d343c, d343d
  if (d343a /= d343d .or. d343b /= d343d .or. d343c /= d343d) stop 14

  print *, d8a, d8b, d8c, d8d
  if (d8a /= d8d .or. d8b /= d8d .or. d8c /= d8d) stop 15

  print *, d28a, d28b, d28c, d28d
  if (d28a /= d28d .or. d28b /= d28d .or. d28c /= d28d) stop 16

  print *, d383a, d383b, d383c, d383d
  if (d383a /= d383d .or. d383b /= d383d .or. d383c /= d383d) stop 17


  print *, dmod38a, dmod38b, dmod38c, dmod38d
  if (dmod38a /= dmod38d .or. dmod38b /= dmod38d .or. dmod38c /= dmod38d) stop 18

  print *, dmod34a, dmod34b, dmod34c, dmod34d
  if (dmod34a /= dmod34d .or. dmod34b /= dmod34d .or. dmod34c /= dmod34d) stop 19

  print *, 'done'

end program dtpUseNamelistRead01
