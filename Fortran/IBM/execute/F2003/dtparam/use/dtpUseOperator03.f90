!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : user-defined operators
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : user-defined operators
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test operators; multiple KINDs.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseModule

  implicit none

  type :: NoParms
   contains
     procedure :: dtEQ_A => noParmsEQ
     procedure :: dtNE_A => noParmsNE
     generic :: operator(.eq.) => dtEQ_A
     generic :: operator(.ne.) => dtNE_A
  end type NoParms

  type, extends(NoParms) :: OneParm (k)
    integer, kind :: k
    integer(k) :: ifld = -1
   contains
     procedure :: dtEQ_A => oneParmsEQ4
     procedure :: dtNE_A => oneParmsNE4
     procedure :: dtEQ_B => oneParmsEQ8
     procedure :: dtNE_B => oneParmsNE8
     generic :: operator(.eq.) => dtEQ_B ! second kind - really want a total of four: one for each of 1,2,4,8
     generic :: operator(.ne.) => dtNE_B ! ditto
  end type OneParm

  type, extends(OneParm) :: TwoParms (l)
    integer, len :: l
    character(l) :: chfld = 'nopqrstuvwxyz'
    integer(k) :: iarr(l) = -2
   contains
     procedure :: dtEQ_A => twoParmsEQ4l
     procedure :: dtNE_A => twoParmsNE4l
     procedure :: dtEQ_B => twoParmsEQ8l
     procedure :: dtNE_B => twoParmsNE8l
  end type TwoParms

  type, extends(TwoParms) :: ThreeParms (k2)
    integer, kind :: k2
    real(k2) :: rarr(l) = -1.3
   contains
     procedure :: dtEQ_A => threeParmsEQ4l4
     procedure :: dtNE_A => threeParmsNE4l4
     procedure :: dtEQ_B => threeParmsEQ8l4
     procedure :: dtNE_B => threeParmsNE8l4
     procedure :: dtEQ_C => threeParmsEQ4l8
     procedure :: dtNE_C => threeParmsNE4l8
     procedure :: dtEQ_D => threeParmsEQ8l8
     procedure :: dtNE_D => threeParmsNE8l8
     generic :: operator(.eq.) => dtEQ_C, dtEQ_D ! third and fourth kind - really want a total of sixteen (4x4)
     generic :: operator(.ne.) => dtNE_C, dtNE_D ! ditto
  end type ThreeParms

  interface operator(.near.)
     module procedure near44
     module procedure near88
     module procedure near48
     module procedure near84
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


  elemental logical function noParmsEQ(this, that)
    class(NoParms), intent(in) :: this
    class(*), intent(in) :: that
    noParmsEQ = .false.
    select type(that)
    class is (NoParms)
      noParmsEQ = .true.
    end select
  end function noParmsEQ

  elemental logical function noParmsNE(this, that)
    class(NoParms), intent(in) :: this
    class(*), intent(in) :: that
    noParmsNE = .not. noParmsEQ(this,that)
  end function noParmsNE


  elemental logical function oneParmsEQ4(this, that)
    class(OneParm(4)), intent(in) :: this
    class(*), intent(in) :: that
    oneParmsEQ4 = .false.
    select type(that)
    class is (OneParm(4))
      oneParmsEQ4 = (this % ifld == that % ifld)
    end select
  end function oneParmsEQ4

  elemental logical function oneParmsNE4(this, that)
    class(OneParm(4)), intent(in) :: this
    class(*), intent(in) :: that
    oneParmsNE4 = .not. oneParmsEQ4(this,that)
  end function oneParmsNE4


  elemental logical function oneParmsEQ8(this, that)
    class(OneParm(8)), intent(in) :: this
    class(*), intent(in) :: that
    oneParmsEQ8 = .false.
    select type(that)
    class is (OneParm(4))
      oneParmsEQ8 = (this % ifld == that % ifld)
    class is (OneParm(8))
      oneParmsEQ8 = (this % ifld == that % ifld)
    end select
  end function oneParmsEQ8

  elemental logical function oneParmsNE8(this, that)
    class(OneParm(8)), intent(in) :: this
    class(*), intent(in) :: that
    oneParmsNE8 = .not. oneParmsEQ8(this,that)
  end function oneParmsNE8


  elemental logical function twoParmsEQ4l(this, that)
    class(TwoParms(4,*)), intent(in) :: this
    class(*), intent(in) :: that
    twoParmsEQ4l = .false.
    select type(that)
    class is (TwoParms(4,*))
      twoParmsEQ4l = (this%OneParm == that%OneParm) .and. (this % l == that % l) &
               .and. (this % chfld == that % chfld) .and. all(this % iarr == that % iarr)
    end select
  end function twoParmsEQ4l

  elemental logical function twoParmsNE4l(this, that)
    class(TwoParms(4,*)), intent(in) :: this
    class(*), intent(in) :: that
    twoParmsNE4l = .not. twoParmsEQ4l(this,that)
  end function twoParmsNE4l


  elemental logical function twoParmsEQ8l(this, that)
    class(TwoParms(8,*)), intent(in) :: this
    class(*), intent(in) :: that
    twoParmsEQ8l = .false.
    select type(that)
    class is (TwoParms(4,*))
      twoParmsEQ8l = (this%OneParm == that%OneParm) .and. (this % l == that % l) &
               .and. (this % chfld == that % chfld) .and. all(this % iarr == that % iarr)
    class is (TwoParms(8,*))
      twoParmsEQ8l = (this%OneParm == that%OneParm) .and. (this % l == that % l) &
               .and. (this % chfld == that % chfld) .and. all(this % iarr == that % iarr)
    end select
  end function twoParmsEQ8l

  elemental logical function twoParmsNE8l(this, that)
    class(TwoParms(8,*)), intent(in) :: this
    class(*), intent(in) :: that
    twoParmsNE8l = .not. twoParmsEQ8l(this,that)
  end function twoParmsNE8l


  elemental logical function threeParmsEQ4l4(this, that)
    class(ThreeParms(4,*,4)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsEQ4l4 = .false.
    select type(that)
    class is (ThreeParms(4,*,4))
      threeParmsEQ4l4 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    end select
  end function threeParmsEQ4l4

  elemental logical function threeParmsNE4l4(this, that)
    class(ThreeParms(4,*,4)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsNE4l4 = .not. threeParmsEQ4l4(this,that)
  end function threeParmsNE4l4



  elemental logical function threeParmsEQ4l8(this, that)
    class(ThreeParms(4,*,8)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsEQ4l8 = .false.
    select type(that)
    class is (ThreeParms(4,*,4))
      threeParmsEQ4l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(4,*,8))
      threeParmsEQ4l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(8,*,4))
      threeParmsEQ4l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(8,*,8))
      threeParmsEQ4l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    end select
  end function threeParmsEQ4l8

  elemental logical function threeParmsNE4l8(this, that)
    class(ThreeParms(4,*,8)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsNE4l8 = .not. threeParmsEQ4l8(this,that)
  end function threeParmsNE4l8


  elemental logical function threeParmsEQ8l4(this, that)
    class(ThreeParms(8,*,4)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsEQ8l4 = .false.
    select type(that)
    class is (ThreeParms(4,*,4))
      threeParmsEQ8l4 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(4,*,8))
      threeParmsEQ8l4 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(8,*,4))
      threeParmsEQ8l4 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(8,*,8))
      threeParmsEQ8l4 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    end select
  end function threeParmsEQ8l4

  elemental logical function threeParmsNE8l4(this, that)
    class(ThreeParms(8,*,4)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsNE8l4 = .not. threeParmsEQ8l4(this,that)
  end function threeParmsNE8l4


  elemental logical function threeParmsEQ8l8(this, that)
    class(ThreeParms(8,*,8)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsEQ8l8 = .false.
    select type(that)
    class is (ThreeParms(4,*,4))
      threeParmsEQ8l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(4,*,8))
      threeParmsEQ8l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(8,*,4))
      threeParmsEQ8l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    class is (ThreeParms(8,*,8))
      threeParmsEQ8l8 = (this%TwoParms == that%TwoParms) .and. all(this % rarr .near. that % rarr)
    end select
  end function threeParmsEQ8l8

  elemental logical function threeParmsNE8l8(this, that)
    class(ThreeParms(8,*,8)), intent(in) :: this
    class(*), intent(in) :: that
    threeParmsNE8l8 = .not. threeParmsEQ8l8(this,that)
  end function threeParmsNE8l8

end module dtpUseModule


program dtpUseOperator03
  use :: dtpUseModule
  implicit none
  type(OneParm(4))        :: p14va, p14vb, p14vc, p14vd
  type(TwoParms(4,5))     :: p245va, p245vb, p245vc, p245vd
  type(ThreeParms(4,5,4)) :: p3454va, p3454vb, p3454vc, p3454vd
  type(TwoParms(4,3))     :: p243va, p243vb, p243vc, p243vd

  type(OneParm(8))        :: p18va, p18vb, p18vc, p18vd
  type(TwoParms(8,5))     :: p285va, p285vb, p285vc, p285vd
  type(ThreeParms(8,5,8)) :: p3858va, p3858vb, p3858vc, p3858vd
  type(TwoParms(8,3))     :: p283va, p283vb, p283vc, p283vd
  type(ThreeParms(4,5,8)) :: p3458va, p3458vb, p3458vc, p3458vd
  type(ThreeParms(8,5,4)) :: p3854va, p3854vb, p3854vc, p3854vd

  integer(4) :: i

  p14va = OneParm(4)(472088165_4)
  p14vb = OneParm(4)(7203_4)
  p14vc = OneParm(4)(32357_4)
  p14vd = OneParm(4)(472088165_4)

  print *, "p14va:", p14va
  print *, "p14vb:", p14vb
  print *, "p14vc:", p14vc
  print *, "p14vd:", p14vd

  p245va = TwoParms(4,5)(1080045576_4,'abcdex',[(i**3,i=1021,1025)])
  p245vb = TwoParms(4,5)(16480_4,'abcdex',[(i,i=1021,1025)])
  p245vc = TwoParms(4,5)(12296_4,'klmnox',[(i,i=1025,1021,-1)])
  p245vd = TwoParms(4,5)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4])

  print *, "p245va:", p245va
  print *, "p245vb:", p245vb
  print *, "p245vc:", p245vc
  print *, "p245vd:", p245vd

  p3454va = ThreeParms(4,5,4)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3454vb = ThreeParms(4,5,4)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.31,14.1e-5])
  p3454vc = ThreeParms(4,5,4)(12296_4,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3454vd = ThreeParms(4,5,4)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])

  print *, "p3454va:", p3454va
  print *, "p3454vb:", p3454vb
  print *, "p3454vc:", p3454vc
  print *, "p3454vd:", p3454vd


  p243va = TwoParms(4,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])
  p243vb = TwoParms(4,3)(1080045576_4,'defx',[(i**3,i=1021,1023)])
  p243vc = TwoParms(4,3)(1080045576_4,'bcdx',[(i**3,i=1021,1023)])
  p243vd = TwoParms(4,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])

  print *, "p243va:", p243va
  print *, "p243vb:", p243vb
  print *, "p243vc:", p243vc
  print *, "p243vd:", p243vd


  p18va = OneParm(8)(472088165_4)
  p18vb = OneParm(8)(7203_4)
  p18vc = OneParm(8)(32357_4)
  p18vd = OneParm(8)(472088165_4)

  print *, "p18va:", p18va
  print *, "p18vb:", p18vb
  print *, "p18vc:", p18vc
  print *, "p18vd:", p18vd

  p285va = TwoParms(8,5)(1080045576_4,'abcdex',[(i**3,i=1021,1025)])
  p285vb = TwoParms(8,5)(16480_4,'abcdex',[(i,i=1021,1025)])
  p285vc = TwoParms(8,5)(12296_4,'klmnox',[(i,i=1025,1021,-1)])
  p285vd = TwoParms(8,5)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4])

  print *, "p285va:", p285va
  print *, "p285vb:", p285vb
  print *, "p285vc:", p285vc
  print *, "p285vd:", p285vd

  p3858va = ThreeParms(8,5,8)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3858vb = ThreeParms(8,5,8)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.31,14.1e-5])
  p3858vc = ThreeParms(8,5,8)(12296_4,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3858vd = ThreeParms(8,5,8)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])

  p3458va = ThreeParms(4,5,8)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3458vb = ThreeParms(4,5,8)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.31,14.1e-5])
  p3458vc = ThreeParms(4,5,8)(12296_4,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3458vd = ThreeParms(4,5,8)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])

  p3854va = ThreeParms(8,5,4)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3854vb = ThreeParms(8,5,4)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.31,14.1e-5])
  p3854vc = ThreeParms(8,5,4)(12296_4,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3854vd = ThreeParms(8,5,4)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])

  print *, "p3858va:", p3858va
  print *, "p3858vb:", p3858vb
  print *, "p3858vc:", p3858vc
  print *, "p3858vd:", p3858vd


  p283va = TwoParms(8,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])
  p283vb = TwoParms(8,3)(1080045576_4,'defx',[(i**3,i=1021,1023)])
  p283vc = TwoParms(8,3)(1080045576_4,'bcdx',[(i**3,i=1021,1023)])
  p283vd = TwoParms(8,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])

  print *, "p283va:", p283va
  print *, "p283vb:", p283vb
  print *, "p283vc:", p283vc
  print *, "p283vd:", p283vd



  if (p14va /= p14vd) error stop 11
  if (p14va == p14vb) error stop 12
  if (p14va == p14vc) error stop 13
  if (p14vb == p14vc) error stop 14

  if (p14va /= p18vd) error stop 15
  if (p14va == p18vb) error stop 16
  if (p14va == p18vc) error stop 17
  if (p14vb == p18vc) error stop 18

  if (p18va /= p14vd) error stop 19
  if (p18va == p14vb) error stop 20
  if (p18va == p14vc) error stop 21
  if (p18vb == p14vc) error stop 22

  if (p18va /= p18vd) error stop 23
  if (p18va == p18vb) error stop 24
  if (p18va == p18vc) error stop 25
  if (p18vb == p18vc) error stop 26


  if (p245va /= p245vd) error stop 27
  if (p245va == p245vb) error stop 28
  if (p245va == p245vc) error stop 29
  if (p245vb == p245vc) error stop 30

  if (p245va /= p285vd) error stop 31
  if (p245va == p285vb) error stop 32
  if (p245va == p285vc) error stop 33
  if (p245vb == p285vc) error stop 34

  if (p285va /= p245vd) error stop 35
  if (p285va == p245vb) error stop 36
  if (p285va == p245vc) error stop 37
  if (p285vb == p245vc) error stop 38

  if (p285va /= p285vd) error stop 39
  if (p285va == p285vb) error stop 40
  if (p285va == p285vc) error stop 41
  if (p285vb == p285vc) error stop 42


  if (p3454va /= p3454vd) error stop 43
  if (p3454va == p3454vb) error stop 44
  if (p3454va == p3454vc) error stop 45
  if (p3454vb == p3454vc) error stop 46

  if (p3454va /= p3458vd) error stop 47
  if (p3454va == p3458vb) error stop 48
  if (p3454va == p3458vc) error stop 49
  if (p3454vb == p3458vc) error stop 50

  if (p3458va /= p3454vd) error stop 51
  if (p3458va == p3454vb) error stop 52
  if (p3458va == p3454vc) error stop 53
  if (p3458vb == p3454vc) error stop 54

  if (p3458va /= p3458vd) error stop 55
  if (p3458va == p3458vb) error stop 56
  if (p3458va == p3458vc) error stop 57
  if (p3458vb == p3458vc) error stop 58

  if (p3854va /= p3854vd) error stop 59
  if (p3854va == p3854vb) error stop 60
  if (p3854va == p3854vc) error stop 61
  if (p3854vb == p3854vc) error stop 62

  if (p3854va /= p3858vd) error stop 63
  if (p3854va == p3858vb) error stop 64
  if (p3854va == p3858vc) error stop 65
  if (p3854vb == p3858vc) error stop 66

  if (p3858va /= p3854vd) error stop 67
  if (p3858va == p3854vb) error stop 68
  if (p3858va == p3854vc) error stop 69
  if (p3858vb == p3854vc) error stop 70

  if (p3858va /= p3858vd) error stop 71
  if (p3858va == p3858vb) error stop 72
  if (p3858va == p3858vc) error stop 73
  if (p3858vb == p3858vc) error stop 74


  if (p243va /= p243vd) error stop 75
  if (p243va == p243vb) error stop 76
  if (p243va == p243vc) error stop 77
  if (p243vb == p243vc) error stop 78

  if (p245va == p243va) error stop 79
  if (p245vb == p243vb) error stop 80
  if (p245vc == p243vc) error stop 81
  if (p245vd == p243vd) error stop 82

  if (p14va == p243va)    error stop 83
  if (p14va == p245va)    error stop 84
  if (p14va == p3454va)   error stop 85

  if (p243va == p14va)    error stop 86
  if (p243va == p245va)   error stop 87
  if (p243va == p3454va)  error stop 88

  if (p245va == p14va)    error stop 89
  if (p245va == p243va)   error stop 90
  if (p245va == p3454va)  error stop 91
  if (p3454va == p14va)   error stop 92
  if (p3454va == p245va)  error stop 93
  if (p3454va == p3454va) error stop 94

  if (p14va  == 1)        error stop 95
  if (p243va == 'abcd')   error stop 96
  if (p243va == .true.)   error stop 97
  if (p243va == (3,4))    error stop 98

  print *, "done"

end program dtpUseOperator03
