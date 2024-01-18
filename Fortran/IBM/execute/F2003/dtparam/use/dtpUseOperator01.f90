!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseOperator01
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
!*  Test operators; start with a single KIND.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUseModule

  implicit none

  type :: NoParms
   contains
     procedure :: dteq => noParmsEQ
     procedure :: dtne => noParmsNE
     generic :: operator(.eq.) => dteq
     generic :: operator(.ne.) => dtne
  end type NoParms

  type, extends(NoParms) :: OneParm (k)
    integer, kind :: k
    integer(k) :: ifld = -1
   contains
     procedure :: dteq => oneParmsEQ4
     procedure :: dtne => oneParmsNE4
  end type OneParm

  type, extends(OneParm) :: TwoParms (l)
    integer, len :: l
    character(l) :: chfld = 'nopqrstuvwxyz'
    integer(k) :: iarr(l) = -2
   contains
     procedure :: dteq => twoParmsEQ4l
     procedure :: dtne => twoParmsNE4l
  end type TwoParms

  type, extends(TwoParms) :: ThreeParms (k2)
    integer, kind :: k2
    real(k2) :: rarr(l) = -1.3
   contains
     procedure :: dteq => threeParmsEQ4l4
     procedure :: dtne => threeParmsNE4l4
  end type ThreeParms

  interface operator(.near.)
     module procedure near4
     module procedure near8
  end interface


contains

  elemental logical function near4(this, that)
    real(4), intent(in) :: this,that
    real(4) :: high_a,low_a,temp

    temp = that * 0.00001
    high_a = temp + that
    low_a = that - temp

    if(that < 0.0E0) then
       near4 = ((this >= high_a) .and. (this <= low_a))
    else
       near4 = ((this <= high_a) .and. (this >= low_a))
    end if
    return
  end function near4


  elemental logical function near8(this, that)
    real(8), intent(in) :: this,that
    real(8) :: high_a,low_a,temp

    temp = that * 0.0000000001d0
    high_a = temp + that
    low_a = that - temp

    if(that < 0.0E0) then
       near8 = ((this >= high_a) .and. (this <= low_a))
    else
       near8 = ((this <= high_a) .and. (this >= low_a))
    end if
    return
  end function near8


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

end module dtpUseModule


program dtpUseOperator01
  use :: dtpUseModule
  implicit none
  type(OneParm(4))        :: p14va, p14vb, p14vc, p14vd
  type(TwoParms(4,5))     :: p245va, p245vb, p245vc, p245vd
  type(ThreeParms(4,5,4)) :: p3454va, p3454vb, p3454vc, p3454vd
  type(TwoParms(4,3))     :: p243va, p243vb, p243vc, p243vd
  integer(4) :: i

  p14va = OneParm(4)(472088165_4)
  p14vb = OneParm(4)(7203_4)
  p14vc = OneParm(4)(32357_4)
  p14vd = OneParm(4)(472088165_4)

  print *, "p14va:", p14va
  print *, "p14vb:", p14vb
  print *, "p14vc:", p14vc
  print *, "p14vd:", p14vd

  if (p14va /= p14vd) stop 11
  if (p14va == p14vb) stop 12
  if (p14va == p14vc) stop 13
  if (p14vb == p14vc) stop 14


  p245va = TwoParms(4,5)(1080045576_4,'abcdex',[(i**3,i=1021,1025)])
  p245vb = TwoParms(4,5)(16480_4,'abcdex',[(i,i=1021,1025)])
  p245vc = TwoParms(4,5)(12296_4,'klmnox',[(i,i=1025,1021,-1)])
  p245vd = TwoParms(4,5)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,1070599167_4,1073741824_4,1076890625_4])

  print *, "p245va:", p245va
  print *, "p245vb:", p245vb
  print *, "p245vc:", p245vc
  print *, "p245vd:", p245vd

  if (p245va /= p245vd) stop 21
  if (p245va == p245vb) stop 22
  if (p245va == p245vc) stop 23
  if (p245vb == p245vc) stop 24


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

  if (p3454va /= p3454vd) stop 31
  if (p3454va == p3454vb) stop 32
  if (p3454va == p3454vc) stop 33
  if (p3454vb == p3454vc) stop 34


  p243va = TwoParms(4,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])
  p243vb = TwoParms(4,3)(1080045576_4,'defx',[(i**3,i=1021,1023)])
  p243vc = TwoParms(4,3)(1080045576_4,'bcdx',[(i**3,i=1021,1023)])
  p243vd = TwoParms(4,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])

  print *, "p243va:", p243va
  print *, "p243vb:", p243vb
  print *, "p243vc:", p243vc
  print *, "p243vd:", p243vd

  if (p243va /= p243vd) stop 41
  if (p243va == p243vb) stop 42
  if (p243va == p243vc) stop 43
  if (p243vb == p243vc) stop 44

  if (p245va == p243va) stop 51
  if (p245vb == p243vb) stop 52
  if (p245vc == p243vc) stop 53
  if (p245vd == p243vd) stop 54

  if (p14va == p243va)    stop 61
  if (p14va == p245va)    stop 62
  if (p14va == p3454va)   stop 63

  if (p243va == p14va)    stop 64
  if (p243va == p245va)   stop 65
  if (p243va == p3454va)  stop 66

  if (p245va == p14va)    stop 67
  if (p245va == p243va)   stop 68
  if (p245va /= p3454va)  stop 69

  if (p3454va == p14va)   stop 70
  if (p3454va == p245va)  stop 71
  if (p3454va /= p3454va) stop 72


  if (p14va  == 1)        stop 73
  if (p243va == 'abcd')   stop 74
  if (p243va == .true.)   stop 75
  if (p243va == (3,4))    stop 76


end program dtpUseOperator01
