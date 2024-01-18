!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUseSelectType01
!*
!*  DATE                       : 2008-08-25
!*
!*  PRIMARY FUNCTIONS TESTED   : DTP and USE
!*
!*  SECONDARY FUNCTIONS TESTED : select type
!*
!*  REFERENCE                  : Feature Number 355310
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : select type
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test select type.
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

contains

  subroutine describe(descrip, this)
    character(*), intent(in) :: descrip
    class(*), intent(in) :: this
    character(255) :: out
    select type(this)

    type is (integer(1))
      write(out,*) "I(1)", this
    type is (integer(2))
      write(out,*) "I(2)", this
    type is (integer(4))
      write(out,*) "I(4)", this
    type is (integer(8))
      write(out,*) "I(8)", this

    type is (logical(1))
      write(out,*) "L(1)", this
    type is (logical(2))
      write(out,*) "L(2)", this
    type is (logical(4))
      write(out,*) "L(4)", this
    type is (logical(8))
      write(out,*) "L(8)", this

    type is (real(4))
      write(out,*) "R(4)", this
    type is (real(8))
      write(out,*) "R(8)", this

    type is (complex(4))
      write(out,*) "Z(4)", this
    type is (complex(8))
      write(out,*) "Z(8)", this

    type is (character(*))
      write(out,*) "ch(", len(this), ") >", this, "<"

    type is (NoParms)
      write(out,*) "NoParms", this

    type is (OneParm(4))
      write(out,*) "OneParm(4)", this
    type is (OneParm(8))
      write(out,*) "OneParm(8)", this

    type is (TwoParms(4,*))
      write(out,*) "TwoParms(4,*)", this
    type is (TwoParms(8,*))
      write(out,*) "TwoParms(8,*)", this

    type is (ThreeParms(4,*,4))
      write(out,*) "ThreeParms(4,*,4)", this
    type is (ThreeParms(4,*,8))
      write(out,*) "ThreeParms(4,*,8)", this
    type is (ThreeParms(8,*,4))
      write(out,*) "ThreeParms(8,*,4)", this
    type is (ThreeParms(8,*,8))
      write(out,*) "ThreeParms(8,*,8)", this

    class default
      out = 'Unknown type'

    end select

    print *, trim(descrip), ":", trim(out)
  end subroutine describe

end module dtpUseModule


program dtpUseSelectType01
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

  p14va = OneParm(4)(-472088165_4)
  p14vb = OneParm(4)(7203_4)
  p14vc = OneParm(4)(32357_4)
  p14vd = OneParm(4)(472088165_4)

  p245va = TwoParms(4,5)(1080045576_4,'abcdex',[(i**3,i=1021,1025)])
  p245vb = TwoParms(4,5)(-16480_4,'abcdex',[(i,i=1021,1025)])
  p245vc = TwoParms(4,5)(12296_4,'klmnox',[(i,i=1025,1021,-1)])
  p245vd = TwoParms(4,5)(-1080045576_4,'abcdex',[1064332261_4,-1067462648_4,1070599167_4,1073741824_4,-1076890625_4])

  p3454va = ThreeParms(4,5,4)(-1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3454vb = ThreeParms(4,5,4)(1080045576_4,'abcdex',[(i**3,i=1021,1025)], &
                              [4.1,5.3,9.1e10,-3.31,14.1e-5])
  p3454vc = ThreeParms(4,5,4)(12296_4,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])
  p3454vd = ThreeParms(4,5,4)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,-1070599167_4,1073741824_4,1076890625_4], &
                              [4.1,5.3,9.1e10,-3.3,14.1e-5])

  p243va = TwoParms(4,3)(-1080045576_4,'abcx',[(i**3,i=1021,1023)])
  p243vb = TwoParms(4,3)(1080045576_4,'defx',[(i**3,i=1021,1023)])
  p243vc = TwoParms(4,3)(-1080045576_4,'bcdx',[(i**3,i=1021,1023)])
  p243vd = TwoParms(4,3)(1080045576_4,'abcx',[(i**3,i=1021,1023)])

  p18va = OneParm(8)(400072088165_8)
  p18vb = OneParm(8)(-720000000003_8)
  p18vc = OneParm(8)(3235000000007_8)
  p18vd = OneParm(8)(4011172088165_8)

  p285va = TwoParms(8,5)(-1080045512476_8,'abcdex',[(i**5,i=1021,1025)])
  p285vb = TwoParms(8,5)(1615413480_8,'abcdex',[(i,i=1021,1025)])
  p285vc = TwoParms(8,5)(1223465696_8,'klmnox',[(i,i=1025,1021,-1)])
  p285vd = TwoParms(8,5)(10800423452545576_8,'abcdex',[252341064332261_8,-106254327462648_8,1072450599167_8,654471073741824_8,107674567890625_8])

  p3858va = ThreeParms(8,5,8)(-1084764560045576_8,'abcdex',[(i**5,i=1021,1025)], &
                              [4.1d0,5.3d0,9.1d10,-3.3d0,14.1d-5])
  p3858vb = ThreeParms(8,5,8)(108004598585576_8,'abcdex',[(i**5,i=1021,1025)], &
                              [4.1d0,5.3d0,9.1d10,-3.31d0,14.1d-5])
  p3858vc = ThreeParms(8,5,8)(122958642546_8,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1d0,5.3d0,9.1d10,-3.3d0,14.1d-5])
  p3858vd = ThreeParms(8,5,8)(-10800455956836976_8,'abcdex',[-1066784332261_8,1376067462648_8,10773560599167_8,104846773741824_8,-8481076890625_8], &
                              [4.1d0,5.3d0,9.1d10,-3.3d0,14.1d-5])

  p3458va = ThreeParms(4,5,8)(1080045576_4,'abcdex',[(i**5,i=1021,1025)], &
                              [4.1d0,5.3d0,9.1d10,-3.3d0,14.1d-5])
  p3458vb = ThreeParms(4,5,8)(-1080045576_4,'abcdex',[(i**5,i=1021,1025)], &
                              [4.1d0,5.3d0,9.1d10,-3.31d0,14.1d-5])
  p3458vc = ThreeParms(4,5,8)(-12296_4,'klmnox',[(i,i=1025,1021,-1)], &
                              [4.1d0,5.3d0,9.1d10,-3.3d0,14.1d-5])
  p3458vd = ThreeParms(4,5,8)(1080045576_4,'abcdex',[1064332261_4,1067462648_4,-1070599167_4,1073741824_4,-1076890625_4], &
                              [4.1d0,5.3d0,9.1d10,-3.3d0,14.1d-5])

  p3854va = ThreeParms(8,5,4)(108004687445576_8,'abcdex',[(i**5,i=1021,1025)], &
                              [4.1e0,5.3e0,9.1e10,-3.3e0,14.1e-5])
  p3854vb = ThreeParms(8,5,4)(-10800455478476_8,'abcdex',[(i**5,i=1021,1025)], &
                              [4.1e0,5.3e0,9.1e10,-3.31e0,14.1e-5])
  p3854vc = ThreeParms(8,5,4)(12295060647846_8,'klmnox',[(i,i=-1021,-1025,-1)], &
                              [4.1e0,5.3e0,9.1e10,-3.3e0,14.1e-5])
  p3854vd = ThreeParms(8,5,4)(10800455476476_8,'abcdex',[-10643324674261_8,-1065597462648_8,10703365599167_8,-107375373741824_8,-103576376890625_8], &
                              [4.1e0,5.3e0,9.1e10,-3.3e0,14.1e-5])

  p283va = TwoParms(8,3)(1080024383845576_8,'abcx',[(i**5,i=1021,1023)])
  p283vb = TwoParms(8,3)(1080045576222_8,'defx',[(-i**5,i=1021,1023)])
  p283vc = TwoParms(8,3)(-108004552347676_8,'bcdx',[(-i**5,i=1021,1023)])
  p283vd = TwoParms(8,3)(1098245080045576_8,'abcx',[(i**5,i=1021,1023)])

  call describe("p14va:", p14va)
  call describe("p14vb:", p14vb)
  call describe("p14vc:", p14vc)
  call describe("p14vd:", p14vd)

  call describe("p245va:", p245va)
  call describe("p245vb:", p245vb)
  call describe("p245vc:", p245vc)
  call describe("p245vd:", p245vd)

  call describe("p3454va:", p3454va)
  call describe("p3454vb:", p3454vb)
  call describe("p3454vc:", p3454vc)
  call describe("p3454vd:", p3454vd)

  call describe("p243va:", p243va)
  call describe("p243vb:", p243vb)
  call describe("p243vc:", p243vc)
  call describe("p243vd:", p243vd)

  call describe("p18va:", p18va)
  call describe("p18vb:", p18vb)
  call describe("p18vc:", p18vc)
  call describe("p18vd:", p18vd)

  call describe("p285va:", p285va)
  call describe("p285vb:", p285vb)
  call describe("p285vc:", p285vc)
  call describe("p285vd:", p285vd)

  call describe("p3858va:", p3858va)
  call describe("p3858vb:", p3858vb)
  call describe("p3858vc:", p3858vc)
  call describe("p3858vd:", p3858vd)

  call describe("p283va:", p283va)
  call describe("p283vb:", p283vb)
  call describe("p283vc:", p283vc)
  call describe("p283vd:", p283vd)

  print *, "done"

end program dtpUseSelectType01
