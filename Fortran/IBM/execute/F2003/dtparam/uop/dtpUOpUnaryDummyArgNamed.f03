!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : several named unary ops appearing as an expression as a dummy argument
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
!*  Define several types with type-bound procedures and generic bindings for
!*  unary operators and verify that the correct function is invoked when the
!*  operation appears in a nested expression as a dummy argument.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpUnaryDummyArgNamedmod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
   contains
     generic :: operator(.pos.) => unaryPlusK2, unaryPlusK4
     procedure, pass :: unaryPlusK2
     procedure, pass :: unaryPlusK4
     generic :: operator(.neg.) => unaryMinusK2, unaryMinusK4
     procedure, pass :: unaryMinusK2
     procedure, pass :: unaryMinusK4
  end type dk

  type dl (l)
     integer, len  :: l
     character(1)  :: cvar(l)
   contains
     generic :: operator(.pos.) => unaryPlusL
     procedure, pass :: unaryPlusL
     generic :: operator(.neg.) => unaryMinusL
     procedure, pass :: unaryMinusL
  end type dl

  type d2k (k,k2)
     integer, kind :: k, k2
     integer(k)    :: ivar
     integer(k2)   :: ivar2
   contains
     generic :: operator(.pos.) => unaryPlus2K22, unaryPlus2K24, unaryPlus2K42, unaryPlus2K44
     procedure, pass :: unaryPlus2K22
     procedure, pass :: unaryPlus2K24
     procedure, pass :: unaryPlus2K42
     procedure, pass :: unaryPlus2K44
     generic :: operator(.neg.) => unaryMinus2K22, unaryMinus2K24, unaryMinus2K42, unaryMinus2K44
     procedure, pass :: unaryMinus2K22
     procedure, pass :: unaryMinus2K24
     procedure, pass :: unaryMinus2K42
     procedure, pass :: unaryMinus2K44
  end type d2k

  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     integer(k)    :: ivar
   contains
     generic :: operator(.pos.) => unaryPlus2L2, unaryPlus2L4
     procedure, pass :: unaryPlus2L2
     procedure, pass :: unaryPlus2L4
     generic :: operator(.neg.) => unaryMinus2L2, unaryMinus2L4
     procedure, pass :: unaryMinus2L2
     procedure, pass :: unaryMinus2L4
  end type d2l

contains

  type(dk(2)) function unaryPlusK2(arg)
    class(dk(2)), intent(in) :: arg
    unaryPlusK2 = dk(2)(arg%ivar + 1)
  end function unaryPlusK2

  type(dk(4)) function unaryPlusK4(arg)
    class(dk(4)), intent(in) :: arg
    unaryPlusK4 = dk(4)(arg%ivar + 2)
  end function unaryPlusK4

  type(dk(2)) function unaryMinusK2(arg)
    class(dk(2)), intent(in) :: arg
    unaryMinusK2 = dk(2)(arg%ivar - 4)
  end function unaryMinusK2

  type(dk(4)) function unaryMinusK4(arg)
    class(dk(4)), intent(in) :: arg
    unaryMinusK4 = dk(4)(arg%ivar - 8)
  end function unaryMinusK4


  function unaryPlusL(arg)
    class(dl(*)), intent(in) :: arg
    type(dl(arg%l+1)) :: unaryPlusL
    unaryPlusL%cvar = [arg%cvar, 'l']
  end function unaryPlusL

  function unaryMinusL(arg)
    class(dl(*)), intent(in) :: arg
    type(dl(arg%l-1)) :: unaryMinusL
    unaryMinusL%cvar = ['m', arg%cvar(1:arg%l-2)]
  end function unaryMinusL



  type(d2k(2,2)) function unaryPlus2K22(arg)
    class(d2k(2,2)), intent(in) :: arg
    unaryPlus2K22 = d2k(2,2)(arg%ivar + 16, arg%ivar2 + 106)
  end function unaryPlus2K22

  type(d2k(2,4)) function unaryPlus2K24(arg)
    class(d2k(2,4)), intent(in) :: arg
    unaryPlus2K24 = d2k(2,4)(arg%ivar + 32, arg%ivar2 + 107)
  end function unaryPlus2K24

  type(d2k(4,2)) function unaryPlus2K42(arg)
    class(d2k(4,2)), intent(in) :: arg
    unaryPlus2K42 = d2k(4,2)(arg%ivar + 64, arg%ivar2 + 108)
  end function unaryPlus2K42

  type(d2k(4,4)) function unaryPlus2K44(arg)
    class(d2k(4,4)), intent(in) :: arg
    unaryPlus2K44 = d2k(4,4)(arg%ivar + 128, arg%ivar2 + 109)
  end function unaryPlus2K44

  type(d2k(2,2)) function unaryMinus2K22(arg)
    class(d2k(2,2)), intent(in) :: arg
    unaryMinus2K22 = d2k(2,2)(arg%ivar - 256, arg%ivar2 + 110)
  end function unaryMinus2K22

  type(d2k(2,4)) function unaryMinus2K24(arg)
    class(d2k(2,4)), intent(in) :: arg
    unaryMinus2K24 = d2k(2,4)(arg%ivar - 512, arg%ivar2 + 111)
  end function unaryMinus2K24

  type(d2k(4,2)) function unaryMinus2K42(arg)
    class(d2k(4,2)), intent(in) :: arg
    unaryMinus2K42 = d2k(4,2)(arg%ivar - 1024, arg%ivar2 + 112)
  end function unaryMinus2K42

  type(d2k(4,4)) function unaryMinus2K44(arg)
    class(d2k(4,4)), intent(in) :: arg
    unaryMinus2K44 = d2k(4,4)(arg%ivar - 2048, arg%ivar2 + 113)
  end function unaryMinus2K44


  function unaryPlus2L2(arg)
    class(d2l(2,*)), intent(in) :: arg
    type(d2l(2,arg%l+1)) :: unaryPlus2L2
    unaryPlus2L2%cvar = [arg%cvar, 'L']
    unaryPlus2L2%ivar = arg%ivar + 4096
  end function unaryPlus2L2

  function unaryPlus2L4(arg)
    class(d2l(4,*)), intent(in) :: arg
    type(d2l(4,arg%l+1)) :: unaryPlus2L4
    unaryPlus2L4%cvar = [arg%cvar, 'Y']
    unaryPlus2L4%ivar = arg%ivar + 8192
  end function unaryPlus2L4

  function unaryMinus2L2(arg)
    class(d2l(2,*)), intent(in) :: arg
    type(d2l(2,arg%l-1)) :: unaryMinus2L2
    unaryMinus2L2%cvar = ['M', arg%cvar(1:arg%l-2)]
    unaryMinus2L2%ivar = arg%ivar - 16384
  end function unaryMinus2L2

  function unaryMinus2L4(arg)
    class(d2l(4,*)), intent(in) :: arg
    type(d2l(4,arg%l-1)) :: unaryMinus2L4
    unaryMinus2L4%cvar = ['Q', arg%cvar(1:arg%l-2)]
    unaryMinus2L4%ivar = arg%ivar - 32768
  end function unaryMinus2L4


end module dtpUOpUnaryDummyArgNamedmod


program dtpUOpUnaryDummyArgNamed

  use dtpUOpUnaryDummyArgNamedmod
  implicit none

  type(dk(2)) :: xk2a, xk2b, xk2c, xk2d
  type(dk(4)) :: xk4a, xk4b, xk4c, xk4d

  type(dl(5)) :: xla, xlb
  type(dl(6)) :: xlc
  type(dl(4)) :: xld

  type(d2k(2,2)) :: x2k22a, x2k22b, x2k22c, x2k22d
  type(d2k(2,4)) :: x2k24a, x2k24b, x2k24c, x2k24d
  type(d2k(4,2)) :: x2k42a, x2k42b, x2k42c, x2k42d
  type(d2k(4,4)) :: x2k44a, x2k44b, x2k44c, x2k44d

  type(d2l(2,9)) :: x2l2a, x2l2b
  type(d2l(2,10)):: x2l2c
  type(d2l(2,8)) :: x2l2d
  type(d2l(4,7)) :: x2l4a, x2l4b
  type(d2l(4,8)) :: x2l4c
  type(d2l(4,6)) :: x2l4d

  integer, parameter :: DK_2_TYPE    = 1
  integer, parameter :: DK_4_TYPE    = 2
  integer, parameter :: DL_TYPE      = 3
  integer, parameter :: D2K_2_2_TYPE = 4
  integer, parameter :: D2K_2_4_TYPE = 5
  integer, parameter :: D2K_4_2_TYPE = 6
  integer, parameter :: D2K_4_4_TYPE = 7
  integer, parameter :: D2L_2_TYPE   = 8
  integer, parameter :: D2L_4_TYPE   = 9
  integer, parameter :: UNKNOWN_TYPE = 10

  character(8) :: expName(10) = [character(8):: "dk(2)", "dk(4)", "dl(*)", "d2k(2,2)", "d2k(2,4)", &
                                                "d2k(4,2)", "d2k(4,4)", "d2l(2,*)", "d2l(4,*)", "unknown"]


  xk2a  =  dk(2)(0)
  xk2b  =  dk(2)(0)
  xk4a  =  dk(4)(0)
  xk4b  =  dk(4)(0)

  xla  =  dl(5)(['a','b','c','d','e'])
  xlb  =  dl(5)(['a','b','c','d','e'])

  x2k22a  =  d2k(2,2)(0,0)
  x2k22b  =  d2k(2,2)(0,0)
  x2k24a  =  d2k(2,4)(0,0)
  x2k24b  =  d2k(2,4)(0,0)
  x2k42a  =  d2k(4,2)(0,0)
  x2k42b  =  d2k(4,2)(0,0)
  x2k44a  =  d2k(4,4)(0,0)
  x2k44b  =  d2k(4,4)(0,0)

  x2l2a  =  d2l(2,9)(['a','b','c','d','e','f','g','h','i'],0)
  x2l2b  =  d2l(2,9)(['a','b','c','d','e','f','g','h','i'],0)
  x2l4a  =  d2l(4,7)(['c','d','e','f','g','h','i'],0)
  x2l4b  =  d2l(4,7)(['c','d','e','f','g','h','i'],0)

  call test(.pos.(.pos.xk2a), DK_2_TYPE)
  call test(.pos.(.pos.dk(2)(0)), DK_2_TYPE)
  call test(.neg.(.pos.xk2a), DK_2_TYPE)
  call test(.neg.(.pos.dk(2)(0)), DK_2_TYPE)
  call test(.pos.(.neg.xk2b), DK_2_TYPE)
  call test(.pos.(.neg.dk(2)(0)), DK_2_TYPE)
  call test(.neg.(.neg.xk2b), DK_2_TYPE)
  call test(.neg.(.neg.dk(2)(0)), DK_2_TYPE)

  call test(.pos.(.pos.xk4a), DK_4_TYPE)
  call test(.pos.(.pos.dk(4)(0)), DK_4_TYPE)
  call test(.neg.(.pos.xk4a), DK_4_TYPE)
  call test(.neg.(.pos.dk(4)(0)), DK_4_TYPE)
  call test(.pos.(.neg.xk4b), DK_4_TYPE)
  call test(.pos.(.neg.dk(4)(0)), DK_4_TYPE)
  call test(.neg.(.neg.xk4b), DK_4_TYPE)
  call test(.neg.(.neg.dk(4)(0)), DK_4_TYPE)

  call test(.pos.(.pos.xla), DL_TYPE)
  call test(.pos.(.pos.dl(4)(['d','e','f','g'])), DL_TYPE)
  call test(.neg.(.pos.xla), DL_TYPE)
  call test(.neg.(.pos.dl(4)(['d','e','f','g'])), DL_TYPE)
  call test(.pos.(.neg.xlb), DL_TYPE)
  call test(.pos.(.neg.dl(6)(['d','e','f','g','h','i'])), DL_TYPE)
  call test(.neg.(.neg.xlb), DL_TYPE)
  call test(.neg.(.neg.dl(6)(['d','e','f','g','h','i'])), DL_TYPE)

  call test(.pos.(.pos.x2k22a), D2K_2_2_TYPE)
  call test(.pos.(.pos.d2k(2,2)(0,0)), D2K_2_2_TYPE)
  call test(.neg.(.pos.x2k22a), D2K_2_2_TYPE)
  call test(.neg.(.pos.d2k(2,2)(0,0)), D2K_2_2_TYPE)
  call test(.pos.(.neg.x2k22b), D2K_2_2_TYPE)
  call test(.pos.(.neg.d2k(2,2)(0,0)), D2K_2_2_TYPE)
  call test(.neg.(.neg.x2k22b), D2K_2_2_TYPE)
  call test(.neg.(.neg.d2k(2,2)(0,0)), D2K_2_2_TYPE)

  call test(.pos.(.pos.x2k24a), D2K_2_4_TYPE)
  call test(.pos.(.pos.d2k(2,4)(0,0)), D2K_2_4_TYPE)
  call test(.neg.(.pos.x2k24a), D2K_2_4_TYPE)
  call test(.neg.(.pos.d2k(2,4)(0,0)), D2K_2_4_TYPE)
  call test(.pos.(.neg.x2k24b), D2K_2_4_TYPE)
  call test(.pos.(.neg.d2k(2,4)(0,0)), D2K_2_4_TYPE)
  call test(.neg.(.neg.x2k24b), D2K_2_4_TYPE)
  call test(.neg.(.neg.d2k(2,4)(0,0)), D2K_2_4_TYPE)

  call test(.pos.(.pos.x2k42a), D2K_4_2_TYPE)
  call test(.pos.(.pos.d2k(4,2)(0,0)), D2K_4_2_TYPE)
  call test(.neg.(.pos.x2k42a), D2K_4_2_TYPE)
  call test(.neg.(.pos.d2k(4,2)(0,0)), D2K_4_2_TYPE)
  call test(.pos.(.neg.x2k42b), D2K_4_2_TYPE)
  call test(.pos.(.neg.d2k(4,2)(0,0)), D2K_4_2_TYPE)
  call test(.neg.(.neg.x2k42b), D2K_4_2_TYPE)
  call test(.neg.(.neg.d2k(4,2)(0,0)), D2K_4_2_TYPE)

  call test(.pos.(.pos.x2k44a), D2K_4_4_TYPE)
  call test(.pos.(.pos.d2k(4,4)(0,0)), D2K_4_4_TYPE)
  call test(.neg.(.pos.x2k44a), D2K_4_4_TYPE)
  call test(.neg.(.pos.d2k(4,4)(0,0)), D2K_4_4_TYPE)
  call test(.pos.(.neg.x2k44b), D2K_4_4_TYPE)
  call test(.pos.(.neg.d2k(4,4)(0,0)), D2K_4_4_TYPE)
  call test(.neg.(.neg.x2k44b), D2K_4_4_TYPE)
  call test(.neg.(.neg.d2k(4,4)(0,0)), D2K_4_4_TYPE)

  call test(.pos.(.pos.x2l2a), D2L_2_TYPE)
  call test(.pos.(.pos.d2l(2,8)(['z','y','x','w','v','u','t','s'],0)), D2L_2_TYPE)
  call test(.neg.(.pos.x2l2a), D2L_2_TYPE)
  call test(.neg.(.pos.d2l(2,8)(['z','y','x','w','v','u','t','s'],0)), D2L_2_TYPE)
  call test(.pos.(.neg.x2l2b), D2L_2_TYPE)
  call test(.pos.(.neg.d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],0)), D2L_2_TYPE)
  call test(.neg.(.neg.x2l2b), D2L_2_TYPE)
  call test(.neg.(.neg.d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],0)), D2L_2_TYPE)

  call test(.pos.(.pos.x2l4a), D2L_4_TYPE)
  call test(.pos.(.pos.d2l(4,6)(['q','p','o','n','m','l'],0)), D2L_4_TYPE)
  call test(.neg.(.pos.x2l4a), D2L_4_TYPE)
  call test(.neg.(.pos.d2l(4,6)(['q','p','o','n','m','l'],0)), D2L_4_TYPE)
  call test(.pos.(.neg.x2l4b), D2L_4_TYPE)
  call test(.pos.(.neg.d2l(4,8)(['q','p','o','n','m','l','k','j'],0)), D2L_4_TYPE)
  call test(.neg.(.neg.x2l4b), D2L_4_TYPE)
  call test(.neg.(.neg.d2l(4,8)(['q','p','o','n','m','l','k','j'],0)), D2L_4_TYPE)

contains

  subroutine test (arg, expectation)
    class(*), intent(in) :: arg
    integer, intent(in)  :: expectation
    integer :: found
    select type (arg)
    type is (dk(2));    found = DK_2_TYPE;    print *, arg
    type is (dk(4));    found = DK_4_TYPE;    print *, arg
    type is (dl(*));    found = DL_TYPE;      print *, arg%l, arg
    type is (d2k(2,2)); found = D2K_2_2_TYPE; print *, arg
    type is (d2k(2,4)); found = D2K_2_4_TYPE; print *, arg
    type is (d2k(4,2)); found = D2K_4_2_TYPE; print *, arg
    type is (d2k(4,4)); found = D2K_4_4_TYPE; print *, arg
    type is (d2l(2,*)); found = D2L_2_TYPE;   print *, arg%l, arg
    type is (d2l(4,*)); found = D2L_4_TYPE;   print *, arg%l, arg
    class default;      found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", trim(expName(expectation)), ", got ", trim(expName(found))
       stop 2
    end if
  end subroutine test

end program dtpUOpUnaryDummyArgNamed
