!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct unary operator functions are used for given type and kind (single named operator)
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
!*  unary operators and verify that the correct function is invoked.
!*  The expressions appear in print statements and assignment statements,
!*  allowing only output, data values, and static type to be checked.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleUnaryNamedmod

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
    unaryPlusL = dl(arg%l+1)([arg%cvar, 'l'])
  end function unaryPlusL

  function unaryMinusL(arg)
    class(dl(*)), intent(in) :: arg
    type(dl(arg%l-1)) :: unaryMinusL
    unaryMinusL = dl(arg%l-1)(['m', arg%cvar(1:arg%l-2)])
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
    unaryPlus2L2 = d2l(2,arg%l+1)([arg%cvar, 'L'],arg%ivar + 4096)
  end function unaryPlus2L2

  function unaryPlus2L4(arg)
    class(d2l(4,*)), intent(in) :: arg
    type(d2l(4,arg%l+1)) :: unaryPlus2L4
    unaryPlus2L4 = d2l(4,arg%l+1)([arg%cvar, 'Y'],arg%ivar + 8192)
  end function unaryPlus2L4

  function unaryMinus2L2(arg)
    class(d2l(2,*)), intent(in) :: arg
    type(d2l(2,arg%l-1)) :: unaryMinus2L2
    unaryMinus2L2 = d2l(2,arg%l-1)(['M', arg%cvar(1:arg%l-2)],arg%ivar - 16384)
  end function unaryMinus2L2

  function unaryMinus2L4(arg)
    class(d2l(4,*)), intent(in) :: arg
    type(d2l(4,arg%l-1)) :: unaryMinus2L4
    unaryMinus2L4 = d2l(4,arg%l-1)(['Q', arg%cvar(1:arg%l-2)],arg%ivar - 32768)
  end function unaryMinus2L4


end module dtpUOpSimpleUnaryNamedmod


program dtpUOpSimpleUnaryNamed

  use dtpUOpSimpleUnaryNamedmod
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


    xk2c  = .pos.xk2a
    xk2a  = .pos.dk(2)(0)
    xk2d  = .neg.xk2b
    xk2b  = .neg.dk(2)(0)
    print *, xk2a, xk2b, xk2c, xk2d, .pos.dk(2)(0), .neg.dk(2)(0)

    xk4c  = .pos.xk4a
    xk4a  = .pos.dk(4)(0)
    xk4d  = .neg.xk4b
    xk4b  = .neg.dk(4)(0)
    print *, xk4a, xk4b, xk4c, xk4d, .pos.dk(4)(0), .neg.dk(4)(0)


    xlc  = .pos.xla
    xla  = .pos.dl(4)(['d','e','f','g'])
    xld  = .neg.xlb
    xlb  = .neg.dl(6)(['d','e','f','g','h','i'])
    print *, xla, xlb, xlc, xld, .pos.dl(4)(['d','e','f','g']), .neg.dl(6)(['d','e','f','g','h','i'])


    x2k22c  = .pos.x2k22a
    x2k22a  = .pos.d2k(2,2)(0,0)
    x2k22d  = .neg.x2k22b
    x2k22b  = .neg.d2k(2,2)(0,0)
    print *, x2k22a, x2k22b, x2k22c, x2k22d, .pos.d2k(2,2)(0,0), .neg.d2k(2,2)(0,0)

    x2k24c  = .pos.x2k24a
    x2k24a  = .pos.d2k(2,4)(0,0)
    x2k24d  = .neg.x2k24b
    x2k24b  = .neg.d2k(2,4)(0,0)
    print *, x2k24a, x2k24b, x2k24c, x2k24d, .pos.d2k(2,4)(0,0), .neg.d2k(2,4)(0,0)

    x2k42c  = .pos.x2k42a
    x2k42a  = .pos.d2k(4,2)(0,0)
    x2k42d  = .neg.x2k42b
    x2k42b  = .neg.d2k(4,2)(0,0)
    print *, x2k42a, x2k42b, x2k42c, x2k42d, .pos.d2k(4,2)(0,0), .neg.d2k(4,2)(0,0)

    x2k44c  = .pos.x2k44a
    x2k44a  = .pos.d2k(4,4)(0,0)
    x2k44d  = .neg.x2k44b
    x2k44b  = .neg.d2k(4,4)(0,0)
    print *, x2k44a, x2k44b, x2k44c, x2k44d, .pos.d2k(4,4)(0,0), .neg.d2k(4,4)(0,0)


    x2l2c  = .pos.x2l2a
    x2l2a  = .pos.d2l(2,8)(['z','y','x','w','v','u','t','s'],0)
    x2l2d  = .neg.x2l2b
    x2l2b  = .neg.d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],0)
    print *, x2l2a, x2l2b, x2l2c, x2l2d, .pos.d2l(2,8)(['z','y','x','w','v','u','t','s'],0), .neg.d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],0)

    x2l4c  = .pos.x2l4a
    x2l4a  = .pos.d2l(4,6)(['q','p','o','n','m','l'],0)
    x2l4d  = .neg.x2l4b
    x2l4b  = .neg.d2l(4,8)(['q','p','o','n','m','l','k','j'],0)
    print *, x2l4a, x2l4b, x2l4c, x2l4d, .pos.d2l(4,6)(['q','p','o','n','m','l'],0), .neg.d2l(4,8)(['q','p','o','n','m','l','k','j'],0)


end program dtpUOpSimpleUnaryNamed