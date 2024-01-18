!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpSimpleBinaryMixedPrintInterface
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : mixed result types, binary ops in print statements (generic interface)
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
!*  Define several types with procedures for binary operators associated via
!*  generic interfaces, and verify that the correct function is invoked and the
!*  correct type created when the operation appears in a print statement.
!*  Only simple expressions (one operator, two operands, no nesting) are used.
!*  Here, operators may give different result types.  Specifically, "*" and "/"
!*  have been adapted to compose operands in different types and split them up
!*  again, respectively (expressions like DK*DK produce an appropriate D2K
!*  object and those like DK*DL or DL*DK produce a D2L object, expressions like
!*  D2K/DK produce either an integer, a DK object, or a D2K object, and expressions
!*  like D2L/DK or D2L/DL produce a DL or a DK object).
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleBinaryMixedPrintInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  type dl (l)
     integer, len  :: l
     character(1)  :: cvar(l)
  end type dl

  type d2k (k,k2)
     integer, kind :: k, k2
     integer(k)    :: ivar
     integer(k2)   :: ivar2
  end type d2k

  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     integer(k)    :: ivar
  end type d2l


  interface operator(*)
    module procedure binaryStar2K22
    module procedure binaryStar2K24
    module procedure binaryStar2K42
    module procedure binaryStar2K44
    module procedure binaryStar2L2k
    module procedure binaryStar2L4k
    module procedure binaryStar2L2
    module procedure binaryStar2L4
  end interface operator(*)

  interface operator(+)
    module procedure binaryPlus2K22
    module procedure binaryPlus2K24
    module procedure binaryPlus2K42
    module procedure binaryPlus2K44
    module procedure binaryPlus2L2
    module procedure binaryPlus2L4
    module procedure binaryPlusK2
    module procedure binaryPlusK4
    module procedure binaryPlusL
  end interface operator(+)

  interface operator(-)
    module procedure binaryMinus2L2
    module procedure binaryMinus2L4
    module procedure binaryMinusK2
    module procedure binaryMinusK4
    module procedure binaryMinusL
  end interface operator(-)

  interface operator(/)
    module procedure binarySlash2K222
    module procedure binarySlash2K242
    module procedure binarySlash2K422
    module procedure binarySlash2K442
    module procedure binarySlash2K224
    module procedure binarySlash2K244
    module procedure binarySlash2K424
    module procedure binarySlash2K444
    module procedure binarySlashL2
    module procedure binarySlashL4
    module procedure binarySlashL2k
    module procedure binarySlashL4k
  end interface operator(/)

contains

  type(dk(2)) function binaryPlusK2(this,that)
    class(dk(2)), intent(in) :: this, that
    binaryPlusK2 = dk(2)(this%ivar + that%ivar + 64)
  end function binaryPlusK2

  type(dk(4)) function binaryPlusK4(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK4 = dk(4)(this%ivar + that%ivar + 32768)
  end function binaryPlusK4

  type(dk(2)) function binaryMinusK2(this,that)
    class(dk(2)), intent(in) :: this, that
    binaryMinusK2 = dk(2)(this%ivar - that%ivar - 128)
  end function binaryMinusK2

  type(dk(4)) function binaryMinusK4(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryMinusK4 = dk(4)(this%ivar - that%ivar - 65536)
  end function binaryMinusK4


  type(d2k(2,2)) function binaryStar2K22(this,that)
    class(dk(2)), intent(in) :: this, that
    binaryStar2K22 = d2k(2,2)(this%ivar,that%ivar)
  end function binaryStar2K22

  type(d2k(2,4)) function binaryStar2K24(this,that)
    class(dk(2)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binaryStar2K24 = d2k(2,4)(this%ivar,that%ivar)
  end function binaryStar2K24

  type(d2k(4,2)) function binaryStar2K42(this,that)
    class(dk(4)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binaryStar2K42 = d2k(4,2)(this%ivar,that%ivar)
  end function binaryStar2K42

  type(d2k(4,4)) function binaryStar2K44(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryStar2K44 = d2k(4,4)(this%ivar,that%ivar)
  end function binaryStar2K44

  function binaryStar2L2K(this,that)
    class(dk(2)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    type(d2l(2,that%l)) :: binaryStar2L2K
    binaryStar2L2K = d2l(2,that%l)(that%cvar,this%ivar)
  end function binaryStar2L2K

  function binaryStar2L4K(this,that)
    class(dk(4)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    type(d2l(4,that%l)) :: binaryStar2L4K
    binaryStar2L4K = d2l(4,that%l)(that%cvar,this%ivar)
  end function binaryStar2L4K


  function binaryPlusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l+1)) :: binaryPlusL
    binaryPlusL = dl(this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'l'])
  end function binaryPlusL

  function binaryMinusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l-1)) :: binaryMinusL
    binaryMinusL = dl(this%l-1)(['m', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)])
  end function binaryMinusL


  function binaryStar2L2(this,that)
    class(dl(*)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    type(d2l(2,this%l)) :: binaryStar2L2
    binaryStar2L2 = d2l(2,this%l)(this%cvar,that%ivar)
  end function binaryStar2L2

  function binaryStar2L4(this,that)
    class(dl(*)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    type(d2l(4,this%l)) :: binaryStar2L4
    binaryStar2L4 = d2l(4,this%l)(this%cvar,that%ivar)
  end function binaryStar2L4


  type(d2k(2,2)) function binaryPlus2K22(this,that)
    class(d2k(2,2)), intent(in) :: this, that
    binaryPlus2K22 = d2k(2,2)(this%ivar + that%ivar + 256, this%ivar2 + that%ivar2 + 106)
  end function binaryPlus2K22

  type(d2k(2,4)) function binaryPlus2K24(this,that)
    class(d2k(2,4)), intent(in) :: this, that
    binaryPlus2K24 = d2k(2,4)(this%ivar + that%ivar + 512, this%ivar2 + that%ivar2 + 107)
  end function binaryPlus2K24

  type(d2k(4,2)) function binaryPlus2K42(this,that)
    class(d2k(4,2)), intent(in) :: this, that
    binaryPlus2K42 = d2k(4,2)(this%ivar + that%ivar + 131072, this%ivar2 + that%ivar2 + 108)
  end function binaryPlus2K42

  type(d2k(4,4)) function binaryPlus2K44(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryPlus2K44 = d2k(4,4)(this%ivar + that%ivar + 262144, this%ivar2 + that%ivar2 + 109)
  end function binaryPlus2K44

  type(dk(2)) function binarySlash2K222(this,that)
    class(d2k(2,2)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K222 = dk(2)(this%ivar + this%ivar2)
  end function binarySlash2K222

  type(dk(4)) function binarySlash2K242(this,that)
    class(d2k(2,4)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K242 = dk(4)(this%ivar)
  end function binarySlash2K242

  type(dk(4)) function binarySlash2K422(this,that)
    class(d2k(4,2)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K422 = dk(4)(this%ivar2)
  end function binarySlash2K422

  type(d2k(2,2)) function binarySlash2K442(this,that)
    class(d2k(4,4)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K442 = d2k(2,2)(this%ivar/that%ivar, this%ivar2/that%ivar)
  end function binarySlash2K442


  integer function binarySlash2K224(this,that)
    class(d2k(2,2)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K224 = (this%ivar * this%ivar2)
  end function binarySlash2K224

  type(dk(2)) function binarySlash2K244(this,that)
    class(d2k(2,4)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K244 = dk(2)(this%ivar)
  end function binarySlash2K244

  type(dk(2)) function binarySlash2K424(this,that)
    class(d2k(4,2)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K424 = dk(2)(this%ivar2)
  end function binarySlash2K424

  type(dk(4)) function binarySlash2K444(this,that)
    class(d2k(4,4)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K444 = dk(4)(this%ivar - this%ivar2)
  end function binarySlash2K444


  function binaryPlus2L2(this,that)
    class(d2l(2,*)), intent(in) :: this, that
    type(d2l(2,this%l+1)) :: binaryPlus2L2
    binaryPlus2L2 = d2l(2,this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'L'],this%ivar + that%ivar + 4096)
  end function binaryPlus2L2

  function binaryPlus2L4(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+1)) :: binaryPlus2L4
    binaryPlus2L4 = d2l(4,this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'Y'],this%ivar + that%ivar + 2097152)
  end function binaryPlus2L4

  function binaryMinus2L2(this,that)
    class(d2l(2,*)), intent(in) :: this, that
    type(d2l(2,this%l-1)) :: binaryMinus2L2
    binaryMinus2L2 = d2l(2,this%l-1)(['M', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)],this%ivar - that%ivar - 8192)
  end function binaryMinus2L2

  function binaryMinus2L4(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l-1)) :: binaryMinus2L4
    binaryMinus2L4 = d2l(4,this%l-1)(['Q', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)],this%ivar - that%ivar - 4194304)
  end function binaryMinus2L4


  function binarySlashL2(this,that)
    class(d2l(2,*)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    type(dl(this%l)) :: binarySlashL2
    binarySlashL2 = dl(this%l)(this%cvar)
  end function binarySlashL2

  function binarySlashL4(this,that)
    class(d2l(4,*)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    type(dl(this%l)) :: binarySlashL4
    binarySlashL4 = dl(this%l)(this%cvar)
  end function binarySlashL4

  type(dk(2)) function binarySlashL2K(this,that)
    class(d2l(2,*)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    binarySlashL2K = dk(2)(this%ivar)
  end function binarySlashL2K

  type(dk(4)) function binarySlashL4K(this,that)
    class(d2l(4,*)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    binarySlashL4K = dk(4)(this%ivar)
  end function binarySlashL4K

end module dtpUOpSimpleBinaryMixedPrintInterfacemod


program dtpUOpSimpleBinaryMixedPrintInterface

  use dtpUOpSimpleBinaryMixedPrintInterfacemod
  implicit none

  type(dk(2)) :: xk2a, xk2b, xk2c
  type(dk(4)) :: xk4a, xk4b, xk4c

  type(dl(5)) :: xla, xlb, xlc

  type(d2k(2,2)) :: x2k22a, x2k22b, x2k22c
  type(d2k(2,4)) :: x2k24a, x2k24b, x2k24c
  type(d2k(4,2)) :: x2k42a, x2k42b, x2k42c
  type(d2k(4,4)) :: x2k44a, x2k44b, x2k44c

  type(d2l(2,9)) :: x2l2a, x2l2b, x2l2c
  type(d2l(4,7)) :: x2l4a, x2l4b, x2l4c

  integer :: intc

  xk2a  =  dk(2)(243)
  xk2b  =  dk(2)(244)

  xk4a  =  dk(4)(32758)
  xk4b  =  dk(4)(32759)

  xla  =  dl(5)(['a','b','c','d','e'])
  xlb  =  dl(5)(['f','g','h','i','j'])

  x2k22a  =  d2k(2,2)(245,246)
  x2k22b  =  d2k(2,2)(247,248)
  x2k24a  =  d2k(2,4)(249,32760)
  x2k24b  =  d2k(2,4)(250,32761)
  x2k42a  =  d2k(4,2)(32766,251)
  x2k42b  =  d2k(4,2)(32767,252)
  x2k44a  =  d2k(4,4)(32762,32763)
  x2k44b  =  d2k(4,4)(32764,32765)

  x2l2a  =  d2l(2,9)(['k','l','m','n','o','p','q','r','s'],253)
  x2l2b  =  d2l(2,9)(['t','u','v','w','x','y','z','A','B'],254)
  x2l4a  =  d2l(4,7)(['C','D','E','F','G','H','I'],32766)
  x2l4b  =  d2l(4,7)(['J','K','L','M','N','O','P'],32767)


  ! try variables:
  print *, xk2a+xk2b		! DK_2_TYPE
  print *, xk2a-xk2b		! DK_2_TYPE
  print *, xk4a+xk4b		! DK_4_TYPE
  print *, xk4a-xk4b		! DK_4_TYPE

  print *, xk2a*xk2b		! D2K_2_2_TYPE
  print *, xk2a*xk4b		! D2K_2_4_TYPE
  print *, xk4a*xk2b		! D2K_4_2_TYPE
  print *, xk4a*xk4b		! D2K_4_4_TYPE

  print *, xk2a*xlb		! D2L_2_TYPE
  print *, xk4a*xlb		! D2L_4_TYPE

  print *, xla+xlb			! DL_TYPE
  print *, xla-xlb			! DL_TYPE

  print *, xla*xk2b		! D2L_2_TYPE
  print *, xla*xk4b		! D2L_4_TYPE

  print *, x2k22a+x2k22b	! D2K_2_2_TYPE
  print *, x2k24a+x2k24b	! D2K_2_4_TYPE
  print *, x2k42a+x2k42b	! D2K_4_2_TYPE
  print *, x2k44a+x2k44b	! D2K_4_4_TYPE

  print *, x2k22a/xk2b		! DK_2_TYPE
  print *, x2k24a/xk2b		! DK_4_TYPE
  print *, x2k42a/xk2b		! DK_4_TYPE
  print *, x2k44a/xk2b		! D2K_2_2_TYPE
  print *, x2k22a/xk4b		! INTEGER_TYPE
  print *, x2k24a/xk4b		! DK_2_TYPE
  print *, x2k42a/xk4b		! DK_2_TYPE
  print *, x2k44a/xk4b		! DK_4_TYPE

  print *, x2l2a+x2l2b		! D2L_2_TYPE
  print *, x2l4a+x2l4b		! D2L_4_TYPE
  print *, x2l2a-x2l2b		! D2L_2_TYPE
  print *, x2l4a-x2l4b		! D2L_4_TYPE

  print *, x2l2a/xk2b		! DL_TYPE
  print *, x2l4a/xk4b		! DL_TYPE
  print *, x2l2a/xlb		! DK_2_TYPE
  print *, x2l4a/xlb		! DK_4_TYPE

  ! try structure constructors:
  print *, dk(2)(54)+dk(2)(210)						! DK_2_TYPE
  print *, dk(2)(169)-dk(2)(192)						! DK_2_TYPE
  print *, dk(4)(26961)+dk(4)(31834)					! DK_4_TYPE
  print *, dk(4)(26493)-dk(4)(25376)					! DK_4_TYPE

  print *, dk(2)(54)*dk(2)(92)						! D2K_2_2_TYPE
  print *, dk(2)(22)*dk(4)(28644)					! D2K_2_4_TYPE
  print *, dk(4)(32619)*dk(2)(45)					! D2K_4_2_TYPE
  print *, dk(4)(7632)*dk(4)(28893)					! D2K_4_4_TYPE

  print *, dk(2)(154)*dl(5)(['l','f','`','&','v'])			! D2L_2_TYPE
  print *, dk(4)(2659)*dl(5)(['~','M','W','N','r'])			! D2L_4_TYPE

  print *, dl(5)(['e','t','C','~','>'])+dl(5)(['7','F','j','m','l'])	! DL_TYPE
  print *, dl(5)(['w','2','%','I','?'])-dl(5)(['a','w','q',')','I'])	! DL_TYPE

  print *, dl(5)(['@','|','p','q','S'])*dk(2)(174)			! D2L_2_TYPE
  print *, dl(5)(['L','m','8','"','a'])*dk(4)(30725)			! D2L_4_TYPE

  print *, d2k(2,2)(244,243)+d2k(2,2)(181,144)				! D2K_2_2_TYPE
  print *, d2k(2,4)(75,19146)+d2k(2,4)(133,9658)			! D2K_2_4_TYPE
  print *, d2k(4,2)(23268,208)+d2k(4,2)(23307,255)			! D2K_4_2_TYPE
  print *, d2k(4,4)(23655,16800)+d2k(4,4)(4747,25770)			! D2K_4_4_TYPE

  print *, d2k(2,2)(121,206)/dk(2)(247)					! DK_2_TYPE
  print *, d2k(2,4)(68,557)/dk(2)(25)					! DK_4_TYPE
  print *, d2k(4,2)(4673,108)/dk(2)(255)					! DK_4_TYPE
  print *, d2k(4,4)(7805,11604)/dk(2)(81)				! D2K_2_2_TYPE
  print *, d2k(2,2)(47,44)/dk(4)(27175)					! INTEGER_TYPE
  print *, d2k(2,4)(214,6160)/dk(4)(29934)				! DK_2_TYPE
  print *, d2k(4,2)(24261,89)/dk(4)(7659)					! DK_2_TYPE
  print *, d2k(4,4)(19279,539)/dk(4)(2303)				! DK_4_TYPE

  print *, d2l(2,9)(['s','F','a','a',')','H','_','>','v'],147) &
           + d2l(2,9)(['g','K','4','O','Y','x','U','o','%'],192)	! D2L_2_TYPE
  print *, d2l(4,7)(['|','W','R','Z','|','=','7'],32662) &
           + d2l(4,7)([':','t','Z',')','X','E','@'],258)		! D2L_4_TYPE
  print *, d2l(2,9)(['h','[','+','F','m','k','%','@','f'],8) &
           - d2l(2,9)(['X','F','k',',','t','6','%','H','`'],173)	! D2L_2_TYPE
  print *, d2l(4,7)(['#','r','Z','N','8','A','u'],13631) &
           - d2l(4,7)(['G','w','w','F','&','?','>'],5967)		! D2L_4_TYPE

  print *, d2l(2,9)(['#','n','1','%','`','<','j','w','Z'],152)/dk(2)(188)! DL_TYPE
  print *, d2l(4,7)(['W',']','u','.','a','8','7'],26321)/dk(4)(28668)	! DL_TYPE
  print *, d2l(2,9)(['n','+','n','>','m','"','g','<','B'],138) &
           / dl(5)(['[','4','b','S','2'])				! DK_2_TYPE
  print *, d2l(4,7)(['U','j','}','`','b','*','A'],11684) &
           / dl(5)(['|','+','N','F','v'])				! DK_4_TYPE
  print *, 'done'

end program dtpUOpSimpleBinaryMixedPrintInterface
