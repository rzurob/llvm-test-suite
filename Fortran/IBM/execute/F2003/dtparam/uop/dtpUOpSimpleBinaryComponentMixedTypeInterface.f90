!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpSimpleBinaryComponentMixedTypeInterface
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct operator is applied with mixed type results and involvement of intrinsic types (generic interface)
!*
!*  REFERENCE                  : Feature Number 361989
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Define several types with derived type components and procedures associated
!*  via generic interface with binary operators, and verify that the correct function
!*  is invoked and the correct type created when the operation appears as a dummy argument.
!*  Here, operators may give different result types.  Specifically, "*" and "/"
!*  have been adapted to compose operands in different types and split them up
!*  again, respectively (expressions like DK*DK produce an appropriate D2K
!*  object and those like DK*DL or DL*DK produce a D2L object, expressions like
!*  D2K/DK produce either an integer, a DK object, or a D2K object, and expressions
!*  like D2L/DK or D2L/DL produce a DL or a DK object).
!*  In this test, we verify that the basic operations work when applied to derived
!*  types with derived type components in simple expressions (one operator, two operands,
!*  no nesting).  Another test, dtpUOpBinaryComponentMixedType, tests more complex
!*  expressions and the direct involvement of components.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleBinaryComponentMixedTypeInterfacemod

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
     type(dk(k2))  :: dkvar
     integer(k)    :: ivar
     type(dk(k2))  :: dkvar2
  end type d2k

  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     type(dl(l))   :: dlvar
     integer(k)    :: ivar(l)
     type(dl(l))   :: dlvar2
  end type d2l


  interface operator(+)
     module procedure binaryPlusK2   ! dk(2)+dk(2) => dk(2)
     module procedure binaryPlusK4   ! dk(4)+dk(4) => dk(4)
     module procedure binaryPlusK2i  ! dk(2)+i(2)  => dk(2)
     module procedure binaryPlusK4i  ! dk(4)+i(4)  => dk(4)
     module procedure binaryPlusL    ! dl+dl => dl
     module procedure binaryPlusLi   ! dl+int=> d2l(4)
     module procedure binaryPlus2K22 ! d2k(2,2)+d2k(2,2) => d2k(2,2)
     module procedure binaryPlus2K24 ! d2k(2,4)+d2k(2,4) => d2k(2,4)
     module procedure binaryPlus2K42 ! d2k(4,2)+d2k(4,2) => d2k(4,2)
     module procedure binaryPlus2K44 ! d2k(4,4)+d2k(4,4) => d2k(4,4)
     module procedure binaryPlus2L2  ! d2l(2)+d2l(2) => d2l(2)
     module procedure binaryPlus2L4  ! d2l(4)+d2l(4) => d2l(4)
  end interface operator(+)

  interface operator(-)
     module procedure binaryMinusK2  ! dk(2)-dk(2) => dk(2)
     module procedure binaryMinusK4  ! dk(4)-dk(4) => dk(4)
     module procedure binaryMinusL   ! dl-dl => dl
     module procedure binaryMinus2L2 ! d2l(2)-d2l(2) => d2l(2)
     module procedure binaryMinus2L4 ! d2l(4)-d2l(4) => d2l(4)
  end interface operator(-)

  interface operator(*)
     module procedure binaryStar2K22 ! dk(2)*dk(2) => D2K(2,2)
     module procedure binaryStar2K24 ! dk(2)*dk(4) => D2K(2,4)
     module procedure binaryStar2K42 ! dk(4)*dk(2) => D2K(4,2)
     module procedure binaryStar2K44 ! dk(4)*dk(4) => D2K(4,4)
     module procedure binaryStar2L2K ! dk(2)*dl    => D2L(2)
     module procedure binaryStar2L4K ! dk(4)*dl    => D2L(4)
     module procedure binaryStar2L2  ! dl*dk(2) => D2L(2)
     module procedure binaryStar2L4  ! dl*dk(4) => D2L(4)
  end interface operator(*)

  interface operator(/)
     module procedure binarySlash2K222 ! d2k(2,2)/dk(2) => dk(2)
     module procedure binarySlash2K242 ! d2k(2,4)/dk(2) => dk(4)
     module procedure binarySlash2K422 ! d2k(4,2)/dk(2) => dk(4)
     module procedure binarySlash2K442 ! d2k(4,4)/dk(2) => d2k(2,2)
     module procedure binarySlash2K224 ! d2k(2,2)/dk(4) => integer
     module procedure binarySlash2K244 ! d2k(2,4)/dk(4) => dk(2)
     module procedure binarySlash2K424 ! d2k(4,2)/dk(4) => dk(2)
     module procedure binarySlash2K444 ! d2k(4,4)/dk(4) => dk(4)
     module procedure binarySlashL2    ! d2l(2)/dk(2) => DL
     module procedure binarySlashL4    ! d2l(4)/dk(4) => DL
     module procedure binarySlashL2k   ! d2l(2)/dl    => DK(2)
     module procedure binarySlashL4k   ! d2l(4)/dl    => DK(4)
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


  type(dk(2)) function binaryPlusK2i(this,that)
    class(dk(2)), intent(in) :: this
    integer(2), intent(in) :: that
    binaryPlusK2i = dk(2)(this%ivar + that - 64)
  end function binaryPlusK2i

  type(dk(4)) function binaryPlusK4i(this,that)
    class(dk(4)), intent(in) :: this
    integer(4), intent(in) :: that
    binaryPlusK4i = dk(4)(this%ivar + that - 32768)
  end function binaryPlusK4i


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
    binaryStar2K22 = d2k(2,2)(dk(2)(that%ivar),this%ivar,dk(2)(this%ivar+that%ivar))
  end function binaryStar2K22

  type(d2k(2,4)) function binaryStar2K24(this,that)
    class(dk(2)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binaryStar2K24 = d2k(2,4)(dk(4)(that%ivar),this%ivar,dk(4)(this%ivar+that%ivar))
  end function binaryStar2K24

  type(d2k(4,2)) function binaryStar2K42(this,that)
    class(dk(4)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binaryStar2K42 = d2k(4,2)(dk(2)(that%ivar),this%ivar,dk(2)(mod(this%ivar,1000)+that%ivar))
  end function binaryStar2K42

  type(d2k(4,4)) function binaryStar2K44(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryStar2K44 = d2k(4,4)(dk(4)(that%ivar),this%ivar,dk(4)(this%ivar+that%ivar))
  end function binaryStar2K44

  function binaryStar2L2K(this,that)
    class(dk(2)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    type(d2l(2,that%l)) :: binaryStar2L2K
    binaryStar2L2K = d2l(2,that%l)(that%cvar,that,this%ivar+983,that)
  end function binaryStar2L2K

  function binaryStar2L4K(this,that)
    class(dk(4)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    type(d2l(4,that%l)) :: binaryStar2L4K
    binaryStar2L4K = d2l(4,that%l)(that%cvar,that,this%ivar+977,that)
  end function binaryStar2L4K


  function binaryPlusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l+1)) :: binaryPlusL
    binaryPlusL = dl(this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'l'])
  end function binaryPlusL

  function binaryPlusLi(this,that)
    class(dl(*)), intent(in) :: this
    integer, intent(in) :: that
    integer :: i
    type(d2l(4,this%l)) :: binaryPlusLi
    binaryPlusLi = d2l(4,this%l)(this%cvar,this,[(that+i,i=1,this%l)],dl(this%l)(this%cvar))
  end function binaryPlusLi


  function binaryMinusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l-1)) :: binaryMinusL
    binaryMinusL = dl(this%l-1)(['m', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)])
  end function binaryMinusL


  function binaryStar2L2(this,that)
    class(dl(*)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    type(d2l(2,this%l)) :: binaryStar2L2
    binaryStar2L2 = d2l(2,this%l)(this%cvar,this,that%ivar,this)
  end function binaryStar2L2

  function binaryStar2L4(this,that)
    class(dl(*)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    type(d2l(4,this%l)) :: binaryStar2L4
    binaryStar2L4 = d2l(4,this%l)(this%cvar,this,that%ivar,this)
  end function binaryStar2L4


  type(d2k(2,2)) function binaryPlus2K22(this,that)
    class(d2k(2,2)), intent(in) :: this, that
    binaryPlus2K22 = d2k(2,2)(dk(2)(this%dkvar%ivar+that%dkvar%ivar+106), this%ivar+that%ivar+256, dk(2)(this%dkvar2%ivar-that%dkvar2%ivar+997))
  end function binaryPlus2K22

  type(d2k(2,4)) function binaryPlus2K24(this,that)
    class(d2k(2,4)), intent(in) :: this, that
    binaryPlus2K24 = d2k(2,4)(dk(4)(this%dkvar%ivar+that%dkvar%ivar+107), this%ivar+that%ivar+512, dk(4)(this%dkvar2%ivar+that%dkvar2%ivar+29989))
  end function binaryPlus2K24

  type(d2k(4,2)) function binaryPlus2K42(this,that)
    class(d2k(4,2)), intent(in) :: this, that
    binaryPlus2K42 = d2k(4,2)(dk(2)(this%dkvar%ivar+that%dkvar%ivar+108), this%ivar+that%ivar+131072, dk(2)(this%dkvar2%ivar-that%dkvar2%ivar+991))
  end function binaryPlus2K42

  type(d2k(4,4)) function binaryPlus2K44(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryPlus2K44 = d2k(4,4)(dk(4)(this%dkvar%ivar+that%dkvar%ivar+109), this%ivar+that%ivar+262144 ,dk(4)(this%dkvar2%ivar+that%dkvar2%ivar+29983))
  end function binaryPlus2K44

  type(dk(2)) function binarySlash2K222(this,that)
    class(d2k(2,2)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K222 = this%dkvar
  end function binarySlash2K222

  type(dk(4)) function binarySlash2K242(this,that)
    class(d2k(2,4)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K242 = this%dkvar2
  end function binarySlash2K242

  type(dk(4)) function binarySlash2K422(this,that)
    class(d2k(4,2)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    type(dk(2)) :: tmp
    tmp = this%dkvar + this%dkvar2
    binarySlash2K422 = dk(4)(tmp%ivar)
  end function binarySlash2K422

  type(d2k(2,2)) function binarySlash2K442(this,that)
    class(d2k(4,4)), intent(in) :: this
    class(dk(2)), intent(in) :: that
    binarySlash2K442 = d2k(2,2)(dk(2)(this%dkvar%ivar/that%ivar), this%ivar/that%ivar, dk(2)(this%dkvar2%ivar/that%ivar))
  end function binarySlash2K442


  integer function binarySlash2K224(this,that)
    class(d2k(2,2)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K224 = (int(this%ivar,4) * this%dkvar%ivar)
  end function binarySlash2K224

  type(dk(2)) function binarySlash2K244(this,that)
    class(d2k(2,4)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K244 = dk(2)(this%ivar)
  end function binarySlash2K244

  type(dk(2)) function binarySlash2K424(this,that)
    class(d2k(4,2)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K424 = this%dkvar + this%dkvar2 + 991_2
  end function binarySlash2K424

  type(dk(4)) function binarySlash2K444(this,that)
    class(d2k(4,4)), intent(in) :: this
    class(dk(4)), intent(in) :: that
    binarySlash2K444 = this%dkvar + this%dkvar2 + 33301
  end function binarySlash2K444


  function binaryPlus2L2(this,that)
    class(d2l(2,*)), intent(in) :: this, that
    type(d2l(2,this%l+1)) :: binaryPlus2L2
    binaryPlus2L2 = d2l(2,this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'L'], dl(this%l+1)([this%cvar,'x']), &
                                    [this%ivar + that%ivar + 4096, 0], dl(this%l+1)([this%cvar,'y']))
  end function binaryPlus2L2

  function binaryPlus2L4(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l+1)) :: binaryPlus2L4
    binaryPlus2L4 = d2l(4,this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'Y'], dl(this%l+1)([this%cvar,'X']), &
                                    [-1, this%ivar + that%ivar + 2097152], dl(this%l+1)([this%cvar,'Y']))
  end function binaryPlus2L4

  function binaryMinus2L2(this,that)
    class(d2l(2,*)), intent(in) :: this, that
    type(d2l(2,this%l-1)) :: binaryMinus2L2
    binaryMinus2L2 = d2l(2,this%l-1)(['M', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)], &
                                     dl(this%l-1)(this%cvar(1:this%l-1)), [this%ivar(1:this%l-1) - that%ivar(1:this%l-1) - 8192],dl(this%l-1)(this%cvar(1:this%l-1)))
  end function binaryMinus2L2

  function binaryMinus2L4(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,this%l-1)) :: binaryMinus2L4
    binaryMinus2L4 = d2l(4,this%l-1)(['Q', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)], &
                                     dl(this%l-1)(this%cvar(1:this%l-1)), [this%ivar(1:this%l-1) - that%ivar(1:this%l-1) - 4194304], dl(this%l-1)(this%cvar(1:this%l-1)))
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
    binarySlashL4 = this%dlvar
  end function binarySlashL4

  type(dk(2)) function binarySlashL2K(this,that)
    class(d2l(2,*)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    binarySlashL2K = dk(2)(minval(this%ivar))
  end function binarySlashL2K

  type(dk(4)) function binarySlashL4K(this,that)
    class(d2l(4,*)), intent(in) :: this
    class(dl(*)), intent(in) :: that
    binarySlashL4K = dk(4)(maxval(this%ivar))
  end function binarySlashL4K

end module dtpUOpSimpleBinaryComponentMixedTypeInterfacemod


program dtpUOpSimpleBinaryComponentMixedTypeInterface

  use dtpUOpSimpleBinaryComponentMixedTypeInterfacemod
  implicit none
  
  type(dk(2)) :: xk2a, xk2b
  type(dk(4)) :: xk4a, xk4b

  type(dl(5)) :: xla, xlb

  type(d2k(2,2)) :: x2k22a, x2k22b
  type(d2k(2,4)) :: x2k24a, x2k24b
  type(d2k(4,2)) :: x2k42a, x2k42b
  type(d2k(4,4)) :: x2k44a, x2k44b

  type(d2l(2,9)) :: x2l2a, x2l2b
  type(d2l(4,7)) :: x2l4a, x2l4b

  integer :: i

  integer, parameter :: DK_2_TYPE    = 1
  integer, parameter :: DK_4_TYPE    = 2
  integer, parameter :: DL_TYPE      = 3
  integer, parameter :: D2K_2_2_TYPE = 4
  integer, parameter :: D2K_2_4_TYPE = 5
  integer, parameter :: D2K_4_2_TYPE = 6
  integer, parameter :: D2K_4_4_TYPE = 7
  integer, parameter :: D2L_2_TYPE   = 8
  integer, parameter :: D2L_4_TYPE   = 9
  integer, parameter :: INTEGER_TYPE = 10
  integer, parameter :: UNKNOWN_TYPE = 11

  character(8) :: expName(11) = [character(8):: "dk(2)", "dk(4)", "dl(*)", "d2k(2,2)", "d2k(2,4)", &
                                                "d2k(4,2)", "d2k(4,4)", "d2l(2,*)", "d2l(4,*)", "integer", "unknown"]

  xk2a  =  dk(2)(243)
  xk2b  =  dk(2)(244)

  xk4a  =  dk(4)(32758)
  xk4b  =  dk(4)(32759)

  xla  =  dl(5)(['a','b','c','d','e'])
  xlb  =  dl(5)(['f','g','h','i','j'])

  x2k22a  =  d2k(2,2)(dk(2)(246),245,dk(2)(30001))
  x2k22b  =  d2k(2,2)(dk(2)(248),247,dk(2)(30002))
  x2k24a  =  d2k(2,4)(dk(4)(32760),249,dk(4)(800001))
  x2k24b  =  d2k(2,4)(dk(4)(32761),250,dk(4)(800002))
  x2k42a  =  d2k(4,2)(dk(2)(251),32766,dk(2)(30003))
  x2k42b  =  d2k(4,2)(dk(2)(252),32767,dk(2)(30004))
  x2k44a  =  d2k(4,4)(dk(4)(32763),32762,dk(4)(800005))
  x2k44b  =  d2k(4,4)(dk(4)(32765),32764,dk(4)(800006))

  x2l2a  =  d2l(2,9)(['k','l','m','n','o','p','q','r','s'], dl(9)(['A','l','B','n','C','p','D','r','E']), &
                     [(253-i,i=10,90,10)], dl(9)(['F','l','G','n','H','p','I','r','J']))
  x2l2b  =  d2l(2,9)(['t','u','v','w','x','y','z','A','B'], dl(9)(['k','K','m','L','o','M','q','N','s']), &
                     [(254-i,i=10,90,10)],dl(9)(['k','S','m','T','o','U','q','V','s']))
  x2l4a  =  d2l(4,7)(['C','D','E','F','G','H','I'], dl(7)(['W','n','X','p','Y','r','Z']), &
                     [(32766-i,i=10,70,10)],dl(7)(['k','[','m','^','o',']','_']))
  x2l4b  =  d2l(4,7)(['J','K','L','M','N','O','P'], dl(7)(['J','`','L','a','N','q','z']), &
                     [(32767-i,i=10,70,10)],dl(7)(['b','K','c','M','d','3','@']))


  ! try variables:
  call test(xk2a+xk2b, DK_2_TYPE)
  call test(xk2a+100_2,DK_2_TYPE)
  call test(xk2a-xk2b, DK_2_TYPE)
  call test(xk4a+xk4b, DK_4_TYPE)
  call test(xk4a+4800, DK_4_TYPE)
  call test(xk4a-xk4b, DK_4_TYPE)

  call test(xk2a*xk2b, D2K_2_2_TYPE)
  call test(xk2a*xk4b, D2K_2_4_TYPE)
  call test(xk4a*xk2b, D2K_4_2_TYPE)
  call test(xk4a*xk4b, D2K_4_4_TYPE)

  call test(xk2a*xlb, D2L_2_TYPE)
  call test(xk4a*xlb, D2L_4_TYPE)

  call test(xla+xlb, DL_TYPE)
  call test(xla+300, D2L_4_TYPE)
  call test(xla-xlb, DL_TYPE)

  call test(xla*xk2b, D2L_2_TYPE)
  call test(xla*xk4b, D2L_4_TYPE)

  call test(x2k22a+x2k22b, D2K_2_2_TYPE)
  call test(x2k24a+x2k24b, D2K_2_4_TYPE)
  call test(x2k42a+x2k42b, D2K_4_2_TYPE)
  call test(x2k44a+x2k44b, D2K_4_4_TYPE)

  call test(x2k22a/xk2b, DK_2_TYPE)
  call test(x2k24a/xk2b, DK_4_TYPE)
  call test(x2k42a/xk2b, DK_4_TYPE)
  call test(x2k44a/xk2b, D2K_2_2_TYPE)
  call test(x2k22a/xk4b, INTEGER_TYPE)
  call test(x2k24a/xk4b, DK_2_TYPE)
  call test(x2k42a/xk4b, DK_2_TYPE)
  call test(x2k44a/xk4b, DK_4_TYPE)

  call test(x2l2a+x2l2b, D2L_2_TYPE)
  call test(x2l4a+x2l4b, D2L_4_TYPE)
  call test(x2l2a-x2l2b, D2L_2_TYPE)
  call test(x2l4a-x2l4b, D2L_4_TYPE)

  call test(x2l2a/xk2b, DL_TYPE)
  call test(x2l4a/xk4b, DL_TYPE)
  call test(x2l2a/xlb, DK_2_TYPE)
  call test(x2l4a/xlb, DK_4_TYPE)

  ! try structure constructors:
  call test(dk(2)(54)+dk(2)(210), DK_2_TYPE)
  call test(dk(2)(54)+100_2, DK_2_TYPE)
  call test(dk(2)(169)-dk(2)(192), DK_2_TYPE)
  call test(dk(4)(26961)+dk(4)(31834), DK_4_TYPE)
  call test(dk(4)(26961)+4800, DK_4_TYPE)
  call test(dk(4)(26493)-dk(4)(25376), DK_4_TYPE)

  call test(dk(2)(54)*dk(2)(92), D2K_2_2_TYPE)
  call test(dk(2)(22)*dk(4)(28644), D2K_2_4_TYPE)
  call test(dk(4)(32619)*dk(2)(45), D2K_4_2_TYPE)
  call test(dk(4)(7632)*dk(4)(28893), D2K_4_4_TYPE)

  call test(dk(2)(154)*dl(5)(['l','f','`','&','v']), D2L_2_TYPE)
  call test(dk(4)(2659)*dl(5)(['~','M','W','N','r']), D2L_4_TYPE)

  call test(dl(5)(['e','t','C','~','>'])+dl(5)(['7','F','j','m','l']), DL_TYPE)
  call test(dl(5)(['e','t','C','~','>'])+100, D2L_4_TYPE)
  call test(dl(5)(['w','2','%','I','?'])-dl(5)(['a','w','q',')','I']), DL_TYPE)

  call test(dl(5)(['@','|','p','q','S'])*dk(2)(174), D2L_2_TYPE)
  call test(dl(5)(['L','m','8','"','a'])*dk(4)(30725), D2L_4_TYPE)

  call test(d2k(2,2)(dk(2)(243),244,dk(2)(34471))+d2k(2,2)(dk(2)(144),181,dk(2)(34483)), D2K_2_2_TYPE)
  call test(d2k(2,4)(dk(4)(19146),75,dk(4)(34487))+d2k(2,4)(dk(4)(9658),133,dk(4)(34499)), D2K_2_4_TYPE)
  call test(d2k(4,2)(dk(2)(208),23268,dk(2)(34501))+d2k(4,2)(dk(2)(255),23307,dk(2)(34511)), D2K_4_2_TYPE)
  call test(d2k(4,4)(dk(4)(16800),23655,dk(4)(34513))+d2k(4,4)(dk(4)(25770),4747,dk(4)(34519)), D2K_4_4_TYPE)

  call test(d2k(2,2)(dk(2)(206),121,dk(2)(34537))/dk(2)(247), DK_2_TYPE)
  call test(d2k(2,4)(dk(4)(557),68,dk(4)(34543))/dk(2)(25), DK_4_TYPE)
  call test(d2k(4,2)(dk(2)(108),4673,dk(2)(34549))/dk(2)(255), DK_4_TYPE)
  call test(d2k(4,4)(dk(4)(11604),7805,dk(4)(34583))/dk(2)(81), D2K_2_2_TYPE)
  call test(d2k(2,2)(dk(2)(44),47,dk(2)(34589))/dk(4)(27175), INTEGER_TYPE)
  call test(d2k(2,4)(dk(4)(6160),214,dk(4)(34591))/dk(4)(29934), DK_2_TYPE)
  call test(d2k(4,2)(dk(2)(89),24261,dk(2)(34603))/dk(4)(7659), DK_2_TYPE)
  call test(d2k(4,4)(dk(4)(539),19279,dk(4)(34607))/dk(4)(2303), DK_4_TYPE)

  call test(d2l(2,9)(['s','F','a','a',')','H','_','>','v'], dl(9)([';','|','6','!','&','3','?','u','*']), &
                     [(147-i,i=10,90,10)],dl(9)(['U','#','g','v','x','w','d','U','~'])) &
            + d2l(2,9)(['g','K','4','O','Y','x','U','o','%'], dl(9)(['5','R','`','g','A','!','T','0','`']), &
                       [(192-i,i=10,90,10)],dl(9)(['|','N','w','N','2','L','-','<','p'])), D2L_2_TYPE)
  call test(d2l(4,7)(['|','W','R','Z','|','=','7'], dl(7)(['#','O','@','}','E','<',':']), &
                     [(32662-i,i=10,70,10)],dl(7)(['x','=',']','L','U','B','D'])) &
            + d2l(4,7)([':','t','Z',')','X','E','@'], dl(7)(['J','7','_','#','p','u',')']), &
                       [(258-i,i=10,70,10)],dl(7)(['v','{','M','E','`',' ','%'])), D2L_4_TYPE)
  call test(d2l(2,9)(['h','[','+','F','m','k','%','@','f'], dl(9)(['a','=','a','4','[','V',':','_','3']), &
                     [(8-i,i=10,90,10)],dl(9)(['P',' ','Z','H','*','5','O','(','r'])) &
            - d2l(2,9)(['X','F','k',',','t','6','%','H','`'], dl(9)(['E','r','.','8',' ','E','D','A','v']), &
                       [(173-i,i=10,90,10)],dl(9)(['2','1','q','j','K','n','8','u','2'])), D2L_2_TYPE)
  call test(d2l(4,7)(['#','r','Z','N','8','A','u'], dl(7)(['c','7',',','C','*','N','3']), &
                     [(13631-i,i=10,70,10)],dl(7)(['M','6','Y','.','n','K','?'])) &
            - d2l(4,7)(['G','w','w','F','&','?','>'], dl(7)(['4','b','=','/',':','(','m']), &
                       [(5967-i,i=10,70,10)],dl(7)(['M','^','f','Y','8','V','4'])), D2L_4_TYPE)

  call test(d2l(2,9)(['#','n','1','%','`','<','j','w','Z'], dl(9)(['3','K','o','>','}','~','!','C','4']), &
                     [(152-i,i=10,90,10)],dl(9)(['!','3','e','N','h','*','v','A','9'])) &
            / dk(2)(188), DL_TYPE)
  call test(d2l(4,7)(['W',']','u','.','a','8','7'], dl(7)(['u','%','*','9','e','J','"']), &
                     [(26321-i,i=10,70,10)],dl(7)(['@','`','e','Y','%','X',';'])) &
            / dk(4)(28668), DL_TYPE)
  call test(d2l(2,9)(['n','+','n','>','m','"','g','<','B'], dl(9)([',','-',')','[','Z','V','k','N','d']), &
                     [(138-i,i=10,90,10)],dl(9)(['K','K','(','*','$','1','j',';','r'])) &
            / dl(5)(['[','4','b','S','2']), DK_2_TYPE)
  call test(d2l(4,7)(['U','j','}','`','b','*','A'], dl(7)(['(','_','k','t','&','D','Q']), &
                     [(11684-i,i=10,70,10)],dl(7)(['0','B','r','!','3','.','J'])) &
            / dl(5)(['|','+','N','F','v']), DK_4_TYPE)

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
    type is (integer);  found = INTEGER_TYPE; print *, arg
    class default;      found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", trim(expName(expectation)), ", got ", trim(expName(found))
       stop 2
    end if
  end subroutine test

end program dtpUOpSimpleBinaryComponentMixedTypeInterface
