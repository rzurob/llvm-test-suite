!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpSimpleBinaryDummyArgInterface
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : binary ops appearing as dummy arguments (generic interface)
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
!*  Define several types with  procedures for binary operators associated via
!*  generic interfaces, and verify that the correct function is invoked when the
!*  operation appears as a dummy argument.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleBinaryDummyArgInterfacemod

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
    module procedure binaryMinus2K22
    module procedure binaryMinus2K24
    module procedure binaryMinus2K42
    module procedure binaryMinus2K44
    module procedure binaryMinus2L2
    module procedure binaryMinus2L4
    module procedure binaryMinusK2
    module procedure binaryMinusK4
    module procedure binaryMinusL
  end interface operator(-)

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

  type(d2k(2,2)) function binaryMinus2K22(this,that)
    class(d2k(2,2)), intent(in) :: this, that
    binaryMinus2K22 = d2k(2,2)(this%ivar - that%ivar - 1024, this%ivar2 + that%ivar2 + 110)
  end function binaryMinus2K22

  type(d2k(2,4)) function binaryMinus2K24(this,that)
    class(d2k(2,4)), intent(in) :: this, that
    binaryMinus2K24 = d2k(2,4)(this%ivar - that%ivar - 2048, this%ivar2 + that%ivar2 + 111)
  end function binaryMinus2K24

  type(d2k(4,2)) function binaryMinus2K42(this,that)
    class(d2k(4,2)), intent(in) :: this, that
    binaryMinus2K42 = d2k(4,2)(this%ivar - that%ivar - 524288, this%ivar2 + that%ivar2 + 112)
  end function binaryMinus2K42

  type(d2k(4,4)) function binaryMinus2K44(this,that)
    class(d2k(4,4)), intent(in) :: this, that
    binaryMinus2K44 = d2k(4,4)(this%ivar - that%ivar - 1048576, this%ivar2 + that%ivar2 + 113)
  end function binaryMinus2K44


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


end module dtpUOpSimpleBinaryDummyArgInterfacemod


program dtpUOpSimpleBinaryDummyArgInterface

  use dtpUOpSimpleBinaryDummyArgInterfacemod
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

  character(7) :: expName(10) = [character(8):: "dk(2)", "dk(4)", "dl(*)", "d2k(2,2)", "d2k(2,4)", &
                                                "d2k(4,2)", "d2k(4,4)", "d2l(2,*)", "d2l(4,*)", "unknown"]

  xk2a  =  dk(2)(1)
  xk2b  =  dk(2)(2)
  xk4a  =  dk(4)(3)
  xk4b  =  dk(4)(4)

  xla  =  dl(5)(['a','b','c','d','e'])
  xlb  =  dl(5)(['a','b','c','d','e'])

  x2k22a  =  d2k(2,2)(5,6)
  x2k22b  =  d2k(2,2)(7,8)
  x2k24a  =  d2k(2,4)(9,10)
  x2k24b  =  d2k(2,4)(11,12)
  x2k42a  =  d2k(4,2)(13,14)
  x2k42b  =  d2k(4,2)(15,16)
  x2k44a  =  d2k(4,4)(17,18)
  x2k44b  =  d2k(4,4)(19,20)

  x2l2a  =  d2l(2,9)(['a','b','c','d','e','f','g','h','i'],21)
  x2l2b  =  d2l(2,9)(['a','b','c','d','e','f','g','h','i'],22)
  x2l4a  =  d2l(4,7)(['c','d','e','f','g','h','i'],23)
  x2l4b  =  d2l(4,7)(['c','d','e','f','g','h','i'],-65303)


  call test(xk2a+xk2a, DK_2_TYPE)
  call test(dk(2)(25)+dk(2)(26), DK_2_TYPE)
  call test(xk2b-xk2b, DK_2_TYPE)
  call test(dk(2)(27)-dk(2)(28), DK_2_TYPE)

  call test(xk4a+xk4a, DK_4_TYPE)
  call test(dk(4)(33)+dk(4)(34), DK_4_TYPE)
  call test(xk4b-xk4b, DK_4_TYPE)
  call test(dk(4)(35)-dk(4)(36), DK_4_TYPE)

  call test(xla+xla, DL_TYPE)
  call test(dl(4)(['d','e','f','g'])+dl(4)(['d','e','f','g']), DL_TYPE)
  call test(xlb-xlb, DL_TYPE)
  call test(dl(6)(['d','e','f','g','h','i'])-dl(6)(['d','e','f','g','h','i']), DL_TYPE)

  call test(x2k22a+x2k22a, D2K_2_2_TYPE)
  call test(d2k(2,2)(41,42)+d2k(2,2)(43,44), D2K_2_2_TYPE)
  call test(x2k22b-x2k22b, D2K_2_2_TYPE)
  call test(d2k(2,2)(45,46)-d2k(2,2)(47,48), D2K_2_2_TYPE)

  call test(x2k24a+x2k24a, D2K_2_4_TYPE)
  call test(d2k(2,4)(57,58)+d2k(2,4)(59,60), D2K_2_4_TYPE)
  call test(x2k24b-x2k24b, D2K_2_4_TYPE)
  call test(d2k(2,4)(-60,-59)-d2k(2,4)(-58,-57), D2K_2_4_TYPE)

  call test(x2k42a+x2k42a, D2K_4_2_TYPE)
  call test(d2k(4,2)(-48,-47)+d2k(4,2)(-46,-45), D2K_4_2_TYPE)
  call test(x2k42b-x2k42b, D2K_4_2_TYPE)
  call test(d2k(4,2)(-44,-43)-d2k(4,2)(-42,-41), D2K_4_2_TYPE)

  call test(x2k44a+x2k44a, D2K_4_4_TYPE)
  call test(d2k(4,4)(-32,-31)+d2k(4,4)(-30,-29), D2K_4_4_TYPE)
  call test(x2k44b-x2k44b, D2K_4_4_TYPE)
  call test(d2k(4,4)(-28,-27)-d2k(4,4)(-26,-25), D2K_4_4_TYPE)

  call test(x2l2a+x2l2a, D2L_2_TYPE)
  call test(d2l(2,8)(['z','y','x','w','v','u','t','s'],-106)+d2l(2,8)(['z','y','x','w','v','u','t','s'],-108), D2L_2_TYPE)
  call test(x2l2b-x2l2b, D2L_2_TYPE)
  call test(d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],-107)-d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],107), D2L_2_TYPE)

  call test(x2l4a+x2l4a, D2L_4_TYPE)
  call test(d2l(4,6)(['q','p','o','n','m','l'],-104)+d2l(4,6)(['q','p','o','n','m','l'],-106), D2L_4_TYPE)
  call test(x2l4a-x2l4b, D2L_4_TYPE)
  call test(d2l(4,8)(['q','p','o','n','m','l','k','j'],-106)-d2l(4,8)(['q','p','o','n','m','l','k','j'],104), D2L_4_TYPE)


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
       print *, "Expected ", expName(expectation), ", got ", expName(found)
       stop 2
    end if
  end subroutine test

end program dtpUOpSimpleBinaryDummyArgInterface
