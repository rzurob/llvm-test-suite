!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpEntry
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : operator associated with procedure defined as ENTRY
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
!*  Define several types with type-bound procedures, some defined using the
!*  ENTRY mechanism, and generic bindings for binary operators and verify
!*  that the correct function is invoked when the operation appears as a dummy argument.
!*  Note that different entries can return different types.
!*
!*  (derived from dtpUOpSimpleBinaryDummyArg)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpEntrymod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
   contains
     generic :: operator(+) => binaryPlusK2, binaryPlusK4
     procedure, pass :: binaryPlusK2
     procedure, pass :: binaryPlusK4
     generic :: operator(-) => binaryMinusK2, binaryMinusK4
     procedure, pass :: binaryMinusK2
     procedure, pass :: binaryMinusK4
  end type dk

  type dl (l)
     integer, len  :: l
     character(1)  :: cvar(l)
   contains
     generic :: operator(+) => binaryPlusL
     procedure, pass :: binaryPlusL
     generic :: operator(-) => binaryMinusL
     procedure, pass :: binaryMinusL
  end type dl

  type d2k (k,k2)
     integer, kind :: k, k2
     integer(k)    :: ivar
     integer(k2)   :: ivar2
   contains
     generic :: operator(+) => binaryPlus2K22, binaryPlus2K24, binaryPlus2K42, binaryPlus2K44
     procedure, pass :: binaryPlus2K22
     procedure, pass :: binaryPlus2K24
     procedure, pass :: binaryPlus2K42
     procedure, pass :: binaryPlus2K44
     generic :: operator(-) => binaryMinus2K22, binaryMinus2K24, binaryMinus2K42, binaryMinus2K44
     procedure, pass :: binaryMinus2K22
     procedure, pass :: binaryMinus2K24
     procedure, pass :: binaryMinus2K42
     procedure, pass :: binaryMinus2K44
  end type d2k

  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     integer(k)    :: ivar
   contains
     generic :: operator(+) => binaryPlus2L2, binaryPlus2L4
     procedure, pass :: binaryPlus2L2
     procedure, pass :: binaryPlus2L4
     generic :: operator(-) => binaryMinus2L2, binaryMinus2L4
     procedure, pass :: binaryMinus2L2
     procedure, pass :: binaryMinus2L4
  end type d2l

contains

  ! Same types in and out, different function
  type(dk(2)) function binaryPlusK2(this,that)
    class(dk(2)), intent(in) :: this, that
    type(dk(2)) :: binaryMinusK2
    binaryPlusK2 = dk(2)(this%ivar + that%ivar + 64)
    return
    entry binaryMinusK2(this,that)
    binaryMinusK2 = dk(2)(this%ivar - that%ivar - 128)
  end function binaryPlusK2

  type(dk(4)) function binaryPlusK4(this,that)
    class(dk(4)), intent(in) :: this, that
    type(dk(4)) :: binaryMinusK4
    binaryPlusK4 = dk(4)(this%ivar + that%ivar + 32768)
    return
    entry binaryMinusK4(this,that)
    binaryMinusK4 = dk(4)(this%ivar - that%ivar - 65536)
  end function binaryPlusK4


  ! Quasi- same type in and out, but different length parameter out for plus as for minus
  function binaryPlusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(this%l+1)) :: binaryPlusL
    type(dl(this%l-1)) :: binaryMinusL
    binaryPlusL = dl(this%l+1)([achar(mod(iachar(this%cvar) + iachar(that%cvar), 96)+32), 'l'])
    return
    entry binaryMinusL(this,that)
    binaryMinusL = dl(this%l-1)(['m', achar(mod(iachar(this%cvar(1:this%l-2)) + iachar(that%cvar(1:that%l-2)), 96)+32)])
  end function binaryPlusL


  ! Same operation, but different types in and out
  function binaryPlus2K22(this22,that22)
    class(d2k(2,2)), intent(in) :: this22, that22
    class(d2k(2,4)), intent(in) :: this24, that24
    class(d2k(4,2)), intent(in) :: this42, that42
    class(d2k(4,4)), intent(in) :: this44, that44
    type(d2k(2,2)) :: binaryPlus2K22
    type(d2k(2,4)) :: binaryPlus2K24
    type(d2k(4,2)) :: binaryPlus2K42
    type(d2k(4,4)) :: binaryPlus2K44

    ! entry binaryPlus2K22(this22,that22)
    binaryPlus2K22 = d2k(2,2)(this22%ivar + that22%ivar + 256, this22%ivar2 + that22%ivar2 + 106)
    return

    entry binaryPlus2K24(this24,that24)
    binaryPlus2K24 = d2k(2,4)(this24%ivar + that24%ivar + 512, this24%ivar2 + that24%ivar2 + 107)
    return

    entry binaryPlus2K42(this42,that42)
    binaryPlus2K42 = d2k(4,2)(this42%ivar + that42%ivar + 131072, this42%ivar2 + that42%ivar2 + 108)
    return

    entry binaryPlus2K44(this44,that44)
    binaryPlus2K44 = d2k(4,4)(this44%ivar + that44%ivar + 262144, this44%ivar2 + that44%ivar2 + 109)
  end function binaryPlus2K22

  function binaryMinus2K22(this22,that22)
    class(d2k(2,2)), intent(in) :: this22, that22
    class(d2k(2,4)), intent(in) :: this24, that24
    class(d2k(4,2)), intent(in) :: this42, that42
    class(d2k(4,4)), intent(in) :: this44, that44
    type(d2k(2,2)) :: binaryMinus2K22
    type(d2k(2,4)) :: binaryMinus2K24
    type(d2k(4,2)) :: binaryMinus2K42
    type(d2k(4,4)) :: binaryMinus2K44

    ! entry binaryMinus2K22(this22,that22)
    binaryMinus2K22 = d2k(2,2)(this22%ivar - that22%ivar - 1024, this22%ivar2 + that22%ivar2 + 110)
    return

    entry binaryMinus2K24(this24,that24)
    binaryMinus2K24 = d2k(2,4)(this24%ivar - that24%ivar - 2048, this24%ivar2 + that24%ivar2 + 111)
    return

    entry binaryMinus2K42(this42,that42)
    binaryMinus2K42 = d2k(4,2)(this42%ivar - that42%ivar - 524288, this42%ivar2 + that42%ivar2 + 112)
    return

    entry binaryMinus2K44(this44,that44)
    binaryMinus2K44 = d2k(4,4)(this44%ivar - that44%ivar - 1048576, this44%ivar2 + that44%ivar2 + 113)
  end function binaryMinus2K22


  ! Both length and kind different (NOT good style, but possible, and therefore needs to be tested)
  function binaryPlus2L2(this2,that2)
    class(d2l(2,*)), intent(in) :: this2, that2
    class(d2l(4,*)), intent(in) :: this4, that4
    type(d2l(2,this2%l+1)) :: binaryPlus2L2
    type(d2l(4,this4%l+1)) :: binaryPlus2L4
    type(d2l(2,this2%l-1)) :: binaryMinus2L2
    type(d2l(4,this4%l-1)) :: binaryMinus2L4

    !entry binaryPlus2L2(this2,that2)
    binaryPlus2L2 = d2l(2,this2%l+1)([achar(mod(iachar(this2%cvar) + iachar(that2%cvar), 96)+32), 'L'],this2%ivar + that2%ivar + 4096)
    return

    entry binaryPlus2L4(this4,that4)
    binaryPlus2L4 = d2l(4,this4%l+1)([achar(mod(iachar(this4%cvar) + iachar(that4%cvar), 96)+32), 'Y'],this4%ivar + that4%ivar + 2097152)
    return

    entry binaryMinus2L2(this2,that2)
    binaryMinus2L2 = d2l(2,this2%l-1)(['M', achar(mod(iachar(this2%cvar(1:this2%l-2)) + iachar(that2%cvar(1:that2%l-2)), 96)+32)],this2%ivar - that2%ivar - 8192)
    return

    entry binaryMinus2L4(this4,that4)
    binaryMinus2L4 = d2l(4,this4%l-1)(['Q', achar(mod(iachar(this4%cvar(1:this4%l-2)) + iachar(that4%cvar(1:that4%l-2)), 96)+32)],this4%ivar - that4%ivar - 4194304)
  end function binaryPlus2L2


end module dtpUOpEntrymod


program dtpUOpEntry

  use dtpUOpEntrymod
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
  x2l4b  =  d2l(4,7)(['c','d','e','f','g','h','i'],24)


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
  call test(d2l(2,8)(['z','y','x','w','v','u','t','s'],-16)+d2l(2,8)(['z','y','x','w','v','u','t','s'],-15), D2L_2_TYPE)
  call test(x2l2b-x2l2b, D2L_2_TYPE)
  call test(d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],-14)-d2l(2,10)(['z','y','x','w','v','u','t','s','r','p'],-13), D2L_2_TYPE)

  call test(x2l4a+x2l4a, D2L_4_TYPE)
  call test(d2l(4,6)(['q','p','o','n','m','l'],-8)+d2l(4,6)(['q','p','o','n','m','l'],-7), D2L_4_TYPE)
  call test(x2l4b-x2l4b, D2L_4_TYPE)
  call test(d2l(4,8)(['q','p','o','n','m','l','k','j'],-6)-d2l(4,8)(['q','p','o','n','m','l','k','j'],-5), D2L_4_TYPE)


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

end program dtpUOpEntry
