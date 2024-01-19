!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : operators defined in parent class (generic interface)
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
!*  Define several types with procedures and associate them with binary operators
!*  via generic interfaces. Also define types derived from these (appropriately named
!*  <type>Child) and verify that the correct function is invoked when the
!*  operation, applied to the child, appears as a dummy argument.
!*  Conceptual caveat: technically, this is not inheritance as it might be understood
!*  usually, but the use of interfaces and generic types is correct, nonetheless.
!*  Note that the type produced by the operation will be the parent type, since
!*  we are not returning pointers here.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpInheritedOperatorInterfacemod

  implicit none

  type dk (k)
     integer, kind :: k
     integer(k)    :: ivar
  end type dk

  type, extends(dk) :: dkChild
    character(6) :: mark = 'dkChild'
  end type


  type dl (l)
     integer, len  :: l
     character(1)  :: cvar(l)
  end type dl

  type, extends(dl) :: dlChild
    character(6) :: mark = 'dlChild'
  end type


  type d2k (k,k2)
     integer, kind :: k, k2
     integer(k)    :: ivar
     integer(k2)   :: ivar2
  end type d2k

  type, extends(d2k) :: d2kChild
    character(7) :: mark = 'd2kChild'
  end type


  type d2l (k,l)
     integer, kind :: k
     integer, len  :: l
     character(1)  :: cvar(l)
     integer(k)    :: ivar
  end type d2l

  type, extends(d2l) :: d2lChild
    character(7) :: mark = 'd2lChild'
  end type

  interface operator(+)
     module procedure binaryPlusK2
     module procedure binaryPlusK4
     module procedure binaryPlusL
     module procedure binaryPlus2K22
     module procedure binaryPlus2K24
     module procedure binaryPlus2K42
     module procedure binaryPlus2K44
     module procedure binaryPlus2L2
     module procedure binaryPlus2L4
  end interface operator(+)

  interface operator(-)
     module procedure binaryMinusK2
     module procedure binaryMinusK4
     module procedure binaryMinusL
     module procedure binaryMinus2K22
     module procedure binaryMinus2K24
     module procedure binaryMinus2K42
     module procedure binaryMinus2K44
     module procedure binaryMinus2L2
     module procedure binaryMinus2L4
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


end module dtpUOpInheritedOperatorInterfacemod


program dtpUOpInheritedOperatorInterface

  use dtpUOpInheritedOperatorInterfacemod
  implicit none

  type(dkChild(2)) :: xk2a, xk2b, xk2c, xk2d
  type(dkChild(4)) :: xk4a, xk4b, xk4c, xk4d

  type(dlChild(5)) :: xla, xlb
  type(dlChild(6)) :: xlc
  type(dlChild(4)) :: xld

  type(d2kChild(2,2)) :: x2k22a, x2k22b, x2k22c, x2k22d
  type(d2kChild(2,4)) :: x2k24a, x2k24b, x2k24c, x2k24d
  type(d2kChild(4,2)) :: x2k42a, x2k42b, x2k42c, x2k42d
  type(d2kChild(4,4)) :: x2k44a, x2k44b, x2k44c, x2k44d

  type(d2lChild(2,9)) :: x2l2a, x2l2b
  type(d2lChild(2,10)):: x2l2c
  type(d2lChild(2,8)) :: x2l2d
  type(d2lChild(4,7)) :: x2l4a, x2l4b
  type(d2lChild(4,8)) :: x2l4c
  type(d2lChild(4,6)) :: x2l4d

  integer, parameter :: DK_2_TYPE          = 1
  integer, parameter :: DK_4_TYPE          = 2
  integer, parameter :: DL_TYPE            = 3
  integer, parameter :: D2K_2_2_TYPE       = 4
  integer, parameter :: D2K_2_4_TYPE       = 5
  integer, parameter :: D2K_4_2_TYPE       = 6
  integer, parameter :: D2K_4_4_TYPE       = 7
  integer, parameter :: D2L_2_TYPE         = 8
  integer, parameter :: D2L_4_TYPE         = 9
  integer, parameter :: DK_2_CHILD_TYPE    = 10
  integer, parameter :: DK_4_CHILD_TYPE    = 11
  integer, parameter :: DL_CHILD_TYPE      = 12
  integer, parameter :: D2K_2_2_CHILD_TYPE = 13
  integer, parameter :: D2K_2_4_CHILD_TYPE = 14
  integer, parameter :: D2K_4_2_CHILD_TYPE = 15
  integer, parameter :: D2K_4_4_CHILD_TYPE = 16
  integer, parameter :: D2L_2_CHILD_TYPE   = 17
  integer, parameter :: D2L_4_CHILD_TYPE   = 18
  integer, parameter :: UNKNOWN_TYPE       = 19

  character(12) :: expName(19) = &
       [character(12):: "dk(2)", "dk(4)", "dl(*)", "d2k(2,2)", "d2k(2,4)", &
                        "d2k(4,2)", "d2k(4,4)", "d2l(2,*)", "d2l(4,*)", &
                        "dkChild(2)", "dkChild(4)", "dlChild(*)", "d2kChild(2,2)", "d2kChild(2,4)", &
                        "d2kChild(4,2)", "d2kChild(4,4)", "d2lChild(2,*)", "d2lChild(4,*)", "unknown"]

  xk2a  =  dkChild(2)(1)
  xk2b  =  dkChild(2)(2)
  xk4a  =  dkChild(4)(3)
  xk4b  =  dkChild(4)(4)

  xla   =  dlChild(5)(['a','b','c','d','e'])
  xlb   =  dlChild(5)(['a','b','c','d','e'])

  x2k22a  =  d2kChild(2,2)(5,6)
  x2k22b  =  d2kChild(2,2)(7,8)
  x2k24a  =  d2kChild(2,4)(9,10)
  x2k24b  =  d2kChild(2,4)(11,12)
  x2k42a  =  d2kChild(4,2)(13,14)
  x2k42b  =  d2kChild(4,2)(15,16)
  x2k44a  =  d2kChild(4,4)(17,18)
  x2k44b  =  d2kChild(4,4)(19,20)

  x2l2a  =  d2lChild(2,9)(['a','b','c','d','e','f','g','h','i'],21)
  x2l2b  =  d2lChild(2,9)(['a','b','c','d','e','f','g','h','i'],22)
  x2l4a  =  d2lChild(4,7)(['c','d','e','f','g','h','i'],23)
  x2l4b  =  d2lChild(4,7)(['c','d','e','f','g','h','i'],24)


  call test(xk2a+xk2a, DK_2_TYPE)
  call test(dkChild(2)(25)+dkChild(2)(26), DK_2_TYPE)
  call test(xk2b-xk2b, DK_2_TYPE)
  call test(dkChild(2)(27)-dkChild(2)(28), DK_2_TYPE)

  call test(xk4a+xk4a, DK_4_TYPE)
  call test(dkChild(4)(33)+dkChild(4)(34), DK_4_TYPE)
  call test(xk4b-xk4b, DK_4_TYPE)
  call test(dkChild(4)(35)-dkChild(4)(36), DK_4_TYPE)

  call test(xla+xla, DL_TYPE)
  call test(dlChild(4)(['d','e','f','g'])+dlChild(4)(['d','e','f','g']), DL_TYPE)
  call test(xlb-xlb, DL_TYPE)
  call test(dlChild(6)(['d','e','f','g','h','i'])-dlChild(6)(['d','e','f','g','h','i']), DL_TYPE)

  call test(x2k22a+x2k22a, D2K_2_2_TYPE)
  call test(d2kChild(2,2)(41,42)+d2kChild(2,2)(43,44), D2K_2_2_TYPE)
  call test(x2k22b-x2k22b, D2K_2_2_TYPE)
  call test(d2kChild(2,2)(45,46)-d2kChild(2,2)(47,48), D2K_2_2_TYPE)

  call test(x2k24a+x2k24a, D2K_2_4_TYPE)
  call test(d2kChild(2,4)(57,58)+d2kChild(2,4)(59,60), D2K_2_4_TYPE)
  call test(x2k24b-x2k24b, D2K_2_4_TYPE)
  call test(d2kChild(2,4)(-60,-59)-d2kChild(2,4)(-58,-57), D2K_2_4_TYPE)

  call test(x2k42a+x2k42a, D2K_4_2_TYPE)
  call test(d2kChild(4,2)(-48,-47)+d2kChild(4,2)(-46,-45), D2K_4_2_TYPE)
  call test(x2k42b-x2k42b, D2K_4_2_TYPE)
  call test(d2kChild(4,2)(-44,-43)-d2kChild(4,2)(-42,-41), D2K_4_2_TYPE)

  call test(x2k44a+x2k44a, D2K_4_4_TYPE)
  call test(d2kChild(4,4)(-32,-31)+d2kChild(4,4)(-30,-29), D2K_4_4_TYPE)
  call test(x2k44b-x2k44b, D2K_4_4_TYPE)
  call test(d2kChild(4,4)(-28,-27)-d2kChild(4,4)(-26,-25), D2K_4_4_TYPE)

  call test(x2l2a+x2l2a, D2L_2_TYPE)
  call test(d2lChild(2,8)(['z','y','x','w','v','u','t','s'],-16)+d2lChild(2,8)(['z','y','x','w','v','u','t','s'],-15), D2L_2_TYPE)
  call test(x2l2b-x2l2b, D2L_2_TYPE)
  call test(d2lChild(2,10)(['z','y','x','w','v','u','t','s','r','p'],-14)-d2lChild(2,10)(['z','y','x','w','v','u','t','s','r','p'],-13), D2L_2_TYPE)

  call test(x2l4a+x2l4a, D2L_4_TYPE)
  call test(d2lChild(4,6)(['q','p','o','n','m','l'],-8)+d2lChild(4,6)(['q','p','o','n','m','l'],-7), D2L_4_TYPE)
  call test(x2l4b-x2l4b, D2L_4_TYPE)
  call test(d2lChild(4,8)(['q','p','o','n','m','l','k','j'],-6)-d2lChild(4,8)(['q','p','o','n','m','l','k','j'],-5), D2L_4_TYPE)


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

    ! and the child types:
    type is (dkChild(2));    found = DK_2_CHILD_TYPE;    print *, arg
    type is (dkChild(4));    found = DK_4_CHILD_TYPE;    print *, arg
    type is (dlChild(*));    found = DL_CHILD_TYPE;      print *, arg%l, arg
    type is (d2kChild(2,2)); found = D2K_2_2_CHILD_TYPE; print *, arg
    type is (d2kChild(2,4)); found = D2K_2_4_CHILD_TYPE; print *, arg
    type is (d2kChild(4,2)); found = D2K_4_2_CHILD_TYPE; print *, arg
    type is (d2kChild(4,4)); found = D2K_4_4_CHILD_TYPE; print *, arg
    type is (d2lChild(2,*)); found = D2L_2_CHILD_TYPE;   print *, arg%l, arg
    type is (d2lChild(4,*)); found = D2L_4_CHILD_TYPE;   print *, arg%l, arg

    class default;      found = UNKNOWN_TYPE
    end select
    if (expectation /= found) then
       print *, "Expected ", expName(expectation), ", got ", expName(found)
       stop 2
    end if
  end subroutine test

end program dtpUOpInheritedOperatorInterface
