!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpSimpleUnaryElementalInterface
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct unary operators are used for a given TKR (elemental) (generic interface)
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
!*  Define several types with procedures for unary operators associated via
!*  generic interfaces and verify that the correct function is invoked for a given TKR.
!*  We can handle arrays via elemental or via one definition for rank 0 (scalar), rank 1, rank 2, ...
!*  This case uses elemental only.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleUnaryElementalInterfacemod

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
     module procedure unaryPlusK2
     module procedure unaryPlusK4
     module procedure unaryPlusL
     module procedure unaryPlus2K24
     module procedure unaryPlus2K42
     module procedure unaryPlus2L2
     module procedure unaryPlus2L4
  end interface operator(+)

contains

  elemental type(dk(2)) function unaryPlusK2(arg)
    class(dk(2)), intent(in) :: arg
    unaryPlusK2 = dk(2)(arg%ivar + 1009)
  end function unaryPlusK2

  elemental type(dk(4)) function unaryPlusK4(arg)
    class(dk(4)), intent(in) :: arg
    unaryPlusK4 = dk(4)(arg%ivar + 50051)
  end function unaryPlusK4

  elemental type(d2k(2,4)) function unaryPlus2K24(arg)
    class(d2k(2,4)), intent(in) :: arg
    unaryPlus2K24 = d2k(2,4)(arg%ivar + 1019, arg%ivar2 + 50021)
  end function unaryPlus2K24

  elemental type(d2k(4,2)) function unaryPlus2K42(arg)
    class(d2k(4,2)), intent(in) :: arg
    unaryPlus2K42 = d2k(4,2)(arg%ivar + 50023, arg%ivar2 + 1021)
  end function unaryPlus2K42

  elemental function unaryPlusL(arg)
    class(dl(*)), intent(in) :: arg
    type(dl(6)) :: unaryPlusL
    unaryPlusL = dl(6)([arg%cvar, '@'])
  end function unaryPlusL

  elemental function unaryPlus2L2(arg)
    class(d2l(2,*)), intent(in) :: arg
    type(d2l(2,10)) :: unaryPlus2L2
    unaryPlus2L2 = d2l(2,10)([arg%cvar, '%'],arg%ivar + 1031)
  end function unaryPlus2L2

  elemental function unaryPlus2L4(arg)
    class(d2l(4,*)), intent(in) :: arg
    type(d2l(4,8)) :: unaryPlus2L4
    unaryPlus2L4 = d2l(4,8)([arg%cvar, '^'],arg%ivar + 50033)
  end function unaryPlus2L4

end module dtpUOpSimpleUnaryElementalInterfacemod


program dtpUOpSimpleUnaryElementalInterface

  use dtpUOpSimpleUnaryElementalInterfacemod
  implicit none
  
  type(dk(2)) :: xk2a
  type(dk(4)) :: xk4a, xk4b
  type(dl(5)) :: xla, xlb, xlc, xld
  type(d2k(2,4)) :: x2k24a, x2k24b, x2k24c, x2k24d
  type(d2k(4,2)) :: x2k42a, x2k42b, x2k42c, x2k42d
  type(d2l(2,9)) :: x2l2a, x2l2b
  type(d2l(4,7)) :: x2l4a, x2l4b

  integer :: i, j

  xk2a   =  dk(2)(1)
  xk4a   =  dk(4)(11)
  xk4b   =  dk(4)(12)

  xla    =  dl(5)(['A','b','C','d','E'])
  xlb    =  dl(5)(['f','G','h','I','j'])
  xlc    =  dl(5)(['K','l','M','n','O'])
  xld    =  dl(5)(['p','Q','r','S','t'])

  x2k24a =  d2k(2,4)(21,31)
  x2k24b =  d2k(2,4)(22,32)
  x2k24c =  d2k(2,4)(23,33)
  x2k24d =  d2k(2,4)(24,34)

  x2k42a =  d2k(4,2)(41,51)
  x2k42b =  d2k(4,2)(42,52)
  x2k42c =  d2k(4,2)(43,53)
  x2k42d =  d2k(4,2)(44,54)

  x2l2a  =  d2l(2,9)(['a','B','c','D','e','F','g','H','i'],41)
  x2l2b  =  d2l(2,9)(['A','b','C','d','E','f','G','h','I'],42)
  x2l4a  =  d2l(4,7)(['c','D','e','F','g','H','i'],43)
  x2l4b  =  d2l(4,7)(['C','d','E','f','G','h','I'],44)

  print *, "Compare:"

  print *, "unaryPlusK2 (R0)", dk(2)(xk2a%ivar+1009), [integer::]
  print *, "unaryPlusK4 (R0)", dk(4)(xk4a%ivar+50051), [integer::]
  print *, "unaryPlusK2 (R0)", dk(2)(1+1009), [integer::]
  print *, "unaryPlusK4 (R0)", dk(4)(2+50051), [integer::]

  print *, "unaryPlusK4 (R1)", [dk(4)(xk4a%ivar+50051), dk(4)(xk4b%ivar+50051)], [2]
  print *, "unaryPlusK4 (R2)", [dk(4)(xk4a%ivar+50051), dk(4)(xk4b%ivar+50051)], [2,1]
  print *, "unaryPlusK4 (R1)", [dk(4)(1+50051), dk(4)(2+50051)], [2]
  print *, "unaryPlusK4 (R2)", [((dk(4)(50051+i*10+j), i=1,3),j=1,4)], [3,4]

  print *, "unaryPlus2K24 (R0)", d2k(2,4)(x2k24a%ivar+1019,x2k24a%ivar2+50021), [integer::]
  print *, "unaryPlus2K24 (R0)", d2k(2,4)(21+1019,31+50021), [integer::]
  print *, "unaryPlus2K24 (R2)", reshape([unaryPlus2K24(x2k24a), unaryPlus2K24(x2k24b), unaryPlus2K24(x2k24c), &
                                          unaryPlus2K24(x2k24d), unaryPlus2K24(x2k24a), unaryPlus2K24(x2k24b)],[3,2]), [3,2]

  print *, "unaryPlus2K42 (R0)", d2k(4,2)(x2k42a%ivar+50023,x2k42a%ivar2+1021), [integer::]
  print *, "unaryPlus2K42 (R0)", d2k(4,2)(21+50023,31+1021), [integer::]
  print *, "unaryPlus2K42 (R2)", [unaryPlus2K42(x2k42a), unaryPlus2K42(x2k42d), unaryPlus2K42(x2k42b), &
                                  unaryPlus2K42(x2k42a), unaryPlus2K42(x2k42c), unaryPlus2K42(x2k42b)], [3,2]

  print *, "unaryPlusL (R0)", dl(6)(['A','b','C','d','E','@']), [integer::]
  print *, "unaryPlusL (R1)", [dl(6)(['f','G','h','I','j','@']), dl(6)(['K','l','M','n','O','@']), dl(6)(['p','Q','r','S','t','@'])], [3]
  print *, "unaryPlusL (R2)", [dl(6)(['A','b','C','d','E','@']), dl(6)(['f','G','h','I','j','@']), dl(6)(['K','l','M','n','O','@']), &
                               dl(6)(['p','Q','r','S','t','@']), dl(6)(['K','l','M','n','O','@']), dl(6)(['f','G','h','I','j','@'])], [2,3]

  print *, "unaryPlus2L2 (R0)", d2l(2,x2l2a%l+1)([x2l2a%cvar,'%'],x2l2a%ivar+1031), [integer::]
  print *, "unaryPlus2L2 (R1)", [d2l(2,x2l2a%l+1)([x2l2a%cvar,'%'],x2l2a%ivar+1031), d2l(2,x2l2b%l+1)([x2l2b%cvar,'%'],x2l2b%ivar+1031)], [2]
  print *, "unaryPlus2L4 (R0)", d2l(4,x2l4a%l+1)([x2l4a%cvar,'^'],x2l4a%ivar+50033), [integer::]

  print *, "with:"

  print *, "unaryPlusK2 (R0)", +xk2a, shape(+xk2a)
  print *, "unaryPlusK4 (R0)", +xk4a, shape(+xk4a)
  print *, "unaryPlusK2 (R0)", +dk(2)(1), shape(+dk(2)(1))
  print *, "unaryPlusK4 (R0)", +dk(4)(2), shape(+dk(4)(2))

  print *, "unaryPlusK4 (R1)", +[xk4a, xk4b], shape(+[xk4a, xk4b])
  print *, "unaryPlusK4 (R2)", +reshape([xk4a, xk4b],[2,1]), shape(+reshape([xk4a, xk4b],[2,1]))
  print *, "unaryPlusK4 (R1)", +[dk(4)(1), dk(4)(2)], shape(+[dk(4)(1), dk(4)(2)])
  print *, "unaryPlusK4 (R2)", +reshape([((dk(4)(i*10+j),i=1,3),j=1,4)],[3,4]), shape(+reshape([((dk(4)(0),i=1,3),j=1,4)],[3,4]))

  print *, "unaryPlus2K24 (R0)", +x2k24a, shape(+x2k24a)
  print *, "unaryPlus2K24 (R0)", +d2k(2,4)(21,31), shape(+d2k(2,4)(21,31))
  print *, "unaryPlus2K24 (R2)", +reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]), &
                              shape(+reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]))

  print *, "unaryPlus2K42 (R0)", +x2k42a, shape(+x2k42a)
  print *, "unaryPlus2K42 (R0)", +d2k(4,2)(21,31), shape(+d2k(4,2)(21,31))
  print *, "unaryPlus2K42 (R2)", +reshape([x2k42a, x2k42d, x2k42b, x2k42a, x2k42c, x2k42b],[3,2]), &
                              shape(+reshape([x2k42a, x2k42b, x2k42c, x2k42d, x2k42a, x2k42b],[3,2]))

  print *, "unaryPlusL (R0)", +xla, shape(+xla)
  print *, "unaryPlusL (R1)", +[xlb, xlc, xld], shape(+[xlb, xlc, xld])
  print *, "unaryPlusL (R2)", +reshape([xla,xlb,xlc,xld,xlc,xlb],[2,3]), shape(+reshape([xla,xlb,xlc,xld,xlc,xlb],[2,3]))

  print *, "unaryPlus2L2 (R0)", +x2l2a, shape(+x2l2a)
  print *, "unaryPlus2L2 (R1)", +[x2l2a, x2l2b], shape(+[x2l2a, x2l2b])
  print *, "unaryPlus2L4 (R0)", +x2l4a, shape(+x2l4a)

  print *, "done"

end program dtpUOpSimpleUnaryElementalInterface
