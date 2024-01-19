!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2009-02-11
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct binary operators are used for a given TKR (elemental only, generic interface)
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
!*  generic interfaces, and verify that the correct function is invoked for a given TKR.
!*  We can handle arrays via elemental or via separate definitions for each rank pair.
!*  This case uses only elemental definitions.
!*  To reduce code inflation, we don't define routines for all ranks for all types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpSimpleBinaryElementalInterfacemod

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
    module procedure binaryPlusK2
    module procedure binaryPlusK4
    module procedure binaryPlus2K24
    module procedure binaryPlus2K42
    module procedure binaryPlusL
    module procedure binaryPlus2L2
    module procedure binaryPlus2L4
  end interface operator(+)

contains

  elemental type(dk(2)) function binaryPlusK2(this,that)
    class(dk(2)), intent(in) :: this, that
    binaryPlusK2 = dk(2)(this %ivar + that%ivar + 1009)
  end function binaryPlusK2

  elemental type(dk(4)) function binaryPlusK4(this,that)
    class(dk(4)), intent(in) :: this, that
    binaryPlusK4 = dk(4)(this %ivar + that%ivar + 1000003)
  end function binaryPlusK4

  elemental type(d2k(2,4)) function binaryPlus2K24(this,that)
    class(d2k(2,4)), intent(in) :: this, that
    binaryPlus2K24 = d2k(2,4)(this %ivar + that%ivar + 1033, this %ivar2 + that%ivar2 + 1000081)
  end function binaryPlus2K24

  elemental type(d2k(4,2)) function binaryPlus2K42(this,that)
    class(d2k(4,2)), intent(in) :: this, that
    binaryPlus2K42 = d2k(4,2)(this %ivar + that%ivar + 1000099, this %ivar2 + that%ivar2 + 1039)
  end function binaryPlus2K42

  elemental function binaryPlusL(this,that)
    class(dl(*)), intent(in) :: this, that
    type(dl(10)) :: binaryPlusL
    binaryPlusL = dl(10)([this %cvar, that%cvar])
  end function binaryPlusL

  elemental function binaryPlus2L2(this,that)
    class(d2l(2,*)), intent(in) :: this, that
    type(d2l(2,18)) :: binaryPlus2L2
    binaryPlus2L2 = d2l(2,18)([this%cvar, that%cvar], this%ivar + that%ivar + 2003)
  end function binaryPlus2L2

  elemental function binaryPlus2L4(this,that)
    class(d2l(4,*)), intent(in) :: this, that
    type(d2l(4,14)) :: binaryPlus2L4
    binaryPlus2L4 = d2l(4,14)([this%cvar, that%cvar],this %ivar + that%ivar + 1000117)
  end function binaryPlus2L4

end module dtpUOpSimpleBinaryElementalInterfacemod


program dtpUOpSimpleBinaryElementalInterface

  use dtpUOpSimpleBinaryElementalInterfacemod
  implicit none

  type(dk(2)) :: xk2a, xk2b, xk2c, xk2arr(1)
  type(dk(4)) :: xk4a, xk4b, xk4c, xk4arr(1,2,3)
  type(dl(5)) :: xla, xlb, xlc, xld
  type(d2k(2,4)) :: x2k24a, x2k24b, x2k24c, x2k24d
  type(d2l(2,9)) :: x2l2a, x2l2b
  type(d2l(4,7)) :: x2l4a, x2l4b

  integer :: i, j

  xk2a   = dk(2)(1)
  xk2b   = dk(2)(3)
  xk2c   = dk(2)(9)
  xk2arr = dk(2)(5)
  xk4a   = dk(4)(11)
  xk4b   = dk(4)(12)
  xk4c   = dk(4)(22)
  xk4arr = reshape([(dk(4)(1000000+i), i=1,6)],[1,2,3])

  xla    = dl(5)(['A','b','C','d','E'])
  xlb    = dl(5)(['f','G','h','I','j'])
  xlc    = dl(5)(['K','l','M','n','O'])
  xld    = dl(5)(['p','Q','r','S','t'])

  x2k24a = d2k(2,4)(21,31)
  x2k24b = d2k(2,4)(22,32)
  x2k24c = d2k(2,4)(23,33)
  x2k24d = d2k(2,4)(24,34)

  x2l2a  = d2l(2,9)(['a','B','c','D','e','F','g','H','i'],41)
  x2l2b  = d2l(2,9)(['A','b','C','d','E','f','G','h','I'],42)
  x2l4a  = d2l(4,7)(['c','D','e','F','g','H','i'],43)
  x2l4b  = d2l(4,7)(['C','d','E','f','G','h','I'],44)

  print *, "Compare:"

  print *, "binaryPlusK2", dk(2)(xk2a%ivar+xk2b%ivar+1009), [integer::]
  print *, "binaryPlusK4", dk(4)(xk4a%ivar+xk4b%ivar+1000003), [integer::]
  print *, "binaryPlusK2", dk(2)(1+2+1009), [integer::]
  print *, "binaryPlusK4", dk(4)(1+2+1000003), [integer::]

  print *, "binaryPlusK2", [dk(2)(xk2a%ivar+xk2b%ivar+1009), dk(2)(xk2b%ivar+xk2c%ivar+1009)], [2]
  print *, "binaryPlusK2", [dk(2)(xk2arr(1)%ivar+xk2arr(1)%ivar+1009)], [1]
  print *, "binaryPlusK4", [((dk(4)(xk4arr(1,i,j)%ivar+xk4arr(1,i,j)%ivar+1000003),i=1,2),j=1,3)], [1,2,3]

  print *, "binaryPlusK4", [dk(4)(xk4a%ivar+xk4c%ivar+1000003), dk(4)(xk4b%ivar+xk4b%ivar+1000003)], [2]
  print *, "binaryPlusK4", [dk(4)(xk4a%ivar+xk4b%ivar+1000003), dk(4)(xk4b%ivar+xk4c%ivar+1000003)], [2, 1]
  print *, "binaryPlusK4", [dk(4)(1+100+1000003), dk(4)(2+200+1000003)], [2]
  print *, "binaryPlusK4", [((dk(4)(i+j+1000003),i=1,3),j=1,4)], [3,4]

  print *, "binaryPlus2K24", d2k(2,4)(2*x2k24a%ivar+1033,2*x2k24a%ivar2+1000081), [integer::]
  print *, "binaryPlus2K24", d2k(2,4)(21+21+1033,31+31+1000081), [integer::]
  print *, "binaryPlus2K24", [d2k(2,4)(2*x2k24a%ivar+1033,2*x2k24a%ivar2+1000081), d2k(2,4)(2*x2k24b%ivar+1033,2*x2k24b%ivar2+1000081), &
                              d2k(2,4)(2*x2k24c%ivar+1033,2*x2k24c%ivar2+1000081), d2k(2,4)(2*x2k24d%ivar+1033,2*x2k24d%ivar2+1000081), &
                              d2k(2,4)(2*x2k24a%ivar+1033,2*x2k24a%ivar2+1000081), d2k(2,4)(2*x2k24b%ivar+1033,2*x2k24b%ivar2+1000081)], [6]
  print *, "binaryPlus2K24", [d2k(2,4)(2*x2k24a%ivar+1033,2*x2k24a%ivar2+1000081), d2k(2,4)(2*x2k24d%ivar+1033,2*x2k24d%ivar2+1000081), &
                              d2k(2,4)(2*x2k24b%ivar+1033,2*x2k24b%ivar2+1000081), d2k(2,4)(2*x2k24a%ivar+1033,2*x2k24a%ivar2+1000081), &
                              d2k(2,4)(2*x2k24c%ivar+1033,2*x2k24c%ivar2+1000081), d2k(2,4)(2*x2k24b%ivar+1033,2*x2k24b%ivar2+1000081)], [2,3]

  print *, "binaryPlusL", dl(10)(['A','b','C','d','E','A','b','C','d','E']), [integer::]
  print *, "binaryPlusL", [dl(10)(['f','G','h','I','j','f','G','h','I','j']), dl(10)(['K','l','M','n','O','K','l','M','n','O']), &
                           dl(10)(['p','Q','r','S','t','p','Q','r','S','t'])], [3]

  print *, "binaryPlus2L2", d2l(2,18)(['a','B','c','D','e','F','g','H','i','A','b','C','d','E','f','G','h','I'],42+41+2003), [integer::]
  x2l2a  = d2l(2,9)(['a','B','c','D','e','F','g','H','i'],41)
  x2l2b  = d2l(2,9)(['A','b','C','d','E','f','G','h','I'],42)
  x2l4a  = d2l(4,7)(['c','D','e','F','g','H','i'],43)
  x2l4b  = d2l(4,7)(['C','d','E','f','G','h','I'],44)

  print *, "binaryPlus2L2", [d2l(2,18)(['a','B','c','D','e','F','g','H','i','a','B','c','D','e','F','g','H','i'],41+41+2003), &
                             d2l(2,18)(['A','b','C','d','E','f','G','h','I','A','b','C','d','E','f','G','h','I'],42+42+2003)], [2]
  print *, "binaryPlus2L4", d2l(4,x2l4a%l+x2l4b%l)([x2l4a%cvar,x2l4b%cvar], x2l4a%ivar+x2l4b%ivar+1000117), [integer::]
  print *, "binaryPlus2L4", [d2l(4,x2l4a%l+x2l4b%l)([x2l4a%cvar,x2l4b%cvar], x2l4a%ivar+x2l4b%ivar+1000117), &
                             d2l(4,x2l4b%l+x2l4a%l)([x2l4b%cvar,x2l4a%cvar], x2l4b%ivar+x2l4a%ivar+1000117)], [2]
  print *, "binaryPlus2L4", [d2l(4,x2l4a%l+x2l4b%l)([x2l4a%cvar,x2l4b%cvar], x2l4a%ivar+x2l4b%ivar+1000117), &
                             d2l(4,x2l4a%l+x2l4a%l)([x2l4a%cvar,x2l4a%cvar], x2l4a%ivar+x2l4a%ivar+1000117)], [2]
  print *, "binaryPlus2K24", [d2k(2,4)(x2k24a%ivar+x2k24a%ivar+1033, x2k24a%ivar2+x2k24a%ivar2+1000081), &
                              d2k(2,4)(x2k24a%ivar+x2k24b%ivar+1033, x2k24a%ivar2+x2k24b%ivar2+1000081), &
                              d2k(2,4)(x2k24a%ivar+x2k24c%ivar+1033, x2k24a%ivar2+x2k24c%ivar2+1000081), &
                              d2k(2,4)(x2k24a%ivar+x2k24d%ivar+1033, x2k24a%ivar2+x2k24d%ivar2+1000081), &
                              d2k(2,4)(x2k24a%ivar+x2k24a%ivar+1033, x2k24a%ivar2+x2k24a%ivar2+1000081), &
                              d2k(2,4)(x2k24a%ivar+x2k24b%ivar+1033, x2k24a%ivar2+x2k24b%ivar2+1000081)], [3,2]

  print *, "with:"

  print *, "binaryPlusK2", xk2a+xk2b, shape(xk2a+xk2b)
  print *, "binaryPlusK4", xk4a+xk4b, shape(xk4a+xk4b)
  print *, "binaryPlusK2", dk(2)(1)+dk(2)(2), shape(dk(2)(1)+dk(2)(1))
  print *, "binaryPlusK4", dk(4)(1)+dk(4)(2), shape(dk(4)(2)+dk(4)(2))

  print *, "binaryPlusK2", [xk2a, xk2b]+[xk2b, xk2c], shape([xk2a, xk2b]+[xk2b, xk2c])
  print *, "binaryPlusK2", xk2arr+xk2arr, shape(xk2arr+xk2arr)
  print *, "binaryPlusK4", xk4arr+xk4arr, shape(xk4arr+xk4arr)

  print *, "binaryPlusK4", [xk4a, xk4b]+[xk4c, xk4b], shape([xk4a, xk4b]+[xk4c, xk4b])
  print *, "binaryPlusK4", reshape([xk4a, xk4b],[2,1])+reshape([xk4b, xk4c],[2,1]), shape(reshape([xk4a, xk4b],[2,1])+reshape([xk4b, xk4c],[2,1]))
  print *, "binaryPlusK4", [dk(4)(1), dk(4)(2)]+[dk(4)(100), dk(4)(200)], shape([dk(4)(1), dk(4)(2)]+[dk(4)(100), dk(4)(200)])
  print *, "binaryPlusK4", reshape([((dk(4)(i),i=1,3),j=1,4)],[3,4])+reshape([((dk(4)(j),i=1,3),j=1,4)],[3,4]), shape(reshape([((dk(4)(i),i=1,3),j=1,4)],[3,4])+reshape([((dk(4)(j),i=1,3),j=1,4)],[3,4]))

  print *, "binaryPlus2K24", x2k24a+x2k24a, shape(x2k24a+x2k24a)
  print *, "binaryPlus2K24", d2k(2,4)(21,31)+d2k(2,4)(21,31), shape(d2k(2,4)(21,31)+d2k(2,4)(21,31))
  print *, "binaryPlus2K24", [x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b]+[x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b], &
                             shape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b]+[x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b])
  print *, "binaryPlus2K24", reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2])+reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]), &
                             shape(reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2])+reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]))

  print *, "binaryPlusL", xla+xla, shape(xla+xla)
  print *, "binaryPlusL", [xlb, xlc, xld]+[xlb, xlc, xld], shape([xlb, xlc, xld]+[xlb, xlc, xld])

  print *, "binaryPlus2L2", x2l2a+x2l2a, shape(x2l2a+x2l2a)
  print *, "binaryPlus2L2", [x2l2a, x2l2b]+[x2l2a, x2l2b], shape([x2l2a, x2l2b]+[x2l2a, x2l2b])
  print *, "binaryPlus2L4", x2l4a+x2l4b, shape(x2l4a+x2l4b)
  print *, "binaryPlus2L4", [x2l4a, x2l4b]+[x2l4b, x2l4a], shape([x2l4a, x2l4b]+[x2l4b, x2l4a])

  print *, "binaryPlus2L4", x2l4a+[x2l4b, x2l4a], shape(x2l4a+[x2l4b, x2l4a])
  print *, "binaryPlus2K24", x2k24a+reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]), &
                             shape(x2k24a+reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]))

  print *, "done"

end program dtpUOpSimpleBinaryElementalInterface
