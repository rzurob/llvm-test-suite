!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpUOpUnaryElementalTKRMixInterface
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2009-02-11
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : User-Defined Operators
!*
!*  SECONDARY FUNCTIONS TESTED : correct unary operators are used for a given TKR (mixed elemental and non-elemental, generic interface)
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
!*  generic interfaces, and verify that the correct function is invoked for a given TKR.
!*  We can handle arrays via elemental or via one definition for rank 0 (scalar), rank 1, rank 2, ...
!*  This case uses both elemental and multiple definitions.
!*  To reduce code inflation, we don't define routines for all ranks for all types.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpUOpUnaryElementalTKRMixInterfacemod

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
    module procedure unaryPlusK2 ! elemental
    module procedure unaryPlusK4 ! elemental
    module procedure unaryPlusK4R1
    module procedure unaryPlusK4R2
    module procedure unaryPlus2K24 ! elemental
    module procedure unaryPlus2K24R2
    module procedure unaryPlusL ! elemental
    module procedure unaryPlusLR1
    module procedure unaryPlusLR2
    module procedure unaryPlus2L2 ! elemental
    module procedure unaryPlus2L2R1
    module procedure unaryPlus2L4 ! elemental
  end interface operator(+)

contains

  elemental type(dk(2)) function unaryPlusK2(arg)
    class(dk(2)), intent(in) :: arg
    unaryPlusK2 = dk(2)(arg%ivar + 1)
  end function unaryPlusK2

  elemental type(dk(4)) function unaryPlusK4(arg)
    class(dk(4)), intent(in) :: arg
    unaryPlusK4 = dk(4)(arg%ivar + 2)
  end function unaryPlusK4

  function unaryPlusK4R1(arg)
    class(dk(4)), intent(in) :: arg(:)
    type(dk(4)) :: unaryPlusK4R1(size(arg))
    integer :: i
    unaryPlusK4R1 = [(dk(4)(arg(i)%ivar + 17*i), i=lbound(arg,1),ubound(arg,1))]
  end function unaryPlusK4R1

  function unaryPlusK4R2(arg)
    class(dk(4)), intent(in) :: arg(:,:)
    type(dk(4)) :: unaryPlusK4R2(size(arg,1),size(arg,2))
    integer :: i, j
    ! 1021 and 1031 are prime numbers
    unaryPlusK4R2 = reshape([((dk(4)(arg(i,j)%ivar + 1021*i + 1031*j), i=lbound(arg,1),ubound(arg,1)), j=lbound(arg,2),ubound(arg,2))],[size(arg,1),size(arg,2)])
  end function unaryPlusK4R2


  elemental type(d2k(2,4)) function unaryPlus2K24(arg)
    class(d2k(2,4)), intent(in) :: arg
    unaryPlus2K24 = d2k(2,4)(arg%ivar + 32, arg%ivar2 + 107)
  end function unaryPlus2K24

  function unaryPlus2K24R2(arg)
    class(d2k(2,4)), intent(in) :: arg(:,:)
    type(d2k(2,4)) :: unaryPlus2K24R2(size(arg,2),size(arg,1))
    integer :: i, j
    unaryPlus2K24R2 = transpose(arg)
  end function unaryPlus2K24R2


  elemental function unaryPlusL(arg)
    class(dl(*)), intent(in) :: arg
    type(dl(6)) :: unaryPlusL
    unaryPlusL = dl(6)([arg%cvar, '@'])
  end function unaryPlusL

  function unaryPlusLR1(arg)
    class(dl(*)), intent(in) :: arg(:)
    type(dl(arg%l+1)) :: unaryPlusLR1(size(arg))
    integer :: i
    unaryPlusLR1 = [(dl(arg%l+1)([arg(i)%cvar, achar(32+i)]), i=lbound(arg,1),ubound(arg,1))]
  end function unaryPlusLR1

  function unaryPlusLR2(arg)
    class(dl(*)), intent(in) :: arg(:,:)
    type(dl(arg%l+1)) :: unaryPlusLR2(size(arg,2),size(arg,1))
    integer :: i, j, k
    type(dl(6)) temp(size(arg))
!    unaryPlusLR2 = reshape([((dl(arg%l+1)([arg(i,j)%cvar, achar(mod(83*i+89*j,96)+32)]), j=lbound(arg,2),ubound(arg,2)), i=lbound(arg,1),ubound(arg,1))],[size(arg,2),size(arg,1)])
    k = 1
    do i = 1, ubound(arg,1)
        do j = 1, ubound(arg,2)
            temp(k)%cvar = [arg(i,j)%cvar,achar(mod(83*i+89*j,96)+32)]
            k = k + 1
        end do
    end do

    k = 1
    do j = 1, ubound(arg,1)
        do i = 1, ubound(arg,2)
            unaryPlusLR2(i,j)%cvar = temp(k)%cvar
            k = k + 1
        end do
    end do

  end function unaryPlusLR2


  elemental function unaryPlus2L2(arg)
    class(d2l(2,*)), intent(in) :: arg
    type(d2l(2,10)) :: unaryPlus2L2
    unaryPlus2L2 = d2l(2,10)([arg%cvar, '%'],arg%ivar + 4096)
  end function unaryPlus2L2

  integer function unaryPlus2L2R1(arg)
    class(d2l(2,*)), intent(in) :: arg(:)
    integer :: i
    unaryPlus2L2R1 = 0
    do i = 1,arg%l
       unaryPlus2L2R1 = unaryPlus2L2R1 + count(arg(:)%cvar(i) >= 'a')
    end do
  end function unaryPlus2L2R1


  elemental function unaryPlus2L4(arg)
    class(d2l(4,*)), intent(in) :: arg
    type(d2l(4,8)) :: unaryPlus2L4
    unaryPlus2L4 = d2l(4,8)([arg%cvar, '^'],arg%ivar + 8192)
  end function unaryPlus2L4

end module dtpUOpUnaryElementalTKRMixInterfacemod


program dtpUOpUnaryElementalTKRMixInterface

  use dtpUOpUnaryElementalTKRMixInterfacemod
  implicit none
  
  type(dk(2)) :: xk2a, xk2b, xk2arr(1)
  type(dk(4)) :: xk4a, xk4b, xk4arr(1,2,3)
  type(dl(5)) :: xla, xlb, xlc, xld
  type(d2k(2,4)) :: x2k24a, x2k24b, x2k24c, x2k24d
  type(d2l(2,9)) :: x2l2a, x2l2b
  type(d2l(4,7)) :: x2l4a, x2l4b

  integer :: i, j

  xk2a   = dk(2)(1)
  xk2b   = dk(2)(3)
  xk2arr = dk(2)(5)
  xk4a   = dk(4)(11)
  xk4b   = dk(4)(12)
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

  print *, "unaryPlusK2 (elt)", dk(2)(xk2a%ivar+1), [integer::]
  print *, "unaryPlusK4 (elt)", dk(4)(xk4a%ivar+2), [integer::]
  print *, "unaryPlusK2 (elt)", dk(2)(1+1), [integer::]
  print *, "unaryPlusK4 (elt)", dk(4)(2+2), [integer::]

  print *, "unaryPlusK2 (elt)", [dk(2)(xk2a%ivar+1), dk(2)(xk2b%ivar+1)], [2]
  print *, "unaryPlusK2 (elt)", [dk(2)(xk2arr(1)%ivar+1)], [1]
  print *, "unaryPlusK4 (elt)", [((dk(4)(xk4arr(1,i,j)%ivar+2),i=1,2),j=1,3)], [1,2,3]

  print *, "unaryPlusK4R1", [dk(4)(xk4a%ivar+1*17), dk(4)(xk4b%ivar+2*17)], [2]
  print *, "unaryPlusK4R2", [dk(4)(xk4a%ivar+1*1021+1031), dk(4)(xk4b%ivar+2*1021+1031)], [2,1]
  print *, "unaryPlusK4R1", [dk(4)(1+1*17), dk(4)(2+2*17)], [2]
  print *, "unaryPlusK4R2", [((dk(4)(1021*i + 1031*j), i=1,3),j=1,4)], [3,4]

  print *, "unaryPlus2K24 (elt)", d2k(2,4)(x2k24a%ivar+32,x2k24a%ivar2+107), [integer::]
  print *, "unaryPlus2K24 (elt)", d2k(2,4)(21+32,31+107), [integer::]
  print *, "unaryPlus2K24 (elt)", [d2k(2,4)(x2k24a%ivar+32,x2k24a%ivar2+107), d2k(2,4)(x2k24a%ivar+32,x2k24a%ivar2+107), &
                                   d2k(2,4)(x2k24b%ivar+32,x2k24b%ivar2+107), d2k(2,4)(x2k24c%ivar+32,x2k24c%ivar2+107), &
                                   d2k(2,4)(x2k24d%ivar+32,x2k24d%ivar2+107), d2k(2,4)(x2k24a%ivar+32,x2k24a%ivar2+107)], [6]
  print *, "unaryPlus2K24R2", [x2k24a, x2k24d, x2k24b, x2k24a, x2k24c, x2k24b], [2,3]

  print *, "unaryPlusL (elt)", dl(6)(['A','b','C','d','E','@']), [integer::]
  print *, "unaryPlusLR1", [dl(6)(['f','G','h','I','j','!']), dl(6)(['K','l','M','n','O','"']), dl(6)(['p','Q','r','S','t','#'])], [3]
  print *, "unaryPlusLR2", [dl(6)(['A','b','C','d','E',' ']), dl(6)(['f','G','h','I','j','y']), dl(6)(['K','l','M','n','O','s']), &
                            dl(6)(['p','Q','r','S','t','l']), dl(6)(['K','l','M','n','O','f']), dl(6)(['f','G','h','I','j','_'])], [2,3]

  print *, "unaryPlus2L2 (elt)", d2l(2,x2l2a%l+1)([x2l2a%cvar,'%'],x2l2a%ivar+4096), [integer::]
  print *, "unaryPlus2L2R1", count([x2l2a%cvar, x2l2b%cvar] >= 'a'), [integer::]
  print *, "unaryPlus2L4 (elt)", d2l(4,x2l4a%l+1)([x2l4a%cvar,'^'],x2l4a%ivar+8192), [integer::]
  print *, "unaryPlus2L4 (elt)", [d2l(4,x2l4a%l+1)([x2l4a%cvar, '^'],x2l4a%ivar + 8192), d2l(4,x2l4b%l+1)([x2l4b%cvar, '^'],x2l4b%ivar + 8192)], [2]

  print *, "with:"

  print *, "unaryPlusK2 (elt)", +xk2a, shape(+xk2a)
  print *, "unaryPlusK4 (elt)", +xk4a, shape(+xk4a)
  print *, "unaryPlusK2 (elt)", +dk(2)(1), shape(+dk(2)(1))
  print *, "unaryPlusK4 (elt)", +dk(4)(2), shape(+dk(4)(2))

  print *, "unaryPlusK2 (elt)", +[xk2a, xk2b], shape(+[xk2a, xk2b])
  print *, "unaryPlusK2 (elt)", +xk2arr, shape(+xk2arr)
  print *, "unaryPlusK4 (elt)", +xk4arr, shape(+xk4arr)

  print *, "unaryPlusK4R1", +[xk4a, xk4b], shape(+[xk4a, xk4b])
  print *, "unaryPlusK4R2", +reshape([xk4a, xk4b],[2,1]), shape(+reshape([xk4a, xk4b],[2,1]))
  print *, "unaryPlusK4R1", +[dk(4)(1), dk(4)(2)], shape(+[dk(4)(1), dk(4)(2)])
  print *, "unaryPlusK4R2", +reshape([((dk(4)(0),i=1,3),j=1,4)],[3,4]), shape(+reshape([((dk(4)(0),i=1,3),j=1,4)],[3,4]))

  print *, "unaryPlus2K24 (elt)", +x2k24a, shape(+x2k24a)
  print *, "unaryPlus2K24 (elt)", +d2k(2,4)(21,31), shape(+d2k(2,4)(21,31))
  print *, "unaryPlus2K24 (elt)", +[x2k24a, x2k24a, x2k24b, x2k24c, x2k24d, x2k24a], shape(+[x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b])
  print *, "unaryPlus2K24R2", +reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]), &
                              shape(+reshape([x2k24a, x2k24b, x2k24c, x2k24d, x2k24a, x2k24b],[3,2]))

  print *, "unaryPlusL (elt)", +xla, shape(+xla)
  print *, "unaryPlusLR1", +[xlb, xlc, xld], shape(+[xlb, xlc, xld])
  print *, "unaryPlusLR2", +reshape([xla,xlb,xlc,xld,xlc,xlb],[3,2]), shape(+reshape([xla,xlb,xlc,xld,xlc,xlb],[3,2]))

  print *, "unaryPlus2L2 (elt)", +x2l2a, shape(+x2l2a)
  print *, "unaryPlus2L2R1", +[x2l2a, x2l2b], shape(+[x2l2a, x2l2b])
  print *, "unaryPlus2L4 (elt)", +x2l4a, shape(+x2l4a)
  print *, "unaryPlus2L4 (elt)", +[x2l4a, x2l4b], shape(+[x2l4a, x2l4b])

  print *, "done"

end program dtpUOpUnaryElementalTKRMixInterface
