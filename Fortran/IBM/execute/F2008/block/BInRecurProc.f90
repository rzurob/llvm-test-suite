!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BInRecurProc
!*
!*  DATE                       : 2010-12-14
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : block in different contexts (recursive)
!*
!*  DESCRIPTION
!*
!*  Use a BLOCK inside a recursive procedure.  In "build", we take advantage of
!*  the "exit" capability of a block.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BInRecurProc

  type :: tree
     character :: datum
     type(tree), pointer :: left => null()
     type(tree), pointer :: right => null()
  end type tree

  implicit none

  type(tree), pointer :: root
  character(81) :: map = "(a(b(c(d)(e(f(g)(h))()))(i(j)(k()(l()(m()())))))(n(o(p(q(r(s()())())())())())()))"
  integer :: treeCharp

  nullify(root)
  treeCharp = 1
  call build (root, map, treeCharp)
  call preOrder(root)
  print *, depth(root)

contains

  recursive subroutine build(node, map, finger)
    type(tree), pointer :: node
    character(*), intent(in) :: map
    integer :: finger

    if (finger > len(map) .or. map(finger:finger) /= '(') then
       print *, "error"
       stop 10
    end if
    finger = finger + 1
    checkNull: block
      if (map(finger:finger) == ')') exit checkNull
      allocate(node)
      node % datum = map(finger:finger)
      checkNoChildren: block
        finger = finger + 1
        if (map(finger:finger) == ')') exit checkNoChildren
        call build(node % left, map, finger)
        call build(node % right, map, finger)
        if (map(finger:finger) /= ')') then
           print *, "error 2"
           stop 20
        end if
      end block checkNoChildren
    end block checkNull
    finger = finger + 1
  end subroutine build

  recursive subroutine preOrder(node)
    type(tree), pointer :: node
    if (associated(node)) then
       block
          !type(tree), pointer :: nleft = node % left      !<------- not supported yet
          !type(tree), pointer :: nright = node % right    !<------- not supported yet
          type(tree), pointer :: nleft
          type(tree), pointer :: nright

          nleft => node % left
          nright => node % right

          print *, node%datum
          call preOrder(nleft)
          call preOrder(nright)
       end block
    end if
  end subroutine preOrder

  recursive integer function depth(node) result (d)
    type(tree), pointer :: node
    integer :: dLeft, dRight
    d = 0
    if (associated(node)) then
       block
         integer :: dLeft
         integer :: dRight

         dLeft  = depth(node%left)
         dRight = depth(node%right)

         d = max(dLeft, dRight) + 1
       end block
    end if
  end function depth

end program BInRecurProc
