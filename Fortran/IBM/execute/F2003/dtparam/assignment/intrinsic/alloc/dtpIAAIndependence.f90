!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : dtpIAAIndependence
!*
!*  DATE                       : 2009-06-01
!*
!*  PRIMARY FUNCTIONS TESTED   : Intrinsic Assignment with Allocation
!*
!*  SECONDARY FUNCTIONS TESTED : verify that LHS and RHS are independent in intrinsic assignment of allocatable objects
!*
!*  REFERENCE                  : Feature Number 365653
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Assign simple and complex expressions to an object which appears on both
!*  the LHS and RHS of an assignment (i.e., in both var and expr).  Use both
!*  scalars and arrays.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module dtpIAAIndependencemod

  implicit none
  type dka(k)
     integer, kind :: k = 4
     integer(k), allocatable :: ivar
  end type dka

  type dla(l)
     integer, len :: l = 0
     character(l) :: label
     integer, allocatable :: iarr(:)
  end type dla

  type acontainera(k,l)
     integer, kind :: k = 4
     integer, len  :: l = 0
     type(dka(k)), allocatable :: dkvar
     type(dka(k)), allocatable :: dkarr(:,:)
     type(dla(:)), allocatable :: dlvar
     type(dla(l)), allocatable :: dlarr(:,:,:)
  end type acontainera

end module dtpIAAIndependencemod


program dtpIAAIndependence

  use dtpIAAIndependencemod
  implicit none
  integer, parameter :: kExp  = 4
  integer, parameter :: lExpA = 3
  integer, parameter :: lExpB = 1

  type(dka(kExp)) :: vk4, vkArr(2,3:4)
  type(dla(lExpA)) :: vl3
  type(dla(lExpB)) :: vlArr(2,2,2)
  type(acontainera(kExp,lExpB)) :: a1
  type(acontainera(kExp,:)), allocatable :: a1a, a2a
  integer :: i, j, k

  vk4 = dka(kExp)(22)
  vkArr(1,:) = [dka(kExp)(122), dka(kExp)(222)]
  vkArr(2,:) = [dka(kExp)(321), dka(kExp)(432)]

  vl3 = dla(lExpA)("abc",[1,2,3,4])
  vlArr = reshape([(dla(lExpB)(achar(64+i),[(j*j,j=1,i)]), i=1,8)], [2,2,2])

  print *, "Creating a1"
  a1 = acontainera(kExp,lExpB)(vk4, vkArr, vl3, vlArr)

  print *, "vk4:"
  print *, "  k:", vk4%k
  print *, "  ivar kind:", kind(vk4%ivar)
  print *, "  ivar is allocated:", allocated(vk4%ivar)
  print *, "  ivar:", vk4%ivar

  call printAContainera("a1",a1)

  a1a = a1  ! create an allocatable object copying the above
  a2a = a1  ! make another copy, for comparison
  call printAContainera("a1a",a1a)
  call printAContainera("a2a",a2a)
  a1a = a1a ! assign it to itself, then compare
  call printAContainera("a1a again",a1a)

  ! compare kind, length, size, bounds, values in the printout
  a2a%dkvar = a2a%dkvar
  a2a%dlvar = a2a%dlvar
  a2a%dkarr = a2a%dkarr
  a2a%dlarr = a2a%dlarr
  call printAContainera("a2a again",a2a)

  a1a%dkvar%ivar = a1a%dkvar%ivar + 3
  a1a%dlvar%label = 'x' // a1a%dlvar%label
  a1a%dlvar%iarr = a1a%dlvar%iarr - 1
  do i=lbound(a1a%dkarr,1),ubound(a1a%dkarr,1)
     do j=lbound(a1a%dkarr,2),ubound(a1a%dkarr,2)
        a1a%dkarr(i,j)%ivar = a1a%dkarr(i,j)%ivar * 2
     end do
  end do

  do i=lbound(a1a%dlarr,1),ubound(a1a%dlarr,1)
     do j=lbound(a1a%dlarr,2),ubound(a1a%dlarr,2)
        do k=lbound(a1a%dlarr,3),ubound(a1a%dlarr,3)
           a1a%dlarr(i,j,k)%label = a1a%dlarr(i,j,k)%label // ''
           a1a%dlarr(i,j,k)%iarr = a1a%dlarr(i,j,k)%iarr / 2
        end do
     end do
  end do
  call printAContainera("a1a once again",a1a)

contains

  subroutine printAContainera(lab,a1)
    character(*) :: lab
    type(acontainera(kExp,*)) :: a1
    print *, lab, ":"
    print *, "  k, l, allocated:", a1%k, a1%l, allocated(a1%dkvar), allocated(a1%dkarr), allocated(a1%dlvar), allocated(a1%dlarr)
    print *, "  dkvar:", a1%dkvar%k, allocated(a1%dkvar%ivar), kind(a1%dkvar%ivar), a1%dkvar%ivar
    print *, "  dkarr:", a1%dkarr%k, lbound(a1%dkarr), ubound(a1%dkarr), "/", &
             ((allocated(a1%dkarr(i,j)%ivar), a1%dkarr(i,j)%ivar, &
               i=lbound(a1%dkarr,1),ubound(a1%dkarr,1)), &
               j=lbound(a1%dkarr,2),ubound(a1%dkarr,2))
    print *, "  dlvar:", a1%dlvar%l, len(a1%dlvar%label), a1%dlvar%label, &
             allocated(a1%dlvar%iarr), lbound(a1%dlvar%iarr), ubound(a1%dlvar%iarr), a1%dlvar%iarr
    print *, "  dlarr:", a1%dlarr%l, len(a1%dlarr%label), lbound(a1%dlarr), ubound(a1%dlarr), "/", &
         (((a1%dlarr(i,j,k)%label, allocated(a1%dlarr(i,j,k)%iarr), &
            "l:", lbound(a1%dlarr(i,j,k)%iarr), "u:", ubound(a1%dlarr(i,j,k)%iarr), a1%dlarr(i,j,k)%iarr, &
            i=lbound(a1%dlarr,1),ubound(a1%dlarr,1)), &
            j=lbound(a1%dlarr,2),ubound(a1%dlarr,2)), &
            k=lbound(a1%dlarr,3),ubound(a1%dlarr,3))
  end subroutine printAContainera

end program dtpIAAIndependence
