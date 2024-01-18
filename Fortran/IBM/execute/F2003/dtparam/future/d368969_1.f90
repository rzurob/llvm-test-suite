!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-10-01
!*
!*  DESCRIPTION                : tracking defect 368969. Case 1: ice in BE.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module dtpIAABounds006mod

  implicit none
  type dka(k)
     integer, kind :: k
     integer(k), allocatable :: ivar
   contains
     final :: f1, f4
  end type dka

  type dla(l)
     integer, len :: l
     character(l) :: label = ''
     integer, allocatable :: iarr(:)
   contains
     final :: fin
  end type dla

  type acontainer(k,l)
     integer, kind :: k
     integer, len  :: l
     type(dka(k)) :: dkvar
     type(dla(l)) :: dlvar
  end type acontainer

contains

  subroutine f1(a)
    type(dka(1)) :: a
    if (allocated(a%ivar)) then
       deallocate(a%ivar)
    end if
  end subroutine f1

  subroutine f4(a)
    type(dka(4)) :: a
    if (allocated(a%ivar)) then
       deallocate(a%ivar)
    end if
  end subroutine f4

  subroutine fin(a)
    type(dla(*)) :: a
    if (allocated(a%iarr)) then
       deallocate(a%iarr)
    end if
  end subroutine fin

end module dtpIAABounds006mod



program dtpIAABounds006

  use dtpIAABounds006mod
  implicit none

  type(acontainer(1,2)) :: o1a(3)

  integer :: i

  o1a = [(acontainer(1,2)(dka(1)(100+i), dla(2)(repeat(achar(99+i),2),[50+i,500+i])), i=1,size(o1a))]

  end
