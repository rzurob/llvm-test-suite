! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of an nonpoly DT
!*                               TO is of unlmited poly
!*                               both are dummy args
!*                               defect 322393
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base
          integer*8 :: id
          character(:), allocatable :: name
      end type

      contains
          subroutine sub(a, b)
              type(base), allocatable :: a(:,:)
              class(*), allocatable  :: b(:,:)

              call move_alloc(a, b)
          end subroutine

end module

use m
      integer i
      type(base), allocatable :: a(:,:)
      class(*), allocatable :: b(:,:)
      allocate(a(5,2), source=reshape((/ (base(i, char(i+70)), i=1,10) /) , &
           (/5, 2/) ))
      call sub(a, b)

      if ( allocated(a) ) error stop 21
      if ( .not. allocated(b)) error stop 23
      select type(b)
          type is (base)
!              print *, b(1,1)%id, b(2,1)%id, b(3,1)%id, b(4,1)%id, b(5,1)%id
!              print *, b(1,1)%name, b(2,1)%name, b(3,1)%name, b(4,1)%name, b(5,1)%name
!              print *, b(1,2)%id, b(2,2)%id, b(3,2)%id, b(4,2)%id, b(5,2)%id
!              print *, b(1,2)%name, b(2,2)%name, b(3,2)%name, b(4,2)%name, b(5,2)%name
              print *,  (/ ( b(i,1)%id, i = 1,5) /)
              print *,  (/ ( b(i,1)%name, i = 1,5) /)
              print *,  (/ ( b(i,2)%id, i = 1,5) /)
              print *,  (/ ( b(i,2)%name, i = 1,5) /)
          class default
             stop 51
      end select

      end
