! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type real*4
!*                                TO is of type class(*)
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      real*4 x, z
      class(*) y
      allocatable :: x(:),y(:,:), z(:,:)
end module

      use m

      allocate(x(10), source=(/ ( real(aint(sqrt(i*1.0)),4), i=6,25,2) /) )
      allocate(z(2,5), source = reshape(x, (/2, 5/) ) )

      call sub(x)

      call move_alloc(z, y)

      if ( .not. allocated(y) ) error stop 11
      if ( allocated(z)) error stop 13

      select type(y)
            type is (real*4)
                if ( size(y,1) /= 2) error stop 21
                if ( size(y,2) /= 5) error stop 23
                 write (*, '(2f12.8)') y
             end select
      contains
          subroutine sub(x)
              real*4, allocatable  :: x(:)
              allocate(y(4,2), source = reshape(x(8:23:2), (/4,2/)))
          end subroutine

      end
