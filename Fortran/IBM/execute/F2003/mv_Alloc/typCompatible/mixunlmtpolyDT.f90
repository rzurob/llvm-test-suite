! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of type unlimited poly
!*                               FROM is a non-poly DT
!*                               deferred pointer char as component
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
      type  :: base
          character(:), pointer :: ch(:)
      end type

      class(*), allocatable :: a
      type(base), allocatable :: b
end module

      use m
      character(:), pointer :: ch(:)

      allocate(ch(2), source = (/ 'XYZ', 'UVW' /) )

      allocate(base :: a)

      select type (a)
          type is (base)
               a = base(ch)
      end select

      allocate(b)
      allocate( b%ch(3), source = (/ '  IBM   ', 'compiler', ' Fortran' /) )

      call move_alloc(b, a)

      if ( allocated(b) ) stop 11
      if ( .not. allocated(a) ) stop 31

      select type (a)
          type is (base)
              print *, a%ch
          class default
              stop 23
      end select

      end
