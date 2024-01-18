! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type unlimited poly
!*                               TO is function return name
!*                               FROM is an optional dummy arg
!*                               move_alloc appears in a type bound proc
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base
          integer*8, allocatable :: i1
          contains
              procedure :: get_alloc  => func
      end type

      contains
         class(*) function func(arg, brg)
            class(base) :: arg
            class(*), optional, allocatable :: brg
            allocatable func

            allocate(brg, source= arg )
            call move_alloc(brg,func)

            if ( .not. allocated(func)) stop 9
         end function

end module

      use m

      class(base), allocatable :: b
      class(*), allocatable :: c

      allocate(b, source=( base (103_8) ) )

      select type ( x => b%get_alloc( c ))
          type is (base)
             if ( x%i1 /= 103_8) STOP 21
          class default
             STOP 23
      end select

      if ( allocated(c) ) stop 31
      end
