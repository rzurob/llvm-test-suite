! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of poly type with same declared
!*                               type, same dynamic type
!*                               FROM is dummy arg of type bound proc
!*                               TO is type bound proc name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base
          integer id
          contains
              procedure :: get_alloc  => func
      end type

      type, extends(base) :: child
          character(:), allocatable :: ch
      end type

      contains

         class(base) function func(arg,brg)
            class(base) :: arg, brg
            allocatable func, brg

            call move_alloc(brg, func)
         end function

end module

      use m

      class(base), allocatable :: b, d

      allocate(b, source=( base(6) ) )

      select type ( x => b%get_alloc(b) )
          type is ( base )
              if ( x%id /= 6 ) error stop 11
          class default
              stop 13
      end select

      allocate(d, source=( child(8, 'XYZ') ) )

      select type ( x => b%get_alloc(d) )
          type is ( child )
              if ( x%id /= 8 ) error stop 21
              if ( x%ch /= 'XYZ' ) error stop 23
          class default
              stop 25
      end select

      end
