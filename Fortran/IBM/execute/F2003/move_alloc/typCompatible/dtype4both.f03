! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of a DT, an optional dummy arg
!*                               of a type bound proc
!*                               TO is of the same DT as FROM
!*                               TO is function return name
!*                               move_alloc called in a type bound proc
!*                               defect 321971
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type A
          character(:), allocatable :: ch
      end type

      type  :: base
          type(A), allocatable :: a1
          contains
              procedure :: get_alloc  => func
      end type

      contains
         type(base) function func(arg, brg)
            class(base) :: arg
            type(base), target :: brg
            optional :: brg
            allocatable func, brg

            if ( present(brg))  then
               if ( .not. allocated(brg))  allocate(brg, source = arg)
               call move_alloc(brg,func)
            end if

         end function

end module

      use m

      type(base), allocatable :: b

      type(base) d

      allocate(b, source=( base ( A('XYZ') ) ) )

      d =  b%get_alloc( b )

      if ( allocated(b)) error stop 11

      if ( d%a1%ch /= 'XYZ' ) error stop 21

      end