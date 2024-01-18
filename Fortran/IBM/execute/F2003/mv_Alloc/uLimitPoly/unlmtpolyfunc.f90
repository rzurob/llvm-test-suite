! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type unlimited poly
!*                               TO is function name
!*                               FROM/TO is component of a DT
!*                               move_alloc appears in function
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base
          class(*), allocatable :: i1
      end type

      type, extends(base) :: child
          class(base), allocatable :: i2
      end type


      contains
         function func(arg)
            class(base), pointer :: arg
            class(*), allocatable :: func
            integer*8  int8

            int8 = 11
            allocate(arg, source = child ( real(2.1, 4), base(int8) ) )

            select type (arg)
                type is (child)
                    call move_alloc(arg%i2%i1, arg%i1)

                    if ( .not. allocated(arg%i1) ) stop 5
                    if ( allocated(arg%i2%i1) ) stop 6

                    call move_alloc(arg%i1, func)

                    if ( allocated(arg%i1) ) stop 7
                    if ( .not. allocated(func) ) stop 8
		class default
		    stop 9
            end select

         end function

end module

      use m

      class(base), pointer :: a

      select type (x => func(a))
          type is (integer*8)
              if ( x /= 11 ) stop 21
          class default
              stop 23
      end select

      end
