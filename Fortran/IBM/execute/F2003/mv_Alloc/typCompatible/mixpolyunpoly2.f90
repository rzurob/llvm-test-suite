! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of type unlimited poly& component of a DT
!*                               FROM is a dynamic type
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
end module

      use m

      class(base), allocatable :: a,b

      allocate(b, source = child( 'IBM-COMPILER', base(12) ) )
      allocate(a, source = b )

      select type ( b )
          type is ( child )
              call move_alloc(a, b%i2%i1)

              if ( allocated(a)) error stop 11
              if ( .not. allocated(b%i2%i1) ) error stop 13

              select type (   x => b%i2%i1 )
                  type is (child)
                      select type ( y => x%i1 )
                          type is (character(*))
                              if ( y /= 'IBM-COMPILER' ) error stop 21
                          class default
                              stop 23
                      end select

                      select type ( y => x%i2%i1 )
                          type is ( integer)
                              if ( y /= 12 ) error stop 31
                          class default
                              stop 33
                      end select
              end select
          class default
              stop 41
         end select
      end
