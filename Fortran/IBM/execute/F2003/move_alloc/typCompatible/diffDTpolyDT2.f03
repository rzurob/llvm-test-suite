! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of an nonpoly child DT
!*                               TO is of an poly parent DT, dummy arg
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      type  :: base
          character(10) ch
      end type

      type, extends(base) :: child
      end type

      class(base), allocatable :: b
      type(child), allocatable :: a

      allocate(b, source=( base('helloworld') ) )
      allocate(a, source=( child('COMPILER') ) )

      call sub(b)

      if ( allocated(a) ) error stop 11
      if ( .not. allocated(b) ) error stop 12

      select type ( b )
          type is (child)
             if ( b%ch /= 'COMPILER')  ERROR STOP 21
          class default
             STOP 23
      end select

      contains
         subroutine sub(brg)
            class(base), optional :: brg
            allocatable  brg

            call move_alloc(a, brg)

         end subroutine
      end
