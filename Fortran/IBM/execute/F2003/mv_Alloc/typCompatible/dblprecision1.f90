! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM is of type dbl precision
!*                               TO is of unlimited poly
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module m
          interface foo
              class(*) function  func(arg,brg)
                 allocatable func(:)
                 double precision, allocatable :: arg(:)
                 class(*), allocatable :: brg(:)
                 intent(inout) :: arg, brg
              end function
          end interface
      end module

      use m

      double precision, allocatable :: d1(:)
      double precision t,q
      class(*), allocatable :: d2(:)
      logical precision_R8

      allocate(double precision :: d2(20))
      allocate(d1(1), source = (/ real(3, 8) /) )

      select type( x => foo(d1,d2)  )
          type is (double precision)
             if ( .not. precision_R8(x(1), dble(3))) stop 21
          class default
               stop 99
      end select

      end

      class(*) function  func(arg,brg)
          allocatable func(:)
          double precision, allocatable :: arg(:)
          class(*), allocatable :: brg(:)
          intent(inout) :: arg,brg

          call move_alloc(arg, brg)

          if ( allocated(arg) ) stop 32
	  if ( .not. allocated(brg) ) stop 34

          allocate(func(1), source = brg )
      end function
