! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : samepolydifftype.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM/TO are of poly type with same declared
!*                               type, different dynamic type
!*                               FROM is type bound proc name
!*                               TO is dummy arg of type bound proc 
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
      end type 

      type, extends(base) :: child
          character(:), allocatable :: ch
          contains
              procedure :: get_alloc  => func
      end type 

      contains 

         function func(arg,brg)
            class(child) :: arg
            class(base) :: func, brg
            allocatable func, brg

            allocate(func, source=arg)

            call move_alloc(func, brg) 

         end function

end module

      use m

      class(base), allocatable :: d
      class(child), allocatable :: b

      allocate(b, source=( child(8, 'XYZ') ) )
      allocate(d, source=( base(31) ) )

      print *, allocated( b%get_alloc(d) )

      select type (d)
          type is (child) 
              if ( d%id /= 8 ) stop 21
              if ( d%ch /= 'XYZ' ) stop 23 
          class default 
              stop 25 
      end select

      end
