! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : sameDeclDiffdym4FTbasePnt.f 
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
!*  DESCRIPTION                : FROM is non-poly, dummy arg, type child
!*                               TO is name of type-bound proc, poly, type child
!*                               pointer is poly of type base 
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

      class(base), pointer :: p

      contains 

         class(child) function func(arg,brg)
            class(base) :: arg
            type(child) :: brg 
            allocatable func, brg
            target func, brg

            p => brg 
            call move_alloc(brg, func) 
	    
            if ( .not. allocated(func) ) stop 11
            if ( .not. associated(p,func) ) stop 13
         end function

end module

      use m

      class(base), allocatable :: b
      type(child), allocatable :: d

      allocate(b, source=( base(6) ) )
      allocate(d, source=( child(8, 'XYZ') ) )

      select type ( x => b%get_alloc(d) )
          type is ( child )
              if ( x%id /= 8 ) stop 21
              if ( x%ch /= 'XYZ' ) stop 23 
          class default
              stop 25
      end select          

      if ( allocated(d) ) stop 31 

      end
