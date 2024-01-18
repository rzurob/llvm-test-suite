! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ulmtPntFunc.f 
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
!*                               TO is poly, dummy arg, type base
!*                               pointer is class(*), module func name
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
      end type 

      contains 

         class(*) function func(arg,brg)
            type(child) :: arg 
            class(base) :: brg
            allocatable arg, brg
            pointer func
            target arg, brg

	    func => arg

            call move_alloc(arg, brg)

            if ( .not. allocated(brg) ) stop 11
            if ( allocated(arg) ) stop 13

            if (.not. associated (func, brg)) stop 23

         end function

end module

      use m

      class(base), allocatable :: b
      type(child), allocatable :: d

      allocate(b, source= (base(6))  )
      allocate(d, source= (child(8, 'XYZ')) )

      select type ( x => func(d, b) )
          type is ( child )
              if ( x%id /= 8 ) stop 21
              if ( x%ch /= 'XYZ' ) stop 23 
          class default
              stop 25
      end select          

      end
