! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ulmtToFuncPolyFmPnt.f 
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
!*  DESCRIPTION                : FROM is poly, dummy arg, type child
!*                               TO is class(*), external func name 
!*                               pointer is poly, dummy arg, type base 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base
      end type 

      type, extends( base) :: child
          character(:), allocatable :: ch*8
      end type 

      interface 
         class(*) function func(arg,brg)
            import base, child 
            class(child) :: arg 
            class(base) :: brg
            allocatable :: func, arg 
         end function
      end interface


end module

      use m

      class(base), pointer :: p 
      class(child), allocatable :: d

      allocate(p, source= base()  )
      allocate(d, source= (child('XYZabcde123')) )

      select type ( x => func(d, p) )
          type is ( child )
              if ( x%ch /= 'XYZabcde' ) stop 23 
          class default
              stop 25
      end select          

      if ( allocated(d) ) stop 27

      end

         class(*) function func(arg,brg)
            use m, only : base, child
            class(child), intent(inout) :: arg 
            class(base) :: brg
            allocatable arg, func 
            pointer brg 
            target arg, func

            brg => arg

            call move_alloc(arg, func) 

            if (  .not. allocated(func) ) stop 11
  
            select type ( func)
                type is (child)
                    if (.not. associated(brg, func) ) stop 21
                class default
                    stop 27
            end select

         end function

