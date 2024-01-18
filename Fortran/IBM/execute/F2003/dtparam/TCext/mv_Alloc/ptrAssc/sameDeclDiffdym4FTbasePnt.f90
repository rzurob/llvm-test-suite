! GB DTP extension using:
! ftcx_dtp -qck -ql -qdeferredlp /tstdev/F2003/mv_Alloc/ptrAssc/sameDeclDiffdym4FTbasePnt.f
! opt variations: -qnock -qnol -qnodeferredlp

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

      type  :: base(n1,k1)    ! (20,4)
          integer, kind :: k1
          integer, len  :: n1
          integer(k1)      id
          contains
              procedure :: get_alloc  => func
      end type 

      type, extends(base) :: child    ! (20,4)
          character(:), allocatable :: ch
      end type 

      class(base(:,4)), pointer :: p

      contains 

         class(child(:,4)) function func(arg,brg)
            class(base(*,4)) :: arg
            type(child(*,4)) :: brg 
            allocatable func, brg
            target func, brg

            p => brg 
            call move_alloc(brg, func) 
	    
            if ( .not. allocated(func) ) stop 11
            if ( .not. associated(p,func) ) stop 13
         end function

end module

      use m

      class(base(:,4)), allocatable :: b
      type(child(20,4)), allocatable :: d

      allocate(b, source=( base(20,4)(6) ) )
      allocate(d, source=( child(20,4)(8, 'XYZ') ) )

      select type ( x => b%get_alloc(d) )
          type is ( child(*,4) )
              if ( x%id /= 8 ) stop 21
              if ( x%ch /= 'XYZ' ) stop 23 
          class default
              stop 25
      end select          

      if ( allocated(d) ) stop 31 

      end
