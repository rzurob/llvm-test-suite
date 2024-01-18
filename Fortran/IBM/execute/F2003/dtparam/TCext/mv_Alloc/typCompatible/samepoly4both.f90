! GB DTP extension using:
! ftcx_dtp -qck -qnol /tstdev/F2003/mv_Alloc/typCompatible/samepoly4both.f
! opt variations: -qnock -ql

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : samepoly4both.f 
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
!*                               type, same dynamic type 
!*                               FROM is dummy arg of type bound proc 
!*                               TO is type bound proc name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base(k1)    ! (4)
          integer, kind :: k1
          integer(k1)      id
          contains
              procedure :: get_alloc  => func
      end type 

      type, extends(base) :: child    ! (4)
          character(:), allocatable :: ch
      end type 

      contains 

         class(base(4)) function func(arg,brg)
            class(base(4)) :: arg, brg
            allocatable func, brg

            call move_alloc(brg, func) 
         end function

end module

      use m

      class(base(4)), allocatable :: b, d

      allocate(b, source=( base(4)(6) ) )

      select type ( x => b%get_alloc(b) )
          type is ( base(4) )
              if ( x%id /= 6 ) stop 11 
          class default
              stop 13 
      end select          

      allocate(d, source=( child(4)(8, 'XYZ') ) )

      select type ( x => b%get_alloc(d) )
          type is ( child(4) )
              if ( x%id /= 8 ) stop 21
              if ( x%ch /= 'XYZ' ) stop 23 
          class default
              stop 25
      end select          

      end
