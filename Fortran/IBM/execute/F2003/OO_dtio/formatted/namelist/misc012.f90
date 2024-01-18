!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 21/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: unlimited polymorphic component assoicated with character type
!*                                        inside select type and associate construct (defect 300875)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program misc012
   type base
      class(*), allocatable :: u
   end type

   type(base) :: b

   allocate(b%u, source = 'abc')

   select type ( g => b%u )
      type is (character(*))
         associate( d => g )
            if ( d /= 'abc' )  error stop 1_4
            if ( len(d) /= 3 ) error stop 2_4
         end associate
   end select

end program
