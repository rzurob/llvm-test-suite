!*  ===================================================================
!*
!*  DATE                       : 04/03/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Passing a char pointer to an internal
!*                              subroutine which dummy argument is char
!*				target. Test association between the two
!*				chars. Also, a function created within the
!*				subroutine which contains a char pointer.
!*				Test the assoication between the subroutine
!*				char target dummy arg, and the char pointer
!*				within the function.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   character(3), pointer :: pp

   contains

      subroutine tsub(a)

         character(3), value, target :: a
         if (associated(pp, a)) error stop 1_4
	 call tfunc()

         contains
	    subroutine tfunc()

               character(3), pointer :: p
               p => a
               if (.not. associated(p,a)) error stop 2_4
               if (associated(p,pp)) error stop 3_4

            end subroutine

      end subroutine
end module

program argAssocPtr002

   use m
   character(3), target :: targ

   targ = 'abc'

   pp => targ

   call tsub(pp)

 end

