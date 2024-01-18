!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType16d
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Assumed type object cannot appear 
!*                               as a selector of a SELECT TYPE statement 
!*          
!*     
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType16d
implicit none

contains 

   subroutine sub(a) 
    type(*) :: a 

    select type ( a )
      class default
        ERROR STOP 10
    end select
   end subroutine sub

end program AssumedType16d
