! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : pure4.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/06/2006
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
!*  DESCRIPTION                : FROM and TO are of deferred char 
!*                               FROM and TO are dummy args but they
!*                               are the same actual arg
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
    contains
        pure subroutine sub(arg)
            character(:), allocatable, intent(inout) ::arg(:)
            call move_alloc(arg, arg)
        end subroutine
end module
     
        use m
        character(:), allocatable :: ch(:)

        allocate(character(20) :: ch(10))
        call sub(ch)

        if ( allocated(ch) ) stop 21

        end
