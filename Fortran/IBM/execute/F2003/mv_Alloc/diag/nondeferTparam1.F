! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : nondeferTparam1.f 
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
!*  DESCRIPTION                : FROM is dummy arg of type character(:) with
!*                                  deferred type parameter arg2*(:)
!*                               TO is dummy arg of type character(:) with
!*                                  assumed type parameter arg1*(*) 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

character(5), allocatable :: a1
character(:), allocatable :: a2

allocate(a1, source='hello')
allocate(a2, source='hellowor')

call sub(a1, a2)

contains
    subroutine sub(arg1, arg2)
        character(:), allocatable :: arg1*(*), arg2*(12)

        print *, 'arg1=',arg1, ' arg2=',arg2
        call move_alloc(arg2, arg1)
        print *, arg1

    end subroutine 

 end
