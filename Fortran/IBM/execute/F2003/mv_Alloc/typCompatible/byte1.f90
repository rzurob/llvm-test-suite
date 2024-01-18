! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : byte.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/01/2006
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
!*  DESCRIPTION                : FROM/TO are of type byte 
!*                               move_alloc called in internal proc of
!*                               a module procedure
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m
          byte, allocatable   :: g
          contains
              subroutine sub(a)
                  byte, allocatable, intent(inout) :: a
                  call sub1(a)

                  contains
                       subroutine sub1(c)
                           byte, intent(inout), allocatable :: c
                           allocate(g)
                           g = 127
                           call move_alloc(g, c)

                       end subroutine
               end subroutine
      end module

      use m

      byte, allocatable :: b

      call sub(b)

      if ( allocated(g) ) stop 11
      if ( .not. allocated(b) ) stop 21 
      if ( b /= 127 ) stop 31

      end
