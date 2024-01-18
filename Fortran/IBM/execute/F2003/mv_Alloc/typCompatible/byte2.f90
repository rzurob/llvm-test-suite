! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : byte2.f 
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
!*  DESCRIPTION                : FROM is of type byte 
!*                               TO is of unlimit poly
!*                               move_alloc is called in module procedure            
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
                  class(*), allocatable :: a
                  call sub1(a)
               end subroutine

               subroutine sub1(c)
                  class(*), allocatable :: c
                  allocate(g)
                  g = 127
                  call move_alloc(g, c)
               end subroutine
      end module

      program main

      use m

      class(*), allocatable :: b

      call sub(b)

      if ( allocated(g) ) stop 11
      if ( .not. allocated(b) ) stop 21 
      
      select type (b)
          type is (byte)
              if ( b /= 127 ) stop 31
          class default
               stop 41
       end select

      end
