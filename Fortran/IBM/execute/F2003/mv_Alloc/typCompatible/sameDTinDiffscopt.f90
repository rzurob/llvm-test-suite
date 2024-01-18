! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : sameDTindiffscopt.f 
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
!*  DESCRIPTION                : FROM/TO are of a derived-type 
!*                               In different scopting unit, two DT with
!*                               same derived-type definiation
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


      module m

          type t
             sequence
             integer i
          end type

          type(t), allocatable :: a1

          contains
               subroutine sub

                  type t
                     sequence
                     integer i 
                  end type
               
                  type(t), allocatable :: a2

                  allocate(a1, source = t(10))
                  call move_alloc(a1, a2)
                  if ( .not. allocated(a2) ) stop 21 
                  if ( allocated(a1) ) stop 31
                  if ( a2%i /= 10 ) stop 41
               end subroutine
      end module

      use m
         call sub()
      end
