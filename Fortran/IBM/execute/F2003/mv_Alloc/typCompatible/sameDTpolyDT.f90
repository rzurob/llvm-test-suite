! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : sameDTpolyDT.f
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
!*  DESCRIPTION                : TO is of poly DT same as FROM 
!*                               FROM is a non-poly DT 
!*                               component type is same as type defined
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type A
          class(A), pointer :: self => null() 
      end type 
      
      class(A), allocatable, target ::  a1
      type(A), allocatable ::  a2
      
      allocate(a2, source = A(a1) )

      call move_alloc(a2, a1)

      if ( allocated(a2) ) stop 21
      if ( .not. allocated(a1) ) stop 23
      end
