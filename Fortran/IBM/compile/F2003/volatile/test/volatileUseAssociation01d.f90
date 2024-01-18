!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Use Association, VOLATILE
!*                             :
!*  DESCRIPTION                :
!*        11.2.1 The accessed entities have the attributes specified in 
!*   the module, except that an entity may have a different accessibility 
!*   attribute or it may have the Asy. or VOLATILE attribute in the local 
!*   scoping unit even if the associated module entity does not...
!* ===================================================================

  module m
     integer x
     real y 
     complex  z
     common /mixed/ x, y, z
  end module

  program volatileUseAssociation01d 
     use m
     VOLATILE :: mixed  
  end program volatileUseAssociation01d 

