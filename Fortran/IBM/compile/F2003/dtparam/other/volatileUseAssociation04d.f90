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

    type dtp(k, l)
       integer, kind :: k
       integer, len ::  l
       integer(k) :: arr(l)
    end type

    type(dtp(8,8)) ::a

  end module

  program volatileUseAssociation04d 
     use m
     VOLATILE a 
  end program volatileUseAssociation04d 

