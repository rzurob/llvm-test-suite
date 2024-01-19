!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Use Association, VOLATILE
!*  DESCRIPTION                :
!*        11.2.1 The accessed entities have the attributes specified in
!*   the module, except that an entity may have a different accessibility
!*   attribute or it may have the Asy. or VOLATILE attribute in the local
!*   scoping unit even if the associated module entity does not...
!* ===================================================================

  module m
     class(*),pointer :: x
     private x                      ! this entity is declared as private
  end module

  program volatileUseAssociation02d
     use m
     VOLATILE x
  end program volatileUseAssociation02d

