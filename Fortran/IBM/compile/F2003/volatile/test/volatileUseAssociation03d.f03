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

    double precision a
    real b

    type dt
       sequence
       integer x
       real    y
    end type

    type(dt)::c(2,6,7)

    equivalence(a, b, c)

  end module

  program volatileUseAssociation03d
     use m
     VOLATILE  c
  end program volatileUseAssociation03d

