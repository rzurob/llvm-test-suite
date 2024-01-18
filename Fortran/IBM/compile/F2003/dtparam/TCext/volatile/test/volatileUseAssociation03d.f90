! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/volatile/test/volatileUseAssociation03d.f
! opt variations: -ql -qreuse=none

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

    type dt(k1)    ! (4)
       integer, kind :: k1
       sequence
       integer(k1)      x
       real(k1)         y
    end type

    type(dt(4))::c(2,6,7)

    equivalence(a, b, c)

  end module

  program volatileUseAssociation03d
     use m
     VOLATILE  c
  end program volatileUseAssociation03d

