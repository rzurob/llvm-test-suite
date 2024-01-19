!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : subobject, VOLATILE
!*
!*  DESCRIPTION                : functional TC
!*
!*     5.1.2.16
!*        usually a pointer should have the VOLATILE attribute if its
!*     target has teh VOLATILE attribute, all members of an EQUIVALENCE
!*     should have the VOLATILE attribute if one member has
!*     the VOLATILE attribute
!* ===================================================================

  program volatileAttrEquivalence

    double precision a(3)
    real b(5)
    integer c
    complex d

    VOLATILE:: a

    equivalence(a, b(3))

    equivalence(b(3), c, d)

  end program volatileAttrEquivalence
