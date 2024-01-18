!*  ===================================================================
!*
!*  DATE                       : 06/06/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : subobject, VOLATILE
!*
!*  DESCRIPTION                : functional TC
!*
!*     5.1.2.16
!*        usually a pointer should have the VOLATILE attribute if its
!*     target has the VOLATILE attribute, all members of an EQUIVALENCE
!*     should have the VOLATILE attribute if one member has
!*     the VOLATILE attribute
!* ===================================================================

  module m
    contains

      subroutine sub1(parray1, tarray1)
        class(*), target :: tarray1(5,9)
        class(*), pointer :: parray1(:,:)
        VOLATILE:: tarray1

        parray1(-3:, -3:) => tarray1
!        parray1 => tarray1
      end subroutine sub1

      subroutine sub2(parray2, tarray2)
        class(*), target :: tarray2(5,9)
        class(*), pointer :: parray2(:,:)
        VOLATILE:: parray2

        parray2(-3:, -3:) => tarray2
!        parray2 => tarray2
      end subroutine sub2
  end module m

  program volatileAttrDataPointerAssign

    use m

    integer, target  :: tArray(5,9)
    class(*),pointer :: pArray(:,:)

    call sub1(pArray, tArray)

    call sub2(pArray, tArray)

  end program volatileAttrDataPointerAssign

