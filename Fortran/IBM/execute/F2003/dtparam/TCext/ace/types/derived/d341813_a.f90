!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d341813_a
!*
!*  DATE                       : 2008-01-24
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: ICE: AugLoadArrayExpr: Unexpected
!*                               opcode: 50
!*
!*  DESCRIPTION                :
!*  ASTI has attempted to typecast (via OP_CONV=50) a DT_DERIVED_TYPE into
!*  a DT_AGGREGATE. This is not possible in the context, resulting in an ICE.
!*
!*  When the first argument to various intrinsics has a defered type parameter,
!*  the problem occurs.
!*
!*  NOTE:  The Reduced Code below is variation of this ICE, where an
!*         Array Constructor referencing an Array with a Defered Length
!*         Type Parameter causes the ICE.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetdt61l_dlpmod

  type dt(l1)    ! (20,4)
     integer, len  :: l1
  end type dt

end module acetdt61l_dlpmod

program acetdt61l_dlp
  use acetdt61l_dlpmod

  type(dt(20)), allocatable, target :: base(:)
  type(dt(:)), pointer :: matrix(:,:)

  allocate(base(4))
  matrix(1:2,1:2) => base ! rank remapping

  matrix(2,1:2) = [dt(20):: matrix(1,:)] ! <= Line 18

end program acetdt61l_dlp
