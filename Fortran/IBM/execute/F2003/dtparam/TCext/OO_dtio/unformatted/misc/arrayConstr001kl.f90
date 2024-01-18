!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : arrayConstr001kl
!*
!*  PROGRAMMER                 : David Forster (derived from arrayConstr001 by Robert Ma)
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: array constructor 
!*                                        problem found in defect 298826
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program arrayConstr001kl

   type :: mydata (kmydata_1) ! kmydata_1=4
      integer, kind :: kmydata_1
      integer(kmydata_1) ::  i
   end type

   type :: base (kbase_1,lbase_1,lbase_2) ! kbase_1,lbase_1,lbase_2=4,3,2
      integer, kind :: kbase_1
      integer, len :: lbase_1,lbase_2
      type(mydata(kbase_1)) :: b(lbase_2) ! tcx: (kbase_1)
      character(lbase_1) :: c
   end type

   type(base(4,3,2)) :: b1 ! tcx: (4,3,2)
   type(mydata(4)), allocatable :: d1(:) ! tcx: (4)

   allocate ( d1(2), source = (/ mydata(4)(1), mydata(4)(2) /) ) ! tcx: (4) ! tcx: (4)
   b1 = base(4,3,2)( b = d1, c = 'ibm' ) ! tcx: (4,3,2)

   if ( ( b1%b(1)%i /= 1 )  .or. ( b1%b(2)%i /= 2 ) .or. ( b1%c /= 'ibm' ) ) error stop 2_4
end


! Extensions to introduce derived type parameters:
! type: mydata - added parameters (kmydata_1) to invoke with (4) / declare with (4) - 4 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,3,2) / declare with (4,*,2) - 2 changes
