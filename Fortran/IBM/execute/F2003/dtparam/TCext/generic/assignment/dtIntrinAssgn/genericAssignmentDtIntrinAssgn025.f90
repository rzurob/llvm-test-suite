! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn025.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - for allocatable component
!*                                    - if it's an array,
!*                                      it's allocated with the same bound.
!*                                      try without generic tb
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
   end type

   type container(k2,n2)    ! (4,20)
      integer, kind                  :: k2
      integer, len                   :: n2
      type(base(:,k2)), allocatable  :: b1(:)
      type(child(:,k2)), allocatable :: c1(:)
   end type

end module

program genericAssignmentDtIntrinAssgn025
   use m

   type(container(4,20)) :: c1, c2, c3
   pointer :: c2
   allocatable :: c3

   allocate ( c2, c3 )
   c1 = container(4,20) ( (/ base(20,4)(1), base(20,4)(2), base(20,4)(3) /) , (/ child(20,4)(4,5), child(20,4)(6,7), child(20,4)(8,9) /) )
   c2 = c1
   c3 = c2

   print *, c1%b1%i, c1%c1%i, c1%c1%j
   print *, 'b1 bounds:', lbound(c1%b1), ubound(c1%b1),  'c1 bounds:', lbound(c1%c1), ubound(c1%c1)
   print *, c2%b1%i, c2%c1%i, c2%c1%j
   print *, 'b1 bounds:', lbound(c2%b1), ubound(c2%b1),  'c1 bounds:', lbound(c2%c1), ubound(c2%c1)
   print *, c3%b1%i, c3%c1%i, c3%c1%j
   print *, 'b1 bounds:', lbound(c3%b1), ubound(c3%b1),  'c1 bounds:', lbound(c3%c1), ubound(c3%c1)
   
   deallocate ( c2%b1, c2%c1 )
   
   allocate ( c2%b1(-1000:-998), source = (/ base(20,4)(11), base(20,4)(12), base(20,4)(13) /) )
   allocate ( c2%c1(10000:10002), source = (/ child(20,4)(14,15), child(20,4)(16,17), child(20,4)(18,19) /) )

   c3 = c2
   c1 = c3

   print *, c1%b1%i, c1%c1%i, c1%c1%j
   print *, 'b1 bounds:', lbound(c1%b1), ubound(c1%b1),  'c1 bounds:', lbound(c1%c1), ubound(c1%c1)
   print *, c2%b1%i, c2%c1%i, c2%c1%j
   print *, 'b1 bounds:', lbound(c2%b1), ubound(c2%b1),  'c1 bounds:', lbound(c2%c1), ubound(c2%c1)
   print *, c3%b1%i, c3%c1%i, c3%c1%j
   print *, 'b1 bounds:', lbound(c3%b1), ubound(c3%b1),  'c1 bounds:', lbound(c3%c1), ubound(c3%c1)

   deallocate ( c3%b1, c3%c1 )
   
   allocate (base(20,4) :: c3%b1(10:0) )          ! zero-sized
   allocate ( c3%c1(-1:1), source = (/ child(20,4)(14,15), child(20,4)(16,17), child(20,4)(18,19) /) )

   c1 = c3
   c2 = c1

   print *, c1%b1%i, c1%c1%i, c1%c1%j
   print *, 'b1 bounds:', lbound(c1%b1), ubound(c1%b1),  'c1 bounds:', lbound(c1%c1), ubound(c1%c1)
   print *, c2%b1%i, c2%c1%i, c2%c1%j
   print *, 'b1 bounds:', lbound(c2%b1), ubound(c2%b1),  'c1 bounds:', lbound(c2%c1), ubound(c2%c1)
   print *, c3%b1%i, c3%c1%i, c3%c1%j
   print *, 'b1 bounds:', lbound(c3%b1), ubound(c3%b1),  'c1 bounds:', lbound(c3%c1), ubound(c3%c1)

end program
