! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentParameter001.f
! opt variations: -qnock -qnodeferredlp

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
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: named-constant (parameter) should still invoke the generic tb procedures
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

   type base(k1,n1)    ! (1,3)
      integer, kind             :: k1
      integer, len              :: n1
      character(kind=k1,len=n1) :: c = 'xxx'
      contains
         procedure :: ab
         generic :: assignment(=) => ab
   end type


   type, extends( base ) :: child    ! (1,3)
      contains
         procedure :: ab => c
         generic :: assignment(=) => ab !<- specifying the same binding name twice
   end type

   contains

   subroutine ab ( a, b )
      class(base(1,*)), intent(out) :: a
      class(base(1,*)), intent(in) :: b
      a%c = b%c
   end subroutine

   subroutine c ( a, b )
      class(child(1,*)), intent(out) :: a
      class(base(1,*)), intent(in) :: b
      a%c = b%c(3:3) // b%c(2:2) // b%c(1:1)
   end subroutine


end module

program genericAssignmentParameter001
   use m

   type(base(1,3)) :: b1 = base(1,3)('IBM')
   type(base(1,3)), parameter  :: b2 = base(1,3)('FTN')
   type(child(1,3)) :: c1 = child(1,3)('xlf')
   type(child(1,3)), parameter :: c2 = child(1,3)('ghi')

   class(base(1,:)), allocatable :: b3
   allocate ( base(1,3):: b3 )

   b1 = b2
   print *, b1

   c1 = b2
   print *, c1

   c1 = c2
   print *, c1

   b3 = c2
   print *, b3%c

   deallocate ( b3 )

   allocate ( child(1,3) :: b3 )
   b3 = c2

   print *, b3%c

end program
