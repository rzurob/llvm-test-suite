! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qnodeferredlp -qreuse=self /tstdev/F2003/generic/assignment/functional/genericAssignmentArray014.f
! opt variations: -qck -qnok -ql -qdefaultpv -qdeferredlp -qreuse=none

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
!*  DESCRIPTION                : assignment: with scalar derived type component that has generic assignment
!*                                           non-polymorphic operands
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

   type inner(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i= 0
      contains
         generic, private :: assignment(=) => innerassgn
         procedure, pass, private :: innerassgn
   end type

   type base(k2,n1)    ! (4,3)
      integer, kind   :: k2
      integer, len    :: n1
      character(n1)   :: c ='xxx'
      type(inner(k2)) :: in
      contains
         generic :: assignment(=) => bassgn1d
         procedure, pass :: bassgn1d
   end type

   contains

      subroutine bassgn1d ( a, b )
         class(base(4,*)), intent(inout) :: a
         type(base(4,*)), intent(in) :: b(:)

         a%c = b(1)%c
         a%in = b%in

         print *, 'bassgn1d'

      end subroutine

      subroutine innerassgn ( a, b )
         class(inner(4)), intent(inout) :: a
         type(inner(4)), intent(in) :: b(:)

         do k = 1, size(b)
            a%i = a%i + b(k)%i
         end do

         print *, 'innerassgn'

      end subroutine

end module

program genericAssignmentArray014
   use m

   type(base(4,3)) :: b0, b1(5)
   type(base(4,3)), allocatable :: b2(:)
   type(base(4,3)), pointer :: b3(:)

   allocate ( b2(6), b3(3) )

   do i=1,5
      b1(i)%in%i = i
      b1(i)%c = 'ibm'
   end do

   do i=1,size(b2)
      b2(i)%in%i = i
      b2(i)%c = 'ftn'
   end do

   do i=1,size(b3)
      b3(i)%in%i = i
      b3(i)%c = 'XLF'
   end do

   b0 = b1
   print *, b0

   b0 = b2
   print *, b0

   b0 = b3
   print *, b0

end program
