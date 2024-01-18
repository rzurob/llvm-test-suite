! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/generic/assignment/functional/genericAssignmentRecursive001.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

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
!*  DESCRIPTION                : assignment: subroutine being recursive and assigning linked lists
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
      integer, kind             :: k1
      integer, len              :: n1
      integer(k1)               :: i
      type(base(:,k1)), pointer :: next => null()
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

   recursive subroutine bassgn ( a, b )
      class(base(*,4)), intent(out) :: a
      type(base(*,4)), intent(in)   :: b

      a%i = b%i

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         allocate (base(20,4) :: a%next )
         a%next = b%next
      end if

   end subroutine

end module


program genericAssignmentRecursive001
   use m

   type(base(20,4)), target :: b1
   type(base(20,4)), target :: b2

   type(base(:,4)), pointer :: tmp => null()

   b2 = base(20,4)( 100, null() )
   tmp => b2

   do i = 1, 9
      allocate ( tmp%next, source = base(20,4)((i+1)*100, null() ) )
      tmp => tmp%next
   end do

   b1 = b2

   tmp => b1
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do


   tmp => b1

   do i = 1, 9
      allocate ( tmp%next, source = base(20,4)((i+1)*100, null() ) )
      tmp => tmp%next
   end do

   b1 = base(20,4)(-999, null() )
   b2 = b1

   tmp => b2
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   tmp => b1
   do i = 2, 100
      allocate ( tmp%next, source = base(20,4)( i*100, null() ) )
      tmp => tmp%next
   end do

   b2 = b1
   tmp => b2
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

end program
