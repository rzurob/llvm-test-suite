! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentPass002.f
! opt variations: -qnol -qdeferredlp -qreuse=none

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
!*  DESCRIPTION                : assignment: pass-obj specified, assignment to intrinsic type or to derived type
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
      integer(k1)   :: i = -999
      contains
         procedure, pass(b) :: int_type
         procedure, pass(a) :: type_int
         generic :: assignment(=) => type_int, int_type
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j = -999
      contains
         procedure, pass(b) :: int_type => int_child
         procedure, pass(a) :: type_int => child_int
   end type

   contains

   subroutine int_type ( a, b )
      integer, intent(out)    :: a
      class(base(*,4)), intent(in) :: b

      a = b%i

      print *,'int_type'

   end subroutine

   subroutine int_child ( a, b )
      integer, intent(out)    :: a
      class(child(*,4)), intent(in) :: b

      a = b%i + b%j

      print *,'int_child'

   end subroutine

   subroutine type_int ( a, b )
      class(base(*,4)), intent(out) :: a
      integer, intent(in)     :: b

      a%i = b

      print *,'type_int'

   end subroutine

   subroutine child_int ( a, b )
      class(child(*,4)), intent(out) :: a
      integer, intent(in)     :: b

      a%i = b
      a%j = b

      print *,'child_int'

   end subroutine

end module

program genericAssignmentPass002
   use m

   class(base(20,4)), allocatable :: b1
   class(child(20,4)), pointer :: c1
   integer :: i

   allocate ( b1, c1 )

   b1 = 100
   print *, b1%i
   c1 = 1000
   print *, c1%i, c1%j

   i = b1
   print *,i

   i = c1
   print *,i

   deallocate ( b1 )
   allocate ( b1, source = c1 )

   b1 = 5000
   select type ( b1 )
      type is ( child(*,4) )
         print *, b1%i, b1%j
   end select

   i = b1
   print *,i

end program
