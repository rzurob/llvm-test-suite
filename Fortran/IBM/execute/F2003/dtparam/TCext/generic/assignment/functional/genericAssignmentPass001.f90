! GB DTP extension using:
! ftcx_dtp -qk -qdeferredlp -qreuse=none /tstdev/F2003/generic/assignment/functional/genericAssignmentPass001.f
! opt variations: -qck -qnok -qnodeferredlp -qreuse=base

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
!*  DESCRIPTION                : assignment: pass-obj specified
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

   type base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      character(n1) :: c = 'xxx'
      contains
         procedure, pass(b) :: ab
         generic :: assignment(=) => ab
   end type

   type, extends(base) :: child(k2,n2)    ! (4,3,4,3)
      integer, kind :: k2
      integer, len  :: n2
      character(n2) :: d = 'xxx'
      contains
         procedure, pass(b) :: ab => cb
         generic :: assignment(=) => ab
   end type

   contains

   subroutine ab ( a, b )
      character(*), intent(out) :: a
      class(base(4,*)), intent(in)   :: b

      a = b%c

      print *,'ab'

   end subroutine

   subroutine cb ( a, b )
      character(*), intent(out)  :: a
      class(child(4,*,4,*)), intent(in)   :: b

      a(1:3) = b%c
      a(4:6) = b%d

      print *,'cb'

   end subroutine

end module

program genericAssignmentPass001
   use m

   type(base(4,3)) :: b1
   class(base(4,:)), allocatable :: b2
   type(child(4,3,4,3)) :: c1

   character(3) :: c
   character(6) :: d

   allocate ( b2, source = base(4,3)('ftn') )

   b1 = b2 !<- should not call generic assignment
   print *, b1

   c = b1
   print *, c

   b2%c = 'ibm'

   c = b2
   print *, c

   c1 = child(4,3,4,3) ('abc', 'def') !<- should not call generic assignment
   print *, c1

   d = c1
   print *,d
   
   deallocate ( b2 )
   allocate ( b2, source = child(4,3,4,3) ( 'ABC', 'DEF') )
   
   d = b2
   print *,d
   
   b2%c = b1
   select type ( b2 )
      type is ( child(4,*,4,*) )
         print *, b2%c, b2%d
   end select
   

end program
