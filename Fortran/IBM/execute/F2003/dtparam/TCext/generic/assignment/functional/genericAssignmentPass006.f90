! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp /tstdev/F2003/generic/assignment/functional/genericAssignmentPass006.f
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
!*  DESCRIPTION                : assignment: pass-obj specified polymorphic assignment to different derived types
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

   type base(k1,k2,n1)    ! (4,1,3)
      integer, kind             :: k1,k2
      integer, len              :: n1
      integer(k1)               :: i = -999
      character(kind=k2,len=n1) :: c = 'xxx'
      contains
         procedure, pass(b) :: U_base
         generic :: assignment(=) => U_base
   end type

   contains

   subroutine U_base ( a, b )
      class(*), intent(out) :: a
      class(base(4,1,*)), intent(in)   :: b

      select type ( a )
         type is ( integer )
            a = b%i
         type is ( character(*) )
            a(1:3) = b%c
         type is ( real )
            a = real ( b%i )
         type is ( base(4,1,*) )
            a%i = b%i
            a%c = b%c
      end select

      print *,'U_base'

   end subroutine

end module

program genericAssignmentPass006
   use m

   class(base(4,1,:)), allocatable :: b
   class(*), pointer :: u
   integer :: i
   real :: r
   character(3) :: c

   allocate ( b, source = base(4,1,3)( 100, 'IBM' ) )
   i = b
   r = b
   c = b

   print *, i, r, c

   allocate ( integer :: u )
   u = b
   select type ( u )
      type is ( integer )
         print *, u
   end select

   allocate ( real :: u )
   u = b
   select type ( u )
      type is ( real )
         print *, u
   end select

   allocate ( character(3) :: u )
   u = b
   select type ( u )
      type is ( character(*) )
         print *, u
   end select

end program
