! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/assignment/functional/genericAssignmentScalar014.f
! opt variations: -qck -qnok -qnol -qnodeferredlp -qreuse=base

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
!*  DESCRIPTION                : assignment: with some class hierarchy
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
         procedure, private :: bcassgn
         generic :: assignment(=) => bcassgn
   end type

   type, extends(base) :: child(n2,k2)    ! (4,3,20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: i= -999
      contains
         procedure, private :: bcassgn => cassgn
   end type

   type, extends(child) :: gen3(n3,k3)    ! (4,3,20,4,20,4)
      integer, kind :: k3
      integer, len  :: n3
      integer(k3)   :: j = -999
      contains
         procedure, private :: gassgn
         generic :: assignment(=) => gassgn
   end type

   contains

      subroutine bcassgn ( a, b )
         class(base(4,*)), intent(out) :: a
         class(base(4,*)), intent(in) :: b

         a%c = b%c
         
         print *, 'bassgn'

      end subroutine
      
      subroutine cassgn ( a, b )
         class(child(4,*,*,4)), intent(out) :: a
         class(base(4,*)), intent(in) :: b

         a%c = b%c
         
         select type ( b )
            class is ( child(4,*,*,4) )
               a%i = b%i
            class is ( gen3(4,*,*,4,*,4) )
               a%i = b%i
               select type ( a )
                  type is ( gen3(4,*,*,4,*,4) )
                     a%j = b%j
               end select
         end select
         
         print *, 'cassgn'

      end subroutine
      
      subroutine gassgn ( a, b )
         class(gen3(4,*,*,4,*,4)), intent(out) :: a
         integer(4), intent(in)   :: b

         a = gen3(4,3,20,4,20,4)('xxx',b,b)
         
         print *, 'gassgn'

      end subroutine

end module

program genericAssignmentScalar014
   use m
   
   type(base(4,3)) :: b1
   class(base(4,:)), allocatable :: b2
   
   type(child(4,3,20,4)) :: c1
   class(child(4,:,:,4)), pointer :: c2
   
   type(gen3(4,3,20,4,20,4)) :: g1
   
   
   b1 = base(4,3)('abc')
   allocate ( base(4,3) :: b2 )
   allocate ( child(4,3,20,4) :: c2 )
   c1 = child(4,3,20,4)('ABC',100)
   g1 = gen3(4,3,20,4,20,4)('def', 200, 300) 
   
   b2 = b1
   c2 = c1
   
   if ( b2%c /= 'abc' ) error stop 1_4
   if ( ( c2%c /= 'ABC' ) .or. ( c2%i /= 100 ) ) error stop 2_4
   
   g1 = b1
   
   if ( g1%c /= 'abc' ) error stop 3_4
   
   g1 = c1
   
   if ( ( g1%c /= 'ABC' ) .or. ( g1%i /= 100 ) ) error stop 4_4
   
   deallocate ( b2, c2 )
   
   allocate ( child(4,3,20,4) :: b2 )
   
   b2 = c1
   
   allocate ( gen3(4,3,20,4,20,4) :: c2 )
   
   c2 = g1
   
   select type ( b2 )
      type is ( child(4,*,*,4) ) 
         if ( ( b2%c /= 'ABC' ) .or. ( b2%i /= 100 ) ) error stop 5_4
   end select
   
   select type ( c2 )
      type is ( gen3(4,*,*,4,*,4) ) 
         if ( ( c2%c /= 'ABC' ) .or. ( c2%i /= 100 ) .or. ( c2%j /= -999 ) ) error stop 6_4
         
         c2 = 2000
         if ( ( c2%c /= 'xxx' ) .or. ( c2%i /= 2000 ) .or. ( c2%j /= 2000) ) error stop 7_4
         
   end select
   
   g1 = 4000
   if ( ( g1%c /= 'xxx' ) .or. ( g1%i /= 4000 ) .or. ( g1%j /= 4000 ) ) error stop 8_4

end program
