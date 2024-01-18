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

   type base
      character(3) :: c = 'xxx'
      contains
         procedure, private :: bcassgn
         generic :: assignment(=) => bcassgn
   end type

   type, extends(base) :: child
      integer :: i= -999
      contains
         procedure, private :: bcassgn => cassgn
   end type

   type, extends(child) :: gen3
      integer :: j = -999
      contains
         procedure, private :: gassgn
         generic :: assignment(=) => gassgn
   end type

   contains

      subroutine bcassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%c = b%c
         
         print *, 'bassgn'

      end subroutine
      
      subroutine cassgn ( a, b )
         class(child), intent(out) :: a
         class(base), intent(in) :: b

         a%c = b%c
         
         select type ( b )
            class is ( child )
               a%i = b%i
            class is ( gen3 )
               a%i = b%i
               select type ( a )
                  type is ( gen3 )
                     a%j = b%j
               end select
         end select
         
         print *, 'cassgn'

      end subroutine
      
      subroutine gassgn ( a, b )
         class(gen3), intent(out) :: a
         integer(4), intent(in)   :: b

         a = gen3('xxx',b,b)
         
         print *, 'gassgn'

      end subroutine

end module

program genericAssignmentScalar014
   use m
   
   type(base) :: b1
   class(base), allocatable :: b2
   
   type(child) :: c1
   class(child), pointer :: c2
   
   type(gen3) :: g1
   
   
   b1 = base('abc')
   allocate ( b2, c2 )
   c1 = child('ABC',100)
   g1 = gen3('def', 200, 300) 
   
   b2 = b1
   c2 = c1
   
   if ( b2%c /= 'abc' ) error stop 1_4
   if ( ( c2%c /= 'ABC' ) .or. ( c2%i /= 100 ) ) error stop 2_4
   
   g1 = b1
   
   if ( g1%c /= 'abc' ) error stop 3_4
   
   g1 = c1
   
   if ( ( g1%c /= 'ABC' ) .or. ( g1%i /= 100 ) ) error stop 4_4
   
   deallocate ( b2, c2 )
   
   allocate ( child :: b2 )
   
   b2 = c1
   
   allocate ( gen3 :: c2 )
   
   c2 = g1
   
   select type ( b2 )
      type is ( child ) 
         if ( ( b2%c /= 'ABC' ) .or. ( b2%i /= 100 ) ) error stop 5_4
   end select
   
   select type ( c2 )
      type is ( gen3 ) 
         if ( ( c2%c /= 'ABC' ) .or. ( c2%i /= 100 ) .or. ( c2%j /= -999 ) ) error stop 6_4
         
         c2 = 2000
         if ( ( c2%c /= 'xxx' ) .or. ( c2%i /= 2000 ) .or. ( c2%j /= 2000) ) error stop 7_4
         
   end select
   
   g1 = 4000
   if ( ( g1%c /= 'xxx' ) .or. ( g1%i /= 4000 ) .or. ( g1%j /= 4000 ) ) error stop 8_4

end program
