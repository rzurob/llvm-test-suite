!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: with polymorphic derived type component that has generic assignment
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

   type inner
      integer :: i= 0
      contains
         generic, private :: assignment(=) => innerassgn
         procedure, pass, private :: innerassgn
   end type

   type, extends(inner) :: innerchild
      integer :: j = 0
   end type

   type base
      character(3) :: c ='xxx'
      class(inner), allocatable  :: in
      contains
         generic :: assignment(=) => bassgn1d
         procedure, pass :: bassgn1d
   end type

   contains

      subroutine bassgn1d ( a, b )
         class(base), intent(inout) :: a
         type(base), intent(in) :: b(:)

         a%c = b(1)%c
         if ( .not. allocated(a%in) ) allocate ( a%in, source = b(1)%in )

         a%in = b(1)%in

         print *, 'bassgn1d'

      end subroutine

      subroutine innerassgn ( a, b )
         class(inner), intent(inout) :: a
         class(inner), intent(in) :: b

         a%i = b%i

         select type ( a )
            type is ( innerchild )
               select type ( b )
                  type is ( innerchild )
                     a%j = b%j
               end select
         end select

         print *, 'innerassgn'

      end subroutine

end module

program genericAssignmentArray016
   use m

   type(base) :: b1
   type(base) :: b2(3)
   type(base), allocatable :: b3(:)

   b2 = (/ base ('aaa', inner(100) ), &
           base ('bbb', inner(200) ), &
           base ('ccc', inner(300) ) /)

   allocate ( b3(4), source = (/ base ('AAA', inner(1000) ), &
                                 base ('BBB', inner(2000) ), &
                                 base ('CCC', inner(3000) ), &
                                 base ('DDD', inner(4000) ) /)  )

   print *, 'start'

   b1 = b2
   print *, b1%c, b1%in%i

   b1 = b3
   print *, b1%c, b1%in%i

   print *, 'end'

   deallocate ( b3 )

   b2 = (/ base ('ddd', innerchild(10, 100) ), &
           base ('eee', innerchild(20, 200) ), &
           base ('fff', innerchild(30, 300) ) /)

   print *, 'start'

   allocate ( b3(4), source = (/ base ('EEE', innerchild(100,1000) ), &
                                 base ('FFF', innerchild(200,2000) ), &
                                 base ('GGG', innerchild(300,3000) ), &
                                 base ('HHH', innerchild(400,4000) ) /)  )

   deallocate ( b1%in )

   b1 = b2
   print *, b1%c
   select type ( g => b1%in )
      type is (innerchild)
         print *, g%i, g%j
   end select

   b1 = b3
   print *, b1%c
   select type ( g => b1%in )
      type is (innerchild)
         print *, g%i, g%j
   end select

   print *, 'end'

end program
