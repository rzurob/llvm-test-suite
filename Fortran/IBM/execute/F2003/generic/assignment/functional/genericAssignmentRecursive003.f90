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
!*  DESCRIPTION                : assignment: subroutine being recursive and assigning polymorphic linked lists
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
      integer(4) :: k
      contains
         generic, private :: assignment(=) => iassgn
         procedure, private :: iassgn

   end type

   type base
      type(inner) :: i
      class(base), pointer :: next => null()
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      type(inner) :: j
      contains
         procedure, pass :: bassgn => cassgn
    end type

   contains

   subroutine iassgn ( a, b )
      class(inner), intent(out) :: a
      class(inner), intent(in)   :: b

      print *, 'iassgn'
      a%k = b%k

   end subroutine

   recursive subroutine bassgn ( a, b )
      class(base), intent(out) :: a
      class(base), intent(in)   :: b

      print *, 'bassgn'
      a%i = b%i

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         select type ( g => b%next )
            type is ( base )
               allocate ( a%next )
            type is ( child )
               allocate ( child :: a%next )
         end select

         a%next = b%next
      end if

   end subroutine

   recursive subroutine cassgn ( a, b )
      class(child), intent(out) :: a
      class(base), intent(in)   :: b

      print *, 'cassgn'
      a%i = b%i

      select type ( b )
         type is ( child )
            a%j = b%j
      end select

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         select type ( g => b%next )
            type is ( base )
               allocate ( a%next )
            type is ( child )
               allocate ( child :: a%next )
         end select
         a%next = b%next
      end if

   end subroutine

end module


program genericAssignmentRecursive003
   use m

   class(base), pointer :: b1
   class(base), pointer :: b2

   class(base), pointer :: tmp => null()

   allocate ( b1 , source = base( inner(100), null() ) )
   allocate ( b1%next, source = child( inner(200), null(), inner(2000) ) )
   allocate ( b1%next%next, source = base( inner(300), null() ) )
   allocate ( b1%next%next%next, source = child( inner(400), null(), inner(4000) ) )
   allocate ( b1%next%next%next%next, source = base( inner(500), null() ) )
   allocate ( b1%next%next%next%next%next, source = child( inner(600), null(), inner(6000) ) )

   print *,'start1'

   allocate ( b2 )
   b2 = b1
   tmp => b2
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base )
            print *, tmp%i
         type is ( child )
            print *, tmp%i, tmp%j
      end select

      tmp => tmp%next
   end do

   call mynullify ( b2 )

   print *, 'end1'

   allocate ( b2, source = child( inner(100), null(), inner(1000) ) )
   allocate ( b2%next, source = child( inner(200), null(), inner(2000) ) )
   allocate ( b2%next%next, source = child( inner(300), null(), inner(3000) ) )
   allocate ( b2%next%next%next, source = child( inner(400), null(), inner(4000) ) )
   allocate ( b2%next%next%next%next, source = base( inner(500), null() ) )
   allocate ( b2%next%next%next%next%next, source = base( inner(600), null() ) )

   print *, 'start2'

   call mynullify ( b1 )

   allocate ( child :: b1 )
   b1 = b2

   tmp => b1
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base )
            print *, tmp%i
         type is ( child )
            print *, tmp%i, tmp%j
      end select

      tmp => tmp%next
   end do

   print *, 'end2'

   contains

      recursive subroutine mynullify ( a )
         class(base), pointer, intent(inout) :: a

         if ( associated ( a%next ) ) call mynullify ( a%next )

         nullify(a)

      end subroutine

end program
