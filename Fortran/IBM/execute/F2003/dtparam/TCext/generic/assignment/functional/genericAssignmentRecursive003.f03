! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentRecursive003.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
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

   type inner(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: k
      contains
         generic, private :: assignment(=) => iassgn
         procedure, private :: iassgn

   end type

   type base(k2)    ! (4)
      integer, kind            :: k2
      type(inner(k2))          :: i
      class(base(k2)), pointer :: next => null()
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (4)
      type(inner(k2)) :: j
      contains
         procedure, pass :: bassgn => cassgn
    end type

   contains

   subroutine iassgn ( a, b )
      class(inner(4)), intent(out) :: a
      class(inner(4)), intent(in)   :: b

      print *, 'iassgn'
      a%k = b%k

   end subroutine

   recursive subroutine bassgn ( a, b )
      class(base(4)), intent(out) :: a
      class(base(4)), intent(in)   :: b

      print *, 'bassgn'
      a%i = b%i

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         select type ( g => b%next )
            type is ( base(4) )
               allocate ( a%next )
            type is ( child(4) )
               allocate ( child(4) :: a%next )
         end select

         a%next = b%next
      end if

   end subroutine

   recursive subroutine cassgn ( a, b )
      class(child(4)), intent(out) :: a
      class(base(4)), intent(in)   :: b

      print *, 'cassgn'
      a%i = b%i

      select type ( b )
         type is ( child(4) )
            a%j = b%j
      end select

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         select type ( g => b%next )
            type is ( base(4) )
               allocate ( a%next )
            type is ( child(4) )
               allocate ( child(4) :: a%next )
         end select
         a%next = b%next
      end if

   end subroutine

end module


program genericAssignmentRecursive003
   use m

   class(base(4)), pointer :: b1
   class(base(4)), pointer :: b2

   class(base(4)), pointer :: tmp => null()

   allocate ( b1 , source = base(4)( inner(4)(100), null() ) )
   allocate ( b1%next, source = child(4)( inner(4)(200), null(), inner(4)(2000) ) )
   allocate ( b1%next%next, source = base(4)( inner(4)(300), null() ) )
   allocate ( b1%next%next%next, source = child(4)( inner(4)(400), null(), inner(4)(4000) ) )
   allocate ( b1%next%next%next%next, source = base(4)( inner(4)(500), null() ) )
   allocate ( b1%next%next%next%next%next, source = child(4)( inner(4)(600), null(), inner(4)(6000) ) )

   print *,'start1'

   allocate ( b2 )
   b2 = b1
   tmp => b2
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base(4) )
            print *, tmp%i
         type is ( child(4) )
            print *, tmp%i, tmp%j
      end select

      tmp => tmp%next
   end do

   call mynullify ( b2 )

   print *, 'end1'

   allocate ( b2, source = child(4)( inner(4)(100), null(), inner(4)(1000) ) )
   allocate ( b2%next, source = child(4)( inner(4)(200), null(), inner(4)(2000) ) )
   allocate ( b2%next%next, source = child(4)( inner(4)(300), null(), inner(4)(3000) ) )
   allocate ( b2%next%next%next, source = child(4)( inner(4)(400), null(), inner(4)(4000) ) )
   allocate ( b2%next%next%next%next, source = base(4)( inner(4)(500), null() ) )
   allocate ( b2%next%next%next%next%next, source = base(4)( inner(4)(600), null() ) )

   print *, 'start2'

   call mynullify ( b1 )

   allocate ( child(4) :: b1 )
   b1 = b2

   tmp => b1
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base(4) )
            print *, tmp%i
         type is ( child(4) )
            print *, tmp%i, tmp%j
      end select

      tmp => tmp%next
   end do

   print *, 'end2'

   contains

      recursive subroutine mynullify ( a )
         class(base(4)), pointer, intent(inout) :: a

         if ( associated ( a%next ) ) call mynullify ( a%next )

         nullify(a)

      end subroutine

end program
