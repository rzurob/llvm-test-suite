! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentRecursive002.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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

   type base(n1,k1)    ! (20,4)
      integer, kind              :: k1
      integer, len               :: n1
      integer(k1)                :: i
      class(base(:,k1)), pointer :: next => null()
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure, pass :: bassgn => cassgn
    end type

   contains

   recursive subroutine bassgn ( a, b )
      class(base(*,4)), intent(out) :: a
      class(base(*,4)), intent(in)  :: b

      print *, 'bassgn'
      a%i = b%i

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         select type ( g => b%next )
            type is ( base(*,4) )
               allocate ( base(20,4) :: a%next )
            type is ( child(*,4) )
               allocate ( child(20,4) :: a%next )
         end select

         a%next = b%next
      end if

   end subroutine

   recursive subroutine cassgn ( a, b )
      class(child(*,4)), intent(out) :: a
      class(base(*,4)), intent(in)   :: b

      print *, 'cassgn'
      a%i = b%i

      select type ( b )
         type is ( child(*,4) )
            a%j = b%j
      end select

      if ( associated ( a%next ) ) nullify ( a%next )
      if ( associated ( b%next ) ) then
         select type ( g => b%next )
            type is ( base(*,4) )
               allocate (base(a%n1,4) :: a%next )
            type is ( child(*,4) )
               allocate ( child(20,4) :: a%next )
         end select
         a%next = b%next
      end if

   end subroutine

end module


program genericAssignmentRecursive002
   use m

   class(base(:,4)), pointer :: b1
   class(base(:,4)), pointer :: b2

   class(base(:,4)), pointer :: tmp => null()

   allocate ( b1, source = base(20,4)( 100, null() ) )
   allocate ( b1%next, source = child(20,4)( 200, null(), 2000 ) )
   allocate ( b1%next%next, source = base(20,4)( 300, null() ) )
   allocate ( b1%next%next%next, source = child(20,4)( 400, null(), 4000 ) )
   allocate ( b1%next%next%next%next, source = base(20,4)( 500, null() ) )
   allocate ( b1%next%next%next%next%next, source = child(20,4)( 600, null(), 6000 ) )

   allocate (base(20,4) :: b2 )
   b2 = b1
   tmp => b2
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base(*,4) )
            print *, tmp%i
         type is ( child(*,4) )
            print *, tmp%i, tmp%j
      end select

      tmp => tmp%next
   end do

   call mynullify ( b2 )
   
   allocate ( b2, source = child(20,4)( 100, null(), 1000 ) )
   allocate ( b2%next, source = child(20,4)( 200, null(), 2000 ) )
   allocate ( b2%next%next, source = child(20,4)( 300, null(), 3000 ) )
   allocate ( b2%next%next%next, source = child(20,4)( 400, null(), 4000 ) )
   allocate ( b2%next%next%next%next, source = base(20,4)( 500, null() ) )
   allocate ( b2%next%next%next%next%next, source = base(20,4)( 600, null() ) )
   
   call mynullify ( b1 )
   
   allocate ( child(20,4) :: b1 )
   b1 = b2
   
   tmp => b1
   do while ( associated ( tmp ) )
      select type ( tmp )
         type is ( base(*,4) )
            print *, tmp%i
         type is ( child(*,4) )
            print *, tmp%i, tmp%j
      end select

      tmp => tmp%next
   end do
   
   contains
   
      recursive subroutine mynullify ( a )
         class(base(:,4)), pointer, intent(inout) :: a
         
         if ( associated ( a%next ) ) call mynullify ( a%next )
         
         nullify(a)
      
      end subroutine
   

   

end program
