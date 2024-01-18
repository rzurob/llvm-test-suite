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
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: generic tb is an elemental subroutine and non-elemental non-rank one subroutine
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
      integer(4) :: i
      contains
         procedure, nopass :: elementaladdone
         procedure, nopass :: arrayaddtwo

         generic :: process => elementaladdone, arrayaddtwo

   end type

   type, extends(base) :: child
      integer(4) :: j
      contains
         procedure, nopass :: elementaladdone => elementaladdthreechild
         procedure, nopass :: arrayaddtwo => arrayaddfourchild
   end type

   contains

      elemental subroutine elementaladdone ( a )
         class(base), intent(inout) :: a

         a%i = a%i + 1

      end subroutine

      subroutine arrayaddtwo ( a )
         class(base), intent(inout) :: a(:)

         do j =1,size(a)
            a(j)%i = a(j)%i + 2
         end do

      end subroutine


      elemental subroutine elementaladdthreechild ( a )
         class(base), intent(inout) :: a

         a%i = a%i + 3

         select type ( a )
            type is ( child )
               a%j = a%j + 3

         end select

      end subroutine

      subroutine arrayaddfourchild ( a )
         class(base), intent(inout) :: a(:)

         do j =1,size(a)
            a(j)%i = a(j)%i + 4
            select type ( a )
               type is ( child )
                  a%j = a%j + 4
            end select
         end do

      end subroutine

end module

program genericGenericNameElemental001
   use m, only: base, child

   class(base), allocatable :: b0, b1(:), b2(:,:)
   class(child), pointer    :: c0, c1(:), c2(:,:)

   allocate ( b0, source = base(101) )
   allocate ( b1(4), source = (/ ( base(200+i), i = 1, 4 )/) )
   allocate ( b2(2,2), source = reshape ( source = (/ base(301), base(302), base(303), base(304) /) , shape = (/2,2/) ) )

   allocate ( c0, source = child(1001, 10001) )
   allocate ( c1(4), source = (/ ( child(2000+i,20000+i), i = 1, 4 )/) )
   allocate ( c2(2,2), source = reshape ( source = (/ child(3001,30001), child(3002, 30002), child(3003,30003), child(3004,30004) /) , shape = (/2,2/) ) )

   call b0%process(b0)
   print *, b0%i

   call b0%process(b1)
   print *, b1%i

   call b0%process(b2)
   print *, b2%i

   call b0%process(c0)
   print *, c0%i

   call b0%process(c1)
   print *, c1%i

   call b0%process(c2)
   print *, c2%i

   call c0%process(b0)
   print *, b0%i

   call c0%process(b1)
   print *, b1%i

   call c0%process(b2)
   print *, b2%i

   call c0%process(c0)
   print *, c0%i

   call c0%process(c1)
   print *, c1%i

   call c0%process(c2)
   print *, c2%i

   call b1%process(b0)
   print *, b0%i

   call b1%process(b1)
   print *, b1%i

   call b1%process(b2)
   print *, b2%i

   call b1%process(c0)
   print *, c0%i

   call b1%process(c1)
   print *, c1%i

   call b1%process(c2)
   print *, c2%i

   call c1%process(b0)
   print *, b0%i

   call c1%process(b1)
   print *, b1%i

   call c1%process(b2)
   print *, b2%i

   call c1%process(c0)
   print *, c0%i

   call c1%process(c1)
   print *, c1%i

   call c1%process(c2)
   print *, c2%i

end program



