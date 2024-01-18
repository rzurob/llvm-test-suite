! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameElemental002.f
! opt variations: -ql -qreuse=none

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
!*  DESCRIPTION                : generic-name: generic tb is an elemental subroutine and with some overridding and some without overridding
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, nopass :: elementaladdone
         procedure, nopass :: arrayaddtwo
         generic :: process => elementaladdone, arrayaddtwo
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j
      contains
         procedure, nopass :: arrayaddtwo => arrayaddfourchild
   end type

   contains

      elemental subroutine elementaladdone ( a )
         class(base(4)), intent(inout) :: a

         a%i = a%i + 1

         select type ( a )
            type is ( child(4) )
               a%j = a%j + 1
         end select

      end subroutine

      subroutine arrayaddtwo ( a )
         class(base(4)), intent(inout) :: a(:)

         do j =1,size(a)
            a(j)%i = a(j)%i + 2
         end do

      end subroutine

      subroutine arrayaddfourchild ( a )
         class(base(4)), intent(inout) :: a(:)

         do j =1,size(a)
            a(j)%i = a(j)%i + 4
            select type ( a )
               type is ( child(4) )
                  a%j = a%j + 4
            end select
         end do

      end subroutine

end module

program genericGenericNameElemental002
   use m, only: base, child

   class(base(4)), allocatable :: b0, b1(:), b2(:,:)
   class(child(4)), pointer    :: c0, c1(:), c2(:,:)

   allocate ( b0, source = base(4)(101) )
   allocate ( b1(4), source = (/ ( base(4)(200+i), i = 1, 4 )/) )
   allocate ( b2(2,2), source = reshape ( source = (/ base(4)(301), base(4)(302), base(4)(303), base(4)(304) /) , shape = (/2,2/) ) )

   allocate ( c0, source = child(4)(1001, 10001) )
   allocate ( c1(4), source = (/ ( child(4)(2000+i,20000+i), i = 1, 4 )/) )
   allocate ( c2(2,2), source = reshape ( source = (/ child(4)(3001,30001), child(4)(3002, 30002), child(4)(3003,30003), child(4)(3004,30004) /) , shape = (/2,2/) ) )

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



