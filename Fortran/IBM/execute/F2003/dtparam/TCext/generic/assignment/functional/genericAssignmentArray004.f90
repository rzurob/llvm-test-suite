! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/generic/assignment/functional/genericAssignmentArray004.f
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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: dummy arguments of poly explicit shape array
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
      integer(k1)   :: i =0
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (4)
      integer(k1) :: j = 0
   end type

   contains

      subroutine bassgn ( a, b )
         class(base(4)), intent(inout) :: a
         class(base(4)), intent(in) :: b(5)

         print *, 'bassgn'

         a%i = b(1)%i

         do k = 2, 5
            a%i = a%i + b(k)%i
         end do

         select type ( a )
            type is ( child(4) )
               if ( same_type_as ( a, b ) ) then
               	  select type ( b )
               	     type is ( child(4) )
                        a%j = b(1)%j
                        do k = 2, 5
                           a%j = a%j + b(k)%j
                        end do
                  end select
               end if
         end select

      end subroutine

end module

program genericAssignmentArray004
   use m

   class(base(4)), allocatable :: b1, b2(:)

   class(child(4)), pointer :: c1, c2(:)

   allocate ( b1, c1, b2(5), c2(7) )

   b1%i = 100
   c1%i = 200
   c1%j = 300

   do k = 1, 5
      b2(k)%i = 10*k
   end do

   do k = 1, 7
      c2(k)%i = 1000*k
      c2(k)%j = -1000*k
   end do

   b1 = b2
   print *, b1%i

   c1 = c2
   print *, c1%i, c1%j

   b1 = c2
   print *, b1%i

   c1 = b2
   print *, c1%i, c1%j

   deallocate ( b1 )
   allocate ( child(4) :: b1 )

   b1 = c2
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

   b1 = b2
   select type ( b1 )
      type is ( child(4) )
         print *, b1%i, b1%j
   end select

end program
