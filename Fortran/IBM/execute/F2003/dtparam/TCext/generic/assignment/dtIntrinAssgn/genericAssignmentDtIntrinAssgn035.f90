! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/F2003/generic/assignment/dtIntrinAssgn/genericAssignmentDtIntrinAssgn035.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=none

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
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                 - polymorphic container with allocatable array components
!*
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
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
   end type

   type container(k2,n2)    ! (4,20)
      integer, kind                  :: k2
      integer, len                   :: n2
      class(base(:,k2)), allocatable :: b1(:)
   end type

   type, extends(container) :: c_container    ! (4,20)
      type(child(:,k2)), allocatable  :: c1(:,:)
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in)  :: b

         a%i = b%i + 1

         select type ( a )
            type is ( child(*,4) )
               select type ( b )
                  type is ( child(*,4) )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentDtIntrinAssgn035
   use m

   class(container(4,20)), allocatable :: c1, c2
   class(c_container(4,20)), allocatable :: c3

   allocate ( c1, c2, c3 )

   select type ( g => c1 )
      type is ( container(4,*) )
         g = container(4,20)((/ base(20,4)(1), base(20,4)(2), base(20,4)(3) /))
         print *, g%b1%i

         select type ( c2 )
            type is ( container(4,*) )
               c2 = g
               print *, c2%b1%i
         end select

   end select

   deallocate ( c1, c2 )

   allocate ( c_container(4,20) :: c2, c1 )

   select type ( c2 )
      type is ( c_container(4,*) )

         c2 = c_container(4,20)( (/ base(20,4)(4), base(20,4)(5), base(20,4)(6) /) , reshape ( source = (/ child(20,4)(7,8), child(20,4)(9,10), child(20,4)(11,12), child(20,4)(13,14)  /), shape = (/2,2/) ) )
         print *, c2%b1%i, c2%c1%i, c2%c1%j

         select type ( c1 )
            type is ( c_container(4,*) )
               c1 = c2
               print *, c1%b1%i, c1%c1%i, c1%c1%j
         end select

         select type ( c3 )
            type is ( c_container(4,*) )
               c3 = c2
               print *, c3%b1%i, c3%c1%i, c3%c1%j
         end select

   end select

   deallocate ( c1, c2 )

   allocate ( c_container(4,20) :: c2, c1 )

   select type ( c2 )
      type is ( c_container(4,*) )

         c2 = c_container(4,20)( (/ child(20,4)(100,101), child(20,4)(102,103), child(20,4)(104,105) /) , reshape ( source = (/ child(20,4)(106,107), child(20,4)(108,109), child(20,4)(110,111), child(20,4)(112,113)  /), shape = (/2,2/) ) )
         select type ( h => c2%b1 )
            type is ( child(*,4) )
               print *, h%i, h%j, c2%c1%i, c2%c1%j
         end select

         select type ( c1 )
            type is ( c_container(4,*) )
               c1 = c2
               select type ( h => c1%b1 )
                  type is ( child(*,4) )
                     print *, h%i, h%j,  c1%c1%i, c1%c1%j
               end select
         end select

         select type ( c3 )
            type is ( c_container(4,*) )
               c3 = c2
               select type ( h => c3%b1 )
                  type is ( child(*,4) )
                     print *, h%i, h%j, c3%c1%i, c3%c1%j
               end select
         end select

   end select

end program
